source(file="utils.r")

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun <- function(config)
{
   history <- init(config$startPoints)
   history <- evaluateList(history, config$evaluation)
   model <- initModel(history, config)
   while (!config$termination(history, model))
   {
      aa <- aggregatedOperator(history, model, config)
      history <- historyPush(history, aa$newPoints)
      model <- aa$newModel
   }
   return(history)
}

#history initialization
init<-function(startPoints)
{
  initializedHistory<-list()
  for(i in 1:length(startPoints))
  {
    newPoint<-list(coordinates=startPoints[[i]])
    initializedHistory<-c(initializedHistory,list(newPoint))
  }
  return(initializedHistory)
}


#model initialization
initModel<-function(history, config)
{
  initializedModel <- list(
   HMCR=config$HMCR,
   PAR=config$PAR,
   minPitch=config$minPitch,
   maxPitch=config$maxPitch,
   maxPitchChange=config$maxPitchChange
  )
  initializedModel$bestHarmonies <- history
  initializedModel$bestPoint <- getBest(history, config$better)
  initializedModel$worstPoint <- getWorst(history, config$better)
  return(initializedModel)
}

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel, config)
{
  selectedPoints<-selection(history, oldModel)
  newModel<-modelUpdate(selectedPoints, oldModel, config)
  return (
    list(
      newPoints=evaluateList(list(selectedPoints), config$evaluation),
      newModel=newModel
    )
  )
}

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
  #create new, empty harmony from parts of
  # piches of instruments of best harmonies, or just at random
  newHarmony <- list(coordinates=list())  
  for (i in 1:length(model$bestPoint$coordinates)) #for all instruments
  {   
   newPitch <- adjustPitch(createPitch(model, i), model)
   newHarmony$coordinates <- c(newHarmony$coordinates, newPitch) 
  }
  return(newHarmony)
}

createPitch<-function(model, instrumentNumber) 
{
  #use memory
  if(runif(1, 0, 1) < model$HMCR)
  {
    randomHarmony <- randomElement(model$bestHarmonies)
    newPitch <- randomHarmony$coordinates[[instrumentNumber]]
  }
  #use random value
  else 
  {
    newPitch <- runif(1,model$minPitch, model$maxPitch)
  }
  return(newPitch)
}

adjustPitch<-function(newPitch, model)
{
  #adjust the pitch
  if(runif(1,0,1) < model$PAR) 
  {
    #pitch down
    if(runif(1,0,1) < 0.5) 
    {
      newPitch <- newPitch - runif(1, 0, model$maxPitchChange)
    }
    #pitch up
    else
    {
      newPitch <- newPitch + runif(1, 0, model$maxPitchChange)
    }
    if(newPitch > model$maxPitch) newPitch <- model$maxPitch
    if(newPitch < model$minPitch) newPitch <- model$minPitch
  }
  return(newPitch)
}

#take a look at the list of selectedPoints and 
#on the current state of the model, update it 
#and then return
modelUpdate<-function(newPoint, oldModel, config)
{
   newModel <- oldModel
   newPoint$quality <- config$evaluation(newPoint$coordinates)
   if(newPoint$quality < oldModel$worstPoint$quality)
   {
     newModel$bestHarmonies <- c(withoutWorst(oldModel$bestHarmonies, config$better), list(newPoint))
     newModel$bestPoint <- getBest(newModel$bestHarmonies, config$better)
     newModel$worstPoint <- getWorst(newModel$bestHarmonies, config$better)
   }
   return (newModel)
}

