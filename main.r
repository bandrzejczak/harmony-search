#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################

#history initialization
init<-function(startPoints)
{
  initializedHistory<-list()
  for(i in 1:length(startPoints))
  {
    newPoint<-list(coordinates=startPoints[[i]])
    initializedHistory<-c(initializedHistory,list(newPoint))
  }
  #return random harmonics for initialization purposes
  return(initializedHistory)
}

generateRandomPoints<-function(length, instrumentsNumber, minPitch, maxPitch)
{
  randomPoints<-list()
  for(i in 1:length)
    randomPoints<-c(randomPoints,list(runif(instrumentsNumber, minPitch, maxPitch)))
  #return random harmonics for initialization purposes
  return(randomPoints)
}

#model initialization
initModel<-function(history)
{
  initializedModel <- list(HMCR=0.95, PAR=0.1, targetQuality=0.8, maxPitch=5.12, minPitch=-5.12, instrumentsNumber=2, historyMemorySize=10)
  initializedModel$bestHarmonies <- history
  initializedModel$bestPoint <- getBest(history)
  initializedModel$worstPoint <- getWorst(history)
  for (i in 2:length(history)) 
  {
    actualQuality <- history[[i]]$quality
    if(actualQuality > initializedModel$bestPoint$quality) {
      initializedModel$bestPoint <- history[[i]]
    } else if(actualQuality < initializedModel$worstPoint$quality) {
      initializedModel$worstPoint <- history[[i]]
    }
  }
  return(initializedModel)
}

getBest<-function(points) getSpecificPoint(points, function(actual, chosen) actual>chosen)$value
getWorst<-function(points) getSpecificPoint(points, function(actual, chosen) actual<chosen)$value

getSpecificPoint<-function(points, characteristic)
{
  chosenPoint <- list(value=points[[1]], index=1)
  for (i in 2:length(points)) {
    if(characteristic(points[[i]]$quality,chosenPoint$value$quality)){
      chosenPoint$value <- points[[i]]
      chosenPoint$index <- i
    }
  }
  return(chosenPoint)
}

withoutWorst<-function(points) {
  points[[getSpecificPoint(points, function(a,b) a<b)$index]] <- NULL
  return(points)
}


#function checking algorithm termination conditions
termination<-function(history,model)
{
    return(model$targetQuality >= model$bestPoint$quality)
}

#function evaluation point usefulness for the algorithm
#evaluation<-function(point)
#{
#  A <- 10
#  score <- A * length(point)
#  for (i in 1:length(point))
#    score <- score + point[[i] * point[[i]] - A * cos(2 * pi * point[[i]])
#  return(score)
#}

evaluation<-function(point)
{
  score <- 0
  for (i in 1:length(point))
    score <- score + point[[i]] * point[[i]]
  return(score)
}


#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
  #create new, empty harmony
  newHarmony <- list(coordinates=list())  
  for (i in 1:model$instrumentsNumber) 
  {   
   newPitch <- adjustPitch(createPitch(model, i), model)
   newHarmony$coordinates <- c(newHarmony$coordinates, newPitch) 
  }
   return(newHarmony)
}

createPitch<-function(model, i) 
{
  if(runif(1, 0, 1) < model$HMCR) {
    #use memory to create harmony
    #get random harmony
    randomHarmony <- model$bestHarmonies[[round(runif(1, 1, model$historyMemorySize))]]
    newPitch <- randomHarmony$coordinates[[i]]
  } else 
  {
    #append random value
    newPitch <- runif(1,model$minPitch, model$maxPitch)
  }
  return(newPitch)
}

adjustPitch<-function(newPitch, model)
{
  #adjust the pitch
  if(runif(1,0,1) < model$PAR) 
  {
    if(runif(1,0,1) < 0.5) #pitch down
    {
      newPitch <- newPitch + runif(1, model$minPitch - newPitch, 0)
    } else #pitch up
    {
      newPitch <- newPitch + runif(1, 0, model$maxPitch - newPitch)
    }
  }
  return(newPitch)
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(newPoint, oldModel, evaluate)
{
   #take a look at the list of selectedPoints and 
   #on the current state of the model, update it 
   #and then return
   newModel <- oldModel
   newPoint$quality <- evaluate(newPoint$coordinates)
   if(newPoint$quality > oldModel$worstPoint$quality) 
   {
     newModel$bestHarmonies <- c(withoutWorst(oldModel$bestHarmonies), list(newPoint))
     newModel$bestPoint <- getBest(newModel$bestHarmonies)
     newModel$worstPoint <- getWorst(newModel$bestHarmonies)
   }
   return (newModel)
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel, evaluate)
{

   selectedPoints<-selection(history, oldModel)
   newModel<-modelUpdate(selectedPoints, oldModel, evaluate)
   return (list(newPoints=selectedPoints,newModel=newModel))
}

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
   history<-initialization(startPoints)
   history<-evaluateList(history, evaluation)
   model<-initModel(history)
   while (!termination(history,model))
   {
      aa<-aggregatedOperator(history, model, evaluation)
      #aa$newPoints<-evaluateList(aa$newPoints, evaluation)
      history<-historyPush(history,aa$newPoints)
      model<-aa$newModel
   }
   return(history)
}

#push a LIST of points into the history
historyPush<-function(oldHistory, newPoints)
{
   newHistory<-c(oldHistory,newPoints)
   return (newHistory)
}
#read a LIST of points pushed recently into the history
historyPop<-function(history, number)
{
   stop=length(history)
   start=max(stop-number+1,1)
   return(history[start:stop])
}

#evaluate a LIST of points
evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points))
     points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  return (points) 
}


####  THAT'S ALL FOLKS

#Run me:
metaheuristicRun(init, generateRandomPoints(10, 2, -10, 10), termination, evaluation)
