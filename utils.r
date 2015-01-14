generateRandomPoints<-function(length, instrumentsNumber, minPitch, maxPitch)
{
  randomPoints<-list()
  for(i in 1:length)
    randomPoints<-c(randomPoints,list(runif(instrumentsNumber, minPitch, maxPitch)))
  return(randomPoints)
}

evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points))
     points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  return (points)
}

getBest<-function(points, better) getSpecificPoint(points, better)$value
getWorst<-function(points, better) getSpecificPoint(points, function(a, b) !better(a, b))$value

getSpecificPoint<-function(points, characteristic)
{
  chosenPoint <- list(value=points[[1]], index=1)
  for (i in 2:length(points)) {
    if(characteristic(points[[i]],chosenPoint$value)){
      chosenPoint$value <- points[[i]]
      chosenPoint$index <- i
    }
  }
  return(chosenPoint)
}

withoutWorst<-function(points, better) {
  points[[getSpecificPoint(points, function(a,b) !better(a, b))$index]] <- NULL
  return(points)
}

randomElement<-function(list){
	return(list[[round(runif(1, 1, length(list)))]])
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