source(file="../metaheuristic.r")

#quality below 0.1
termination<-function(history, model)
{
    return(model$bestPoint$quality <= 0.01 || (proc.time() - model$startTime)[[3]] > 30)
}

#squared function
evaluation<-function(point)
{
  score <- 0
  for (i in 1:length(point))
    score <- score + point[[i]] * point[[i]]
  return(score)
}

#minimizing target function
better <- function(a, b)
{
	return(a$quality < b$quality)
}

worstStartPoints <- list(
  list(1000,1000,1000,1000,1000,1000),
  list(1000,-1000,1000,-1000,1000,-1000),
  list(-1000,1000,-1000,1000,-1000,1000),
  list(1000,-1000,-1000,1000,1000,-1000),
  list(1000,1000,-1000,1000,1000,1000)
)

config <- list(
	HMCR = 0.95,
	PAR = 0.1,
	maxPitchChange = 5,
	minPitch = -1000,
	maxPitch = 1000,
	termination = termination,
	evaluation = evaluation,
	better = better,
  startPoints = worstStartPoints
)

data <- list()

for (i in 1:20)
{
  startTime <- proc.time()
  #uncomment to randomize start points
  #config$startPoints = generateRandomPoints(10, 6, -1000, 1000)
  history <- metaheuristicRun(config)
  data <- c(data, list((proc.time() - startTime)[[3]], length(history)-10, history[[length(history)]]$quality))
}

matrixData <- matrix(data, ncol=3, byrow=TRUE)
colnames(matrixData) <- list("Execution time", "Iterations number", "Final quality")
write.csv(matrixData, "experiment.csv")