source(file="../metaheuristic.r")
require(graphics)
#quality below 0.1
termination<-function(history, model)
{
    return(model$bestPoint$quality <= 0.0001)
}

#Rastrigin's function function
evaluation<-function(point)
{
  A <- 10
  score <- A * length(point)
  for (i in 1:length(point))
    score <- score + point[[i]] * point[[i]] - A * cos(2 * pi * point[[i]])
  return(score)
}

#minimizing target function
better <- function(a, b)
{
	return(a$quality < b$quality)
}

worstStartPoints <- list(
  list(4.6,4.6),
  list(-4.6,-4.6),
  list(-4.6,4.6),
  list(4.6,-4.6),
  list(4.6,4.6),
  list(-4.6,-4.6),
  list(-4.6,4.6),
  list(4.6,-4.6),
  list(-4.6,4.6),
  list(4.6,-4.6)
)

config <- list(
	HMCR = 0.95,
	PAR = 0.1,
	maxPitchChange = 0.5,
	minPitch = -5.12,
	maxPitch = 5.12,
	startPoints = generateRandomPoints(10, 3, -5.12, 5.12),
  #startPoints = worstStartPoints,
	termination = termination,
	evaluation = evaluation,
	better = better,
  finalPointColor = '#FF69B4'
)

#translates history to matrix of coordinates and colors based on qualities
get2DPlottableData <- function(history)
{
  rbPal <- colorRampPalette(c('blue', 'light blue', 'yellow','red'))
  plottable <- list(x = numeric(), y = numeric())
  qualities <- numeric()
  for (i in 1:length(history))
  {
    plottable$x <- c(plottable$x, history[[i]]$coordinates[[1]])
    plottable$y <- c(plottable$y, history[[i]]$coordinates[[2]])
    qualities <- c(qualities, history[[i]]$quality)
  }
  plottable$Color <- rbPal(512)[as.numeric(cut(qualities,breaks = 512))]
  plottable$Color[length(history)] <- config$finalPointColor
  return(plottable)
}

data <- list()

for (i in 1:20)
{
  startTime <- proc.time()
  history <- metaheuristicRun(config)
  data <- c(data, list((proc.time() - startTime)[[3]], length(history)-10, history[[length(history)]]$quality))
}

#plottable <- get2DPlottableData(history)
#plot(plottable$x, plottable$y, pch = 20, col = plottable$Color)

matrixData <- matrix(data, ncol=3, byrow=TRUE)
colnames(matrixData) <- list("Execution time", "Iterations number", "Final quality")
write.csv(matrixData, "experiment3.csv")
