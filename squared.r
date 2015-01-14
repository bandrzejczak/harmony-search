source(file="metaheuristic.r")

#quality below 0.1
termination<-function(history, model)
{
    return(model$bestPoint$quality <= 0.01)
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

config <- list(
	HMCR = 0.95,
	PAR = 0.1,
	maxPitchChange = 5,
	minPitch = -100,
	maxPitch = 100,
	startPoints = generateRandomPoints(10, 2, -100, 100),
	termination = termination,
	evaluation = evaluation,
	better = better
)

metaheuristicRun(config)