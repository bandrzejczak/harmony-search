source(file="metaheuristic.r")

#quality below 0.1
termination<-function(history, model)
{
    return(model$bestPoint$quality <= 0.01)
}

#Rastrigin's function function
evaluation<-function(point)
{
  A <- 10
  score <- A * length(point)
  for (i in 1:length(point))
    score <- score + point[[i] * point[[i]] - A * cos(2 * pi * point[[i]])
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
	maxPitchChange = 0.05,
	minPitch = -5.12,
	maxPitch = 5.12,
	startPoints = generateRandomPoints(10, 5, -5.12, 5.12),
	termination = termination,
	evaluation = evaluation,
	better = better
)

metaheuristicRun(config)