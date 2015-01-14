source(file="metaheuristic.r")

termination<-function(history, model)
{
  ???
}

evaluation<-function(point)
{
  ???
}

better <- function(a, b)
{
  ???
}

config <- list(
	HMCR = ???,
	PAR = ???,
	maxPitchChange = ???,
	minPitch = ???,
	maxPitch = ???,
	startPoints = ???,
	termination = termination,
	evaluation = evaluation,
	better = better
)

metaheuristicRun(config)