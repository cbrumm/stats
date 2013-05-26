SAMPLE_SIZE = 10000
rollDice <- function(n) {
  mu = 3.5
  sigma = 1.71
  estimates = c()
  for(j in 1:SAMPLE_SIZE) {
    average = sum(sample(1:6, size=n, replace=TRUE)) / n
    estimate = (average - mu) / (sigma / sqrt(n))
    estimates = c(estimates,estimate)
  }
  return(estimates)
}
plotEstimate <- function(n) {
  plot(density(rollDice(n)), col="red")
  lines(density(rnorm(SAMPLE_SIZE)), col="blue")
}
convergencePlot <- function() {
  par(mfrow=c(2,2))
  plotEstimate(1)
  plotEstimate(2)
  plotEstimate(3)
  plotEstimate(6)
}
