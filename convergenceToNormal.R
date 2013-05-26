SAMPLE_SIZE = 10000
NORMAL = density(rnorm(1000000))
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
plotRollDiceEstimate <- function(n) {
  plot(density(rollDice(n)), col="red")
  lines(NORMAL, col="blue")
}
rollDiceConvergencePlot <- function() {
  par(mfrow=c(2,2))
  plotRollDiceEstimate(1)
  plotRollDiceEstimate(2)
  plotRollDiceEstimate(3)
  plotRollDiceEstimate(6)
}
flipCoin <- function(n) {
  mu = 0.7
  error = sqrt(mu * (1 - mu) / n)
  estimates = c()
  for(j in 1:SAMPLE_SIZE) {
    average = sum(sample(0:1, size=n, replace=TRUE)) / n
    estimate = (average - mu) / error
    estimates = c(estimates,estimate)
  }
  return(estimates)
}
plotFlipcoinEstimate <- function(n) {
  plot(density(flipCoin(n)), col="red")
  lines(NORMAL, col="blue")
}
flipCoinConvergencePlot <- function() {
  par(mfrow=c(2,2))
  plotFlipcoinEstimate(1)
  plotFlipcoinEstimate(2)
  plotFlipcoinEstimate(10)
  plotRollDiceEstimate(20)
}
