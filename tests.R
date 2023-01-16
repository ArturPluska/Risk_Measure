# 16/01/2019

set.seed(1)
steps <- 120
series <- 4
dt <- 10/120
dataSeries <- changeRateFX
corMatrix <- cor(changeRateFX)
mu <- as.vector(sapply(X = dataSeries, FUN = mean))
sigma <- as.vector(sapply(X = dataSeries, FUN = sd))
sigma2 <- diag(diag(sigma) %*%  diag(sigma))
chfac <- chol(corMatrix)
densityList <- lapply(X = as.data.frame(sapply(dataSeries, scale)), density, kernel = 'epanechnikov', n = steps)
probSeries <- sapply(X = c(1:series), FUN = function(x) {densityList[[x]]$y / sum(densityList[[x]]$y)})
newSeq <- sapply(X = c(1:series), FUN = function(x) {seq(from = min(densityList[[x]]$x), to = max(densityList[[x]]$x), length.out = steps)})
drift <- mu - 0.5 * sigma2

drift[1]

#CD

set.seed(1)
randCD <- sapply(X = c(1:series), FUN = function(x) {sample(x = newSeq[, x], size = steps, replace = TRUE, prob = probSeries[, x])})
stochCD <- sqrt(dt) * randCD  %*% chfac %*% diag(sigma)

#GS

set.seed(1)
randGS <- matrix(rnorm(series * steps, 0, 1), nrow = steps, ncol = series , byrow = FALSE)
stochGS <- sqrt(dt) * randGS %*% chfac %*% diag(sigma)

#porownanie


sum(replicatedSeries)
sum(rand)

sum(stoch_1)
sum(stoch_2)



ES

