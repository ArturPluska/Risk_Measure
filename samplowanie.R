ratesFX <- readRDS('ratesFX.RDS')
EUR <- RateReturn(ratesFX$EUR)

hist(EUR, breaks = 25, xlim = c(round(min(EUR), digits = 1), round(max(EUR), digits = 1)))
lines(density(EUR, kernel = 'epanechnikov'), col = 'blue')

densityEUR <- density(changeRateFX$EUR, kernel = 'epanechnikov', n = length(EUR))
probEUR <- densityEUR$y/sum(densityEUR$y)

changeEUR <- seq(from = min(densityEUR$x), to = max(densityEUR$x), length.out = length(EUR))
replicatedSample <- sample(EUR, size = 100, replace = FALSE, prob = probEUR)

simulation <- cumprod(1 + replicatedSample)
plot(4.28 * simulation, type = 'l')

hist(replicatedSample, breaks = 25, xlim = c(-0.1, 0.1), ylim = c(0, 20))
lines(density(replicatedSample, kernel = 'epanechnikov'), col = 'red')
lines(density(EUR, kernel = 'epanechnikov'), col = 'blue')