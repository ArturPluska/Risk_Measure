
# wczytanie danych

ratesFX <- readRDS('ratesFX.RDS') # kursy walutowe
foreignDebt <- readRDS('foreignDebt.RDS') # zadluzenie w FX
interestRateFX <- readRDS('interestRateFX.RDS') # oprocentowanie

head(ratesFX); head(foreignDebt); head(interestRateFX) # poczatkowe wartosci
tail(ratesFX); tail(foreignDebt); tail(interestRateFX) # koncowe wartosci
summary(ratesFX)

cor(ratesFX) # macierz korelacji

# przeksztalcenie kursow w stopy zwrotu

changeRateFX <- RateReturn(ratesFX)

head(changeRateFX) # pierwsze obserwacje
tail(changeRateFX) # koncowe obserwacje

cor(changeRateFX) # macierz korelacji

# inspekcja danyh

summary(changeRateFX)

par(mfrow = c(2,2))
for(i in 1:4) {
  hist(changeRateFX[, i], breaks = 20, main = paste("Histogram stop zwrotu ", colnames(changeRateFX)[i]), xlab = "Stopa zwrotu", ylab = "", xlim = c(-0.2, 0.2))
}

# miary ryzyka osiagniete na podstawie danych historycznych

changeRateFX_List <- MatrixOrFrameToList(changeRateFX) # przeksztalcenie danych z macierzy do listy

samiVar_Output <- lapply(changeRateFX_List, SemiVariance) # Semi-Variance
lpm_Output <- lapply(changeRateFX_List, LowerPartialMoments, targetReturn = 0.05, direction = 'upper', method = 'subset', riskAversion = 3) # Lower Partial Moments
VaR_Output_Hist <- lapply(changeRateFX_List, ValueAtRisk) # Value-at-Risk (historical)
VaR_Output_Gaus <- lapply(changeRateFX_List, ValueAtRisk, method = 'gaussian') # Value-at-Risk (gaussian)
CVaR_Output_Hist <- lapply(changeRateFX_List, ValueAtRisk) # Value-at-Risk (historical)
CVar_Output_Gaus <- lapply(changeRateFX_List, ValueAtRisk, method = 'gaussian') # Value-at-Risk (gaussian)

# symulacje Monte Carlo GS nieskorelowane

simulatedFX_GS <- list()

simulatedFX_GS$EUR <- MonteCarloSimulation(GeometricBrownianMotionGS, 
                                           parametersVector = list(startPrice = last(ratesFX$EUR), 
                                                                   dataSeries = changeRateFX$EUR,
                                                                   steps = 120, horizon = 10),
                                           simulationNumber = 10000)

simulatedFX_GS$USD <- MonteCarloSimulation(GeometricBrownianMotionGS, 
                                           parametersVector = list(startPrice = last(ratesFX$USD),
                                                                   dataSeries = changeRateFX$USD,
                                                                   steps = 120, horizon = 10),
                                           simulationNumber = 10000)

simulatedFX_GS$CHF <- MonteCarloSimulation(GeometricBrownianMotionGS, 
                                           parametersVector = list(startPrice = last(ratesFX$CHF),
                                                                   dataSeries = changeRateFX$CHF,
                                                                   steps = 120, horizon = 10),
                                           simulationNumber = 10000)

simulatedFX_GS$JPY <- MonteCarloSimulation(GeometricBrownianMotionGS, 
                                           parametersVector = list(startPrice = last(ratesFX$JPY),
                                                                   dataSeries = changeRateFX$JPY,
                                                                   steps = 120, horizon = 10),
                                           simulationNumber = 10000)

str(simulatedFX_GS)

# symulacje Monte Carlo CD nieskorelowane

simulatedFX_CD <- list()

simulatedFX_CD$EUR <- MonteCarloSimulation(GeometricBrownianMotionCD,
                                           parametersVector = list(startPrice = last(ratesFX$EUR),
                                                                   dataSeries = changeRateFX$EUR,
                                                                   steps = 120,
                                                                   horizon = 10),
                                           simulationNumber = 10000)

simulatedFX_CD$USD <- MonteCarloSimulation(GeometricBrownianMotionCD,
                                           parametersVector = list(startPrice = last(ratesFX$USD),
                                                                   dataSeries = changeRateFX$USD,
                                                                   steps = 120, horizon = 10),
                                           simulationNumber = 10000)

simulatedFX_CD$CHF <- MonteCarloSimulation(GeometricBrownianMotionCD, 
                                           parametersVector = list(startPrice = last(ratesFX$CHF),
                                                                   dataSeries = changeRateFX$CHF,
                                                                   steps = 120,
                                                                   horizon = 10),
                                           simulationNumber = 10000)

simulatedFX_CD$JPY <- MonteCarloSimulation(GeometricBrownianMotionCD, 
                                           parametersVector = list(startPrice = last(ratesFX$JPY),
                                                                   dataSeries = changeRateFX$JPY,
                                                                   steps = 120,
                                                                   horizon = 10),
                                           simulationNumber = 10000)

str(simulatedFX_CD)

# Symulacje Monte Carlo GS skorelowane

simcorFX_GS <- MonteCarloPortfolioSimulation(simulationModel = GeometricBrownianMotionGSCor,
                                          parametersVector = list(startPrice = c(last(ratesFX$EUR), last(ratesFX$USD), last(ratesFX$CHF), last(ratesFX$JPY)),
                                                                  dataSeries = changeRateFX,
                                                                  corMatrix = cor(changeRateFX),
                                                                  steps = 120,
                                                                  horizon = 10),
                                          simulationNumber = 10000)

cor(cbind(simcorFX_GS$EUR$S1, simcorFX_GS$USD$S1, simcorFX_GS$CHF$S1, simcorFX_GS$JPY$S1))

# Symulacje Monte Carlo CD skorelowane

simcorFX_CD <- MonteCarloPortfolioSimulation(simulationModel = GeometricBrownianMotionCDCor,
                                             parametersVector = list(startPrice = c(last(ratesFX$EUR), last(ratesFX$USD), last(ratesFX$CHF), last(ratesFX$JPY)),
                                                                     dataSeries = changeRateFX,
                                                                     corMatrix = cor(changeRateFX),
                                                                     steps = 120,
                                                                     horizon = 10),
                                             simulationNumber = 10000)

cor(cbind(simcorFX_CD$EUR$S1, simcorFX_CD$USD$S1, simcorFX_CD$CHF$S1, simcorFX_CD$JPY$S1))

### EUR ###

# Gaussian
par(mfrow = c(1, 1))
plot(simcorFX_GS$EUR[, 1], type = 'l', ylim = c(min(simcorFX_GS$EUR), max(simcorFX_GS$EUR)), main = 'Symulacja EUR (GBM GS)', xlab = 'Czas', ylab = 'EUR')
for(i in 1:1000) {
  lines(simcorFX_GS$EUR[, i], type = 'l', col = 1 + i)
}
min(simcorFX_GS$EUR); max(simcorFX_GS$EUR)
hist(changeRateFX$EUR)
hist(RateReturn(simcorFX_GS$EUR$S1))

# Custom
par(mfrow = c(1, 1))
plot(simcorFX_CD$EUR[, 1], type = 'l', ylim = c(min(simcorFX_CD$EUR), max(simcorFX_CD$EUR)), main = 'Symulacja EUR (GBM CD)', xlab = 'Czas', ylab = 'EUR')
for(i in 1:1000) {
  lines(simcorFX_CD$EUR[, i], type = 'l', col = 1 + i)
}
min(simcorFX_CD$EUR); max(simcorFX_CD$EUR)
hist(changeRateFX$EUR)
hist(RateReturn(simcorFX_CD$EUR$S1))

### USD ###

# Gaussian
par(mfrow = c(1, 1))
plot(simcorFX_GS$USD[, 1], type = 'l', ylim = c(min(simcorFX_GS$USD), max(simcorFX_GS$USD)), main = 'Symulacja USD (GBM GS)', xlab = 'Czas', ylab = 'USD')
for(i in 1:1000) {
  lines(simcorFX_GS$USD[, i], type = 'l', col = 1 + i)
}
min(simcorFX_GS$USD); max(simcorFX_GS$USD)
hist(changeRateFX$USD)
hist(RateReturn(simcorFX_GS$USD$S1))

# Custom
par(mfrow = c(1, 1))
plot(simcorFX_CD$USD[, 1], type = 'l', ylim = c(min(simcorFX_CD$USD), max(simcorFX_CD$USD)), main = 'Symulacja USD (GBM CD)', xlab = 'Czas', ylab = 'USD')
for(i in 1:1000) {
  lines(simcorFX_CD$USD[, i], type = 'l', col = 1 + i)
}
min(simcorFX_CD$USD); max(simcorFX_CD$USD)
hist(changeRateFX$USD)
hist(RateReturn(simcorFX_CD$USD$S1))


############################################################## proba 1

wghts <- rep(0.25, 4)
dbtValue <- 50000
dt <- DebtScenarios(scenariosFX = simcorFX_GS, portfolioWeights = wghts, debtValue = dbtValue)
agr <- AggregateScenarios(dt)

simRetPort_1 <- as.numeric(last(agr)) / as.numeric(first(agr))-1
mean(simRetPort_1)

RetRsk_SemVar_1 <- mean(simRetPort_1)^2/(SemiVariance(simRetPort_1, targetReturn = mean(simRetPort_1), direction = 'upper', method = 'subset'))
RetRsk_LPM_1 <- mean(simRetPort_1)^2/(LowerPartialMoments(simRetPort_1, targetReturn = mean(simRetPort_1), direction = 'upper'))
RetRsk_VaR_1 <- mean(simRetPort_1)^2/as.numeric(ValueAtRisk(simRetPort_1, prob = 0.05))
RetRsk_ES_1 <- mean(simRetPort_1)^2/as.numeric(ExpectedShortfall(simRetPort_1, prob = 0.05))

############################################################# proba 2

wghts <- c(0.50, 0.3, 0.1, 0.1)
dbtValue <- 50000
dt_2 <- DebtScenarios(scenariosFX = simcorFX_GS, portfolioWeights = wghts, debtValue = dbtValue)
agr_2 <- AggregateScenarios(dt_2)

simRetPort_2 <- as.numeric(last(agr_2)) / as.numeric(first(agr_2))-1
mean(simRetPort_2)

RetRsk_SemVar_2 <- mean(simRetPort_2)^2/(SemiVariance(simRetPort_2, targetReturn = mean(simRetPort_2), direction = 'upper', method = 'subset'))
RetRsk_LPM_2 <- mean(simRetPort_2)^2/(LowerPartialMoments(simRetPort_2, targetReturn = mean(simRetPort_2), direction = 'upper'))
RetRsk_VaR_2 <- mean(simRetPort_2)^2/as.numeric(ValueAtRisk(simRetPort_2, prob = 0.05))
RetRsk_ES_2 <- mean(simRetPort_2)^2/as.numeric(ExpectedShortfall(simRetPort_2, prob = 0.05))

############################################################# proba 3

wghts <- c(1, 0, 0, 0)
dbtValue <- 50000
dt_3 <- DebtScenarios(scenariosFX = simcorFX_GS, portfolioWeights = wghts, debtValue = dbtValue)
agr_3 <- AggregateScenarios(dt_3)

simRetPort_3 <- as.numeric(last(agr_3)) / as.numeric(first(agr_3))-1
mean(simRetPort_3)

RetRsk_SemVar_3 <- mean(simRetPort_3)^2/(SemiVariance(simRetPort_3, targetReturn = mean(simRetPort_3), direction = 'upper', method = 'subset'))
RetRsk_LPM_3 <- mean(simRetPort_3)^2/(LowerPartialMoments(simRetPort_3, targetReturn = mean(simRetPort_3), direction = 'upper'))
RetRsk_VaR_3 <- mean(simRetPort_3)^2/as.numeric(ValueAtRisk(simRetPort_3, prob = 0.05))
RetRsk_ES_3 <- mean(simRetPort_3)^2/as.numeric(ExpectedShortfall(simRetPort_3, prob = 0.05))

############################################################# proba 4

wghts <- c(0, 1, 0, 0)
dbtValue <- 50000
dt_4 <- DebtScenarios(scenariosFX = simcorFX_GS, portfolioWeights = wghts, debtValue = dbtValue)
agr_4 <- AggregateScenarios(dt_4)

simRetPort_4 <- as.numeric(last(agr_4)) / as.numeric(first(agr_4))-1
mean(simRetPort_4)

RetRsk_SemVar_4 <- mean(simRetPort_4)^2/(SemiVariance(simRetPort_4, targetReturn = mean(simRetPort_4), direction = 'upper', method = 'subset'))
RetRsk_LPM_4 <- mean(simRetPort_4)^2/(LowerPartialMoments(simRetPort_4, targetReturn = mean(simRetPort_4), direction = 'upper'))
RetRsk_VaR_4 <- mean(simRetPort_4)^2/as.numeric(ValueAtRisk(simRetPort_4, prob = 0.05))
RetRsk_ES_4 <- mean(simRetPort_4)^2/as.numeric(ExpectedShortfall(simRetPort_4, prob = 0.05))

############################################################# proba 5

wghts <- c(0.83, 0.17, 0, 0)
dbtValue <- 50000
dt_5 <- DebtScenarios(scenariosFX = simcorFX_GS, portfolioWeights = wghts, debtValue = dbtValue)
agr_5 <- AggregateScenarios(dt_5)

simRetPort_5 <- as.numeric(last(agr_5)) / as.numeric(first(agr_5))-1
mean(simRetPort_5)

RetRsk_SemVar_5 <- mean(simRetPort_5)^2/(SemiVariance(simRetPort_5, targetReturn = mean(simRetPort_5), direction = 'upper', method = 'subset'))
RetRsk_LPM_5 <- mean(simRetPort_5)^2/(LowerPartialMoments(simRetPort_5, targetReturn = mean(simRetPort_5), direction = 'upper'))
RetRsk_VaR_5 <- mean(simRetPort_5)^2/as.numeric(ValueAtRisk(simRetPort_5, prob = 0.05))
RetRsk_ES_5 <- mean(simRetPort_5)^2/as.numeric(ExpectedShortfall(simRetPort_5, prob = 0.05))


############################################################# proba 6

wghts <- c(0.83, 0, 0.17, 0)
dbtValue <- 50000
dt_6 <- DebtScenarios(scenariosFX = simcorFX_GS, portfolioWeights = wghts, debtValue = dbtValue)
agr_6 <- AggregateScenarios(dt_6)

simRetPort_6 <- as.numeric(last(agr_6)) / as.numeric(first(agr_6))-1
mean(simRetPort_6)

RetRsk_SemVar_6 <- mean(simRetPort_6)^2/(SemiVariance(simRetPort_6, targetReturn = mean(simRetPort_6), direction = 'upper', method = 'subset'))
RetRsk_LPM_6 <- mean(simRetPort_6)^2/(LowerPartialMoments(simRetPort_6, targetReturn = mean(simRetPort_6), direction = 'upper'))
RetRsk_VaR_6 <- mean(simRetPort_6)^2/as.numeric(ValueAtRisk(simRetPort_6, prob = 0.05))
RetRsk_ES_6 <- mean(simRetPort_6)^2/as.numeric(ExpectedShortfall(simRetPort_6, prob = 0.05))


save.image('wisla_obliczenia_20190116_v1.RData')