

# wczytanie poprzednich danych

load('wisla_obliczenia_20181115_1345.RData')

# wczytanie danych

ratesFX <- read.csv('C:/Users/Artur/Desktop/RiskMeasuresResearch/ratesFX_Monthly.csv')[, c(2, 3, 4, 5)] # kursy walutowe
foreignDebt <- read.csv('C:/Users/Artur/Desktop/RiskMeasuresResearch/foreignDebtPL_Monthly.csv')[, c(2, 3, 4, 5)] # zadluzenie w FX
interestRateFX <- read.csv('C:/Users/Artur/Desktop/RiskMeasuresResearch/interestRateFX_Monthly.csv')[, c(2, 3, 4, 5)] # oprocentowanie

head(ratesFX); head(foreignDebt); head(interestRateFX) # poczatkowe wartosci
tail(ratesFX); tail(foreignDebt); tail(interestRateFX) # koncowe wartosci

cor(ratesFX) # macierz korelacji

# przeksztalcenie kursow w stopy zwrotu

changeRateFX <- RateReturn(as.matrix(ratesFX))

head(changeRateFX) # pierwsze obserwacje
tail(changeRateFX) # koncowe obserwacje

cor(changeRateFX) # macierz korelacji

# inspekcja danyh

summary(changeRateFX)

par(mfrow = c(2,2))
for(i in 1:4) {
  hist(changeRateFX[, i], breaks = 20, main = paste("Histogram stop zwrotu ", colnames(changeRateFX)[i]), xlab = "Stopa zwrotu", ylab = "", xlim = c(-0.2, 0.2))
}


# Miary ryzyka osiagniete na podstawie danych historycznych

rm(zbior)


CVaR(rend$x, method = 'gaussian')
ValueAtRisk(rend$x, method = 'historical')
ValueAtRisk(rend$x, method = 'gaussian')
ExpectedShortfall(rend$x, method = 'historical')
ExpectedShortfall(rend$x, method = 'gaussian')


# Funkcje do symulacji kursu walutowego

changeReturnFX_N1 <- function(randomVariable, returnFXSeries, rFree, steps, horison) {
  u <- exp(sd(returnFXSeries) * sqrt(horison / steps))
  d <- 1 / u
  p <- (exp(rFree * (horison / steps)) - d) / (u - d)
  if(randomVariable > p) {
    return(u)
  } else {
    return(d) 
  }
}




# Funkcja do przeprowadzenia symylacji

MonteCarloSimulationPortfolio <- function(funtionReturn, returnFXSeries, rFree, simulationNumber, steps, horison) {
  monteCarloScenarios <- matrix(NA, nrow = steps, ncol = simulationNumber)
  for(i in 1:simulationNumber) {
    monteCarloScenarios[, i] <- unlist(lapply(runif(steps), funtionReturn, returnFXSeries, rFree, steps , horison))
  }
  return(monteCarloScenarios)
}

# Symulacje

EUR <- changeRateFX[, 1]
USD <- changeRateFX[, 2]
CHF <- changeRateFX[, 3]
JPY <- changeRateFX[, 4]

rFree_EUR <- 0.005
rFree_USD <- 0.02
rFree_CHF <- 0.005
rFree_JPY <- 0.004

noSteps <- 520
noSimulaiton <- 1000
noHorison <- 10

simulationEUR <- as.data.table(MonteCarloSimulationPortfolio(changeReturnFX_N1, returnFXSeries = EUR, rFree = rFree_EUR, simulationNumber = noSimulaiton, steps = noSteps, horison = noHorison))
simulationUSD <- as.data.table(MonteCarloSimulationPortfolio(changeReturnFX_N1, returnFXSeries = USD, rFree = rFree_USD, simulationNumber = noSimulaiton, steps = noSteps, horison = noHorison))
simulationCHF <- as.data.table(MonteCarloSimulationPortfolio(changeReturnFX_N1, returnFXSeries = CHF, rFree = rFree_CHF, simulationNumber = noSimulaiton, steps = noSteps, horison = noHorison))
simulationJPY <- as.data.table(MonteCarloSimulationPortfolio(changeReturnFX_N1, returnFXSeries = JPY, rFree = rFree_JPY, simulationNumber = noSimulaiton, steps = noSteps, horison = noHorison))


####EUR
predictChangeFX_EUR <- matrix(NA, nrow = noSteps, ncol = 1000)
for(i in 1:noSteps) {
  for(j in 1:1000) {
    predictChangeFX_EUR[i, j] <- prod(simulationEUR[[j]][c(1:i)])
  }
}

####USD
predictChangeFX_USD <- matrix(NA, nrow = 120, ncol = 1000)
for(i in 1:120) {
  for(j in 1:1000) {
    predictChangeFX_USD[i, j] <- prod(simulationUSD[[j]][c(1:i)])
  }
}

####CHF
predictChangeFX_CHF <- matrix(NA, nrow = 120, ncol = 1000)
for(i in 1:120) {
  for(j in 1:1000) {
    predictChangeFX_CHF[i, j] <- prod(simulationCHF[[j]][c(1:i)])
  }
}

####JPY
predictChangeFX_JPY <- matrix(NA, nrow = 120, ncol = 1000)
for(i in 1:120) {
  for(j in 1:1000) {
    predictChangeFX_JPY[i, j] <- prod(simulationJPY[[j]][c(1:i)])
  }
}

predictRate_EUR <- predictChangeFX_EUR * 4.28
predictRate_USD <- predictChangeFX_USD * 3.68
predictRate_JPY <- predictChangeFX_JPY * 3.77
predictRate_CHF <- predictChangeFX_CHF * 3.24

par(mfrow = c(1, 1))
plot(predictRate_EUR[, 1], type = 'l', ylim = c(3, 6))
for(i in 1:1000) {
  lines(predictRate_EUR[, i], type = 'l', col = 1 + i)
}

VaR_EUR <- ValueAtRisk(predictChangeFX_EUR[noSteps, ], method = 'historical', horizon = 10, prob = 0.05) # VaR 1.075

VaR_EUR * 4.28 # 4.60
debtFX[178,'EUR']  - VaR_EUR * last(debtFX[178,'EUR']) # strata -10 mld PLN na walucie EUR

(costFX[175, 'EUR'] * VaR_EUR + costFX[169, 'EUR'] * VaR_EUR) - (costFX[175, 'EUR'] + costFX[169, 'EUR']) # strata 255 mln PLN

(costFX[175, 'EUR'] * VaR_EUR + costFX[169, 'EUR'] * VaR_EUR) / mean(sum(debtFX[169, ]), sum(debtFX[175, ]))
(costFX[175, 'EUR'] + costFX[169, 'EUR']) / mean(sum(debtFX[169, ]), sum(debtFX[175, ]))

save.image('wisla_obliczenia_20181115_1345.RData')
