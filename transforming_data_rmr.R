# wczytanie danych

ratesFX <- read.csv('ratesFX_Monthly.csv')[, c(2, 3, 4, 5)] # kursy walutowe
foreignDebt <- read.csv('foreignDebtPL_Monthly.csv')[, c(2, 3, 4, 5)] # zadluzenie w FX
interestRateFX <- read.csv('interestRateFX_Monthly.csv')[, c(2, 3, 4, 5)] # oprocentowanie

# zapisanie danych w plikach RDS

saveRDS(ratesFX, file = 'ratesFX.RDS')
saveRDS(foreignDebt, file = 'foreignDebt.RDS')
saveRDS(interestRateFX, file = 'interestRateFX.RDS')