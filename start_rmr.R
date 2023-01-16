# czyszczenie obszaru roboczego

gc(full = T)
rm(list = ls())
.rs.restartR()

# ustawienie ziarna losowania

set.seed(1)

# wczytanie pliku z potrzebny funkcjami do obliczen

source('user_functions_rmr.R')

# istalowanie, ladowanie lub wylodwanie pakietow

install.packages('data.table')
require(data.table)
detach('package:data.table', unload = T)

install.packages('PerformanceAnalytics')
require(PerformanceAnalytics)
detach('package:PerformanceAnalytics', unload = T)

install.packages('MonteCarlo')
require(MonteCarlo)
detach('package:MonteCarlo', unload = T)

# wczytanie poprzednich obliczen

load('wisla_obliczenia_20181115_1345.RData')

# zapamietanie obiektow
