#instalación de packages, conversión de google sheet a df#

install.packages("tidyverse")
library("tidyverse")
install.packages("googlesheets4")
library("googlesheets4")


MatrizAbundancia<- read_sheet("https://docs.google.com/spreadsheets/d/1-LTl94o8ofQeFLNThc419k7E9dpuTpW5EZjXpuxsM_c/edit#gid=1034440562")
view(MatrizAbundancia)

library(vegan)

#riqueza de especies de cada Muestra del transecto#
Riqueza <- colSums(apply(MatrizAbundancia[-1, 3:6], 2, function(x) x > 0))



#Indice de diversidad Shannon#






