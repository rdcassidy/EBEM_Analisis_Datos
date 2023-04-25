
install.packages("tidyverse")
library("tidyverse")
install.packages("googlesheets4")
library("googlesheets4")
library(vegan)

##descarga google sheet
CoberturaMatriz <- read_sheet("https://docs.google.com/spreadsheets/d/1UG3XUF4fXN6FYNzRiqvKGMEw_yLq5sz0OjaICzwiH_k/edit#gid=0")

AbundanciaMatriz<- read_sheet("https://docs.google.com/spreadsheets/d/1OeJvRsEYiNwlcfnmKyQTQzmtppLDWD1eq8xYeuXDIng/edit#gid=0")

###pie chart de Muestra 1, sin valores de 0##

library(dplyr)
library(ggplot2)

CoberturaMatriz %>%
  filter(M5 != 0) %>%
  ggplot(aes(x="", y=M5, fill=Cobertura)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title="M5")


##Abundancia histograma
hist(AbundanciaMatriz$M1)
ggplot(AbundanciaMatriz)




soy profe rober guiri y tu eres elena 
