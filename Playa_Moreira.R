#instalación de packages, conversión de google sheet a df#

install.packages("tidyverse")
library("tidyverse")
install.packages("googlesheets4")
library("googlesheets4")

library(vegan)

MatrizAbundancia<- read_sheet("https://docs.google.com/spreadsheets/d/1JnAXYvbKoB3_U_uy9feUWILrsvCxFPqUthKyKe1t3dI/edit#gid=0")
view(MatrizAbundancia)

#riqueza de especies de cada Muestra del transecto#
Riqueza <- colSums(apply(MatrizAbundancia[-1, 3:6], 2, function(x) x > 0))


#filtro de datos, Indice de diversidad Shannon#

H <- MatrizAbundancia %>%
  select(3:6) %>%
  map_dfr(~data.frame(H = diversity(.x, "shannon")), 
          .id = "column_name")
H

#Filtro de datos, Indice de Simpson#
S<- MatrizAbundancia %>%
  select(3:6) %>%
  map_dfr(~data.frame(S = diversity(.x, "simpson")), 
          .id = "column_name")
S
ggplot(S)

#join indices into same df##

merged_indices <- merge(H, S, by = "column_name")
merged_indices

# Load required packages
library(ggplot2)


# Create bar charts of S and H
ggplot(S, aes(x = column_name, y = S)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Bar Chart of Column S") +
  xlab("Column Name") +
  ylab("S Value")

ggplot(H, aes(x = column_name, y = H)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Bar Chart of Column H") +
  xlab("Column Name") +
  ylab("S Value")
