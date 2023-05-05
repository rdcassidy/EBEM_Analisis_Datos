#instalación de packages, conversión de google sheet a df#

install.packages("tidyverse")
library("tidyverse")
install.packages("googlesheets4")
library("googlesheets4")
install.packages("vegan")
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


#margalef index
library(vegan)
parc <- read_sheet("https://docs.google.com/spreadsheets/d/1v0Xib8StKjMBgxgyU_-iz8QWWC-9j8BNeM9ua-mgWlE/edit#gid=145651803")

Margalef <- parc %>% 
  group_by(Parcela) %>% 
  summarise(Margalef= (length(unique(Especie))-1)/log(NROW(Especie)))
Margalef
ggplot(Margalef)

library(dplyr)
Margalef <- Margalef %>%
  mutate(new_col = c("M1", "M2", "M3", "M4"))


Margalef <- Margalef %>%
  rename(column_name = new_col)

Margalef
packageVersion("vegan")
#join indices into same df##

merged_indices <- merge(H, S, by = "column_name")
merged_indices

all_indices <- merge(merged_indices, Margalef, by = "column_name")

all_indices <- all_indices %>%  
select (-Parcela)
all_indices

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


ggplot(Margalef, aes(x = Parcela, y = Margalef)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Bar Chart of Marg") +
  xlab("Column Name") +
  ylab("S Value")


##clustered bar chart###

long_indices <- all_indices %>% pivot_longer(-column_name)
long_indices

indices_grafico<- ggplot(long_indices, aes(x = column_name, y = value, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Cuadrícula", y = "Valor", fill = "Indice") +
  theme_gray()
indices_grafico


#####grupos troficos graficos####


piechart <- MatrizAbundancia %>%
  ggplot(aes(x="", y=M3, fill=GT)) +
  geom_bar(stat="identity", width=1, ) +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title="M3")
piechart


# use aggregate to sum the values of A by group in B
sum_GT1 <- aggregate(M1~ GT, data = MatrizAbundancia, FUN = sum)
sum_GT1

sum_GT2 <- aggregate(M2~ GT, data = MatrizAbundancia, FUN = sum)
sum_GT2

sum_GT3 <- aggregate(M3~ GT, data = MatrizAbundancia, FUN = sum)
sum_GT3

sum_GT4 <- aggregate(M4~ GT, data = MatrizAbundancia, FUN = sum)
sum_GT4

merge1 <- merge(sum_GT1, sum_GT2, by = "GT")

merge2 <- merge(sum_GT3, sum_GT4, by = "GT")

mergedGT <- merge(merge1, merge2, by = "GT")
mergedGT



barchart <- mergedGT %>% ggplot(aes(x = ))
