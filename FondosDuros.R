
install.packages("tidyverse")
library("tidyverse")
install.packages("googlesheets4")
library("googlesheets4")
library(vegan)

##descarga google sheet
CoberturaMatriz <- read_sheet("https://docs.google.com/spreadsheets/d/1UG3XUF4fXN6FYNzRiqvKGMEw_yLq5sz0OjaICzwiH_k/edit#gid=0")

AbundanciaMatriz<- read_sheet("https://docs.google.com/spreadsheets/d/1OeJvRsEYiNwlcfnmKyQTQzmtppLDWD1eq8xYeuXDIng/edit#gid=0")

###pie chart de Muestra 1-10, sin valores de 0##

library(dplyr)
library(ggplot2)

##opción sin porcentajes###
make_plots <- function(df, cols) {
  lapply(cols, function(col) {
    df %>%
      filter(.data[[col]] != 0) %>%
      ggplot(aes(x="", y=.data[[col]], fill=Cobertura)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() +
      labs(title=col)
  })
}

###opción con porcentajes feas :((  ###
make_plots <- function(df, cols) {
  lapply(cols, function(col) {
    df %>%
      filter(.data[[col]] != 0) %>%
      ggplot(aes(x="", y=.data[[col]], fill=Cobertura)) +
      geom_bar(stat="identity", width=1, color="white", position = "stack") +
      geom_text(aes(label = paste0(round(.data[[col]]/sum(.data[[col]])*100), "%")), 
                position = position_stack(vjust = 0.5), color = "white") +
      coord_polar("y", start=0) +
      theme_void(base_size = 15) +
      theme(panel.border = element_blank(),
            panel.spacing = element_blank()) +
      labs(title=col) +
      theme(plot.title = element_text(hjust = 0.5))
  })
}


cols <- paste0("M", 1:10)
plots <- make_plots(CoberturaMatriz, cols)
plots




####gráfico de bares segmentados cobertura ###


longcob <- CoberturaMatriz %>% pivot_longer(-Cobertura)
longcob

lc <- longcob %>% select(longcob$name, everything())
lc

graf_cob <- ggplot(longcob, aes(fill= Cobertura, y=value, x=name)) + 
  geom_bar(position="stack", stat="identity", color = "black", width = 0.5)  +
  labs(title = "Cobertura de Parcelas", x = "Cuadrícula", y = "Valor") +
theme(legend.box.background = element_rect(colour = "black"))
graf_cob

print(longcob, n = 100    )


####clustered bar chart abundance####

longabund <- AbundanciaMatriz %>% pivot_longer(-Taxon)

orderedabund <- longabund %>% group_by(name)
orderedabund

graf_abund<- ggplot(longabund, aes(x = name, y = value, fill = Taxon)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Abundancia", x = "Cuadrícula", y = "Valor", fill = "Taxon") +
  theme_gray()
graf_abund









#############################################################################################################################################

#### no uses nada debajo de aquí, son intentos previos######################################################################################

longabund <- AbundanciaMatriz %>% pivot_longer(-Taxon)
longabund

cbp1 <- c("#999999", "#E69F00","#009E73",
          "#F0E442", "#0072B2", "#CC79A7")
graf_abund1 <- ggplot(longabund, aes(fill= Taxon, y=value, x=name)) + 
  geom_bar(position="stack", stat="identity", color = "black") + 
  scale_fill_manual(values = cbp1) +
  labs(title = "Abundancia de Especies", x = "Nombre", y = "Valor") +
  theme(legend.box.background = element_rect(colour = "black")) 
graf_abund1


CoberturaMatriz %>%
  filter(M1 != 0) %>%
  ggplot(aes(x="", y=M1, fill=Cobertura)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title="M1")


CoberturaMatriz %>%
  filter(M2 != 0) %>%
  ggplot(aes(x="", y=M2, fill=Cobertura)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title="M2")



apply(my_df[, 3:6], 2, function(x) {
  i <<- match(names(my_df)[which(names(my_df) %in% colnames(x))], colnames(my_df))
  create_plot(cbind(my_df[, 1:2], x))
})



apply(CoberturaMatriz[, 2:11], 2, function(x) {
  create_pie_chart(cbind(CoberturaMatriz[, 1], x))
  
})
  
  
##Abundancia histograma
hist(AbundanciaMatriz$M1)
ggplot(AbundanciaMatriz)



##chatgpt attempt

make_plots <- function(df, cols) {
  lapply(cols, function(col) {
    df %>%
      filter(.data[[col]] != 0) %>%
      ggplot(aes(x="", y=.data[[col]], fill=Cobertura)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() +
      labs(title=col)
  })
}

cols <- paste0("M", 1:10)
plots <- make_plots(CoberturaMatriz, cols)
plots

