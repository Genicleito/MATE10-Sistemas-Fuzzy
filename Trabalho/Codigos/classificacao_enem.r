#################################
# IMPORT DE BIBLIOTECAS

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(frbs)

#################################
# CARREGAMENTO DOS DADOS

# Assumindo que a diretorio do trabalho é a base do git
enem_2017 <- read_csv("Datasets/MICRODADOS_ENEM_2017.csv")

test.split <- 0.3

test.rows <- round(nrow(enem_2017) * test.split)

enem_2017.test <- enem_2017[1:test.rows, 1:ncol(enem_2017)-1]
enem_2017.label <- enem_2017[1:test.rows, ncol(enem_2017)]
enem_2017.train <- enem_2017[(test.rows+1):nrow(enem_2017), ]

#################################
# Geracao do modelo utilizando learn

data.range <- apply(enem_2017.train, 2, range)


#################################
# Definicao dos labels
lbl.idade <- c("crianca", "adolescente", "adulto", "meia idade", "idoso")

notas.range <- c(0, 1000)
idade.range <- c(1, 100)
