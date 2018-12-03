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
# Geracao do modelo atraves do fbrs.gen
idade.label <- c("crianca", "adolescente", "adulto", "meia idade", "idoso")
sexo.label <- c("feminino", "masculino")
cor_raca.label <- c("branca", "preta", "parda", "amarela", "indigena")
escola.label <- c("publica", "privada", "exterior")
q006.label <- c("sem renda",
                "ate 937,00",
                "de 937,01 ate 1.045,50",
                "de R$1.405,51 ate R$1.874,00",
                "de R$1.874,01 ate R$2.342,50",
                "de R$2.342,51 ate R$2.811,00",
                "de R$2.811,01 ate R$3.748,00",
                "de R$3.748,01 ate R$4.685,00",
                "de R$4.685,01 ate R$5.622,00",
                "de R$5.622,01 ate R$6.559,00",
                "de R$6.559,01 ate R$7.496,00",
                "de R$7.496,01 ate R$8.433,00",
                "de R$8.433,01 ate R$9.370,00",
                "de R$9.370,01 ate R$11.244,00",
                "de R$11.244,01 ate R$14.055,00",
                "de R$14.055,01 ate R$18.740,00",
                "mais de R$ 18.740,00")
q027.label <- c("somente em escola publica",
                "parte em escola publica e parte em escola privada sem bolsa de estudo integral",
                "parte em escola publica e parte em escola privada com bolsa de estudo integral",
                "somente em escola privada SEM bolsa de estudo integral",
                "somente em escola privada COM bolsa de estudo integral")
nota_cn.label <- c("A+", "A", "A−", "B+", "B", "B−", "C+", "C", "C−", "D+", "D", "D−", "F")
nota_ch.label <- c("A+", "A", "A−", "B+", "B", "B−", "C+", "C", "C−", "D+", "D", "D−", "F")
nota_lc.label <- c("A+", "A", "A−", "B+", "B", "B−", "C+", "C", "C−", "D+", "D", "D−", "F")
nota_mt.label <- c("A+", "A", "A−", "B+", "B", "B−", "C+", "C", "C−", "D+", "D", "D−", "F")
nota_redacao.label <- c("A+", "A", "A−", "B+", "B", "B−", "C+", "C", "C−", "D+", "D", "D−", "F")

idade.range <- c(1, 100)
sexo.range <- c(1, 2)
cor_raca.range <- c(1, 5)
escola.range <- c(2, 4)
q006.range <- c(1, 17)
q027.range <- c(1,5)
nota_cn.range <- c(0, 1000)
nota_ch.range <- c(0, 1000)
nota_lc.range <- c(0, 1000)
nota_mt.range <- c(0, 1000)
nota_redacao.range <- c(0, 1000)

# Parametros do modelo
range.data <- matrix(c(idade.range,
                       sexo.range,
                       cor_raca.range,
                       escola.range,
                       q006.range,
                       q027.range,
                       nota_cn.range,
                       nota_ch.range,
                       nota_lc.range,
                       nota_mt.range), nrow = 2)
num.fvalinput <- matrix(c(5, 2, 5, 3, 17, 5, 13, 13, 13, 13), nrow = 1)
names.varinput <- c(idade.label,
                    sexo.label,
                    cor_raca.label,
                    escola.label,
                    q006.label,
                    q027.label,
                    nota_cn.label,
                    nota_ch.label,
                    nota_lc.label,
                    nota_mt.label)
num.fvaloutput <- matrix(13, nrow = 1)
varout.mf <-c() # somente para mandani, buscar com wang-mendel
names.varoutput <- c(nota_redacao.label)
type.model <- "MAMDANI"
type.defuz <- "WAM"
type.tnorm <- "MIN"
type.snorm <- "MAX"
func.tsk <- 
colnames.var <-
  type.implication.func <- 
type.implication.func <- "LUKASIEWICZ"
