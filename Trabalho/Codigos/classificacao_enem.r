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
enem_2017 <- read_csv("Datasets/base-enemBA-pre-processada.csv")
enem_2017$TP_ESCOLA <- NULL

enem_2017$TP_SEXO = as.numeric( unlist( as.factor(enem_2017$TP_SEXO)))
enem_2017$Q006 = as.numeric(unlist( as.factor(enem_2017$Q006)))
enem_2017$Q027 = as.numeric(unlist( as.factor(enem_2017$Q027)))

test.split <- 0.3

test.rows <- round(nrow(enem_2017) * test.split)

enem_2017.test <- enem_2017[1:test.rows, 1:ncol(enem_2017)-1]
enem_2017.label <- enem_2017[1:test.rows, ncol(enem_2017)]
enem_2017.train <- enem_2017[(test.rows+1):nrow(enem_2017), ]

#################################
# Geracao do modelo atraves do fbrs.gen
idade.label <- c("crianca", "adolescente", "adulto", "idoso")
sexo.label <- c("feminino", "masculino")
cor_raca.label <- c("branca", "preta", "parda", "amarela", "indigena")
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
nota_cn.label <- c("A", "B", "C", "D", "E", "F")
nota_ch.label <- c("A", "B", "C", "D", "E", "F")
nota_lc.label <- c("A", "B", "C", "D", "E", "F")
nota_mt.label <- c("A", "B", "C", "D", "E", "F")
nota_redacao.label <- c("A", "B", "C", "D", "E", "F")

idade.range <- c(1, 100)
sexo.range <- c(1, 2)
cor_raca.range <- c(1, 5)
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
                       q006.range,
                       q027.range,
                       nota_cn.range,
                       nota_ch.range,
                       nota_lc.range,
                       nota_mt.range,
                       nota_redacao.range), nrow = 2)
num.fvalinput <- matrix(c(4, 2, 5, 17, 5, 6, 6, 6, 6), nrow = 1)
names.varinput <- c(idade.label,
                    sexo.label,
                    cor_raca.label,
                    q006.label,
                    q027.label,
                    nota_cn.label,
                    nota_ch.label,
                    nota_lc.label,
                    nota_mt.label)
num.fvaloutput <- matrix(c(6) , nrow = 1)
varout.mf <- matrix(c(2,0,0,400,500,
                      1,400,500,600,NA,
                      1,500,600,700,NA,
                      1,600,700,800,NA,
                      1,700,800,900,NA,
                      1,850,1000,1000,NA), nrow = 5, byrow = FALSE)
names.varoutput <- c(nota_redacao.label)
rule <- matrix(c("adolescente","and","masculino","and","parda","and","de R$14.055,01 ate R$18.740,00","and","parte em escola publica e parte em escola privada sem bolsa de estudo integral","and","C","and","C","and","C","and","E","->","D",
                 "adolescente","and","masculino","and","parda","and","mais de R$ 18.740,00","and","somente em escola privada SEM bolsa de estudo integral","and","E","and","B","and","B","and","C","->","A",
                 "adolescente","and","masculino","and","parda","and","mais de R$ 18.740,00","and","somente em escola privada SEM bolsa de estudo integral","and","A","and","B","and","A","and","A","->","A",
                 "adolescente","and","masculino","and","parda","and","de R$2.342,51 ate R$2.811,00","and","somente em escola privada SEM bolsa de estudo integral","and","B","and","B","and","B","and","B","->","A",
                 "adolescente","and","masculino","and","parda","and","de R$3.748,01 ate R$4.685,00","and","somente em escola privada SEM bolsa de estudo integral","and","C","and","C","and","B","and","E","->","E",
                 "adolescente","and","masculino","and","parda","and","de R$2.342,51 ate R$2.811,00","and","somente em escola privada SEM bolsa de estudo integral","and","C","and","C","and","B","and","C","->","D",
                 "adolescente","and","masculino","and","parda","and","de R$5.622,01 ate R$6.559,00","and","somente em escola privada SEM bolsa de estudo integral","and","C","and","C","and","B","and","D","->","D",
                 "adolescente","and","masculino","and","preta","and","de R$1.405,51 ate R$1.874,00","and","somente em escola privada SEM bolsa de estudo integral","and","C","and","C","and","C","and","C","->","D",
                 "adolescente","and","masculino","and","parda","and","de R$2.342,51 ate R$2.811,00","and","somente em escola privada COM bolsa de estudo integral","and","B","and","B","and","B","and","C","->","A",
                 "adolescente","and","masculino","and","parda","and","de R$1.874,01 ate R$2.342,50","and","parte em escola publica e parte em escola privada sem bolsa de estudo integral","and","E","and","D","and","C","and","D","->","D",
                 "adolescente","and","masculino","and","parda","and","de R$1.874,01 ate R$2.342,50","and","parte em escola publica e parte em escola privada sem bolsa de estudo integral","and","D","and","C","and","C","and","D","->","C",
                 "adolescente","and","masculino","and","amarela","and","de R$4.685,01 ate R$5.622,00","and","somente em escola privada COM bolsa de estudo integral","and","C","and","C","and","B","and","C","->","C",
                 "adolescente","and","masculino","and","parda","and","de R$1.874,01 ate R$2.342,50","and","somente em escola privada SEM bolsa de estudo integral","and","D","and","E","and","E","and","E","->","D",
                 "adolescente","and","masculino","and","parda","and","de R$1.874,01 ate R$2.342,50","and","somente em escola privada COM bolsa de estudo integral","and","C","and","C","and","C","and","E","->","D",
                 "adolescente","and","masculino","and","parda","and","de 937,01 ate 1.045,50","and","somente em escola privada SEM bolsa de estudo integral","and","B","and","B","and","B","and","B","->","A",
                 "adolescente","and","masculino","and","preta","and","de R$2.342,51 ate R$2.811,00","and","somente em escola privada SEM bolsa de estudo integral","and","E","and","C","and","B","and","C","->","C",
                 "adolescente","and","masculino","and","parda","and","de R$5.622,01 ate R$6.559,00","and","somente em escola privada SEM bolsa de estudo integral","and","C","and","C","and","C","and","D","->","C",
                 "adolescente","and","masculino","and","preta","and","mais de R$ 18.740,00","and","parte em escola publica e parte em escola privada sem bolsa de estudo integral","and","B","and","C","and","B","and","A","->","C",
                 "adolescente","and","masculino","and","parda","and","ate 937,00","and","somente em escola privada COM bolsa de estudo integral","and","C","and","B","and","B","and","C","->","A",
                 "adolescente","and","masculino","and","preta","and","de 937,01 ate 1.045,50","and","somente em escola privada COM bolsa de estudo integral","and","B","and","B","and","B","and","B","->","A",
                 "adolescente","and","masculino","and","preta","and","de R$1.405,51 ate R$1.874,00","and","somente em escola privada COM bolsa de estudo integral","and","B","and","C","and","C","and","B","->","B",
                 "adolescente","and","masculino","and","parda","and","ate 937,00","and","somente em escola privada SEM bolsa de estudo integral","and","C","and","C","and","B","and","E","->","D",
                 "adolescente","and","masculino","and","parda","and","de R$9.370,01 ate R$11.244,00","and","somente em escola privada SEM bolsa de estudo integral","and","C","and","B","and","B","and","B","->","C",
                 "adolescente","and","masculino","and","parda","and","de R$4.685,01 ate R$5.622,00","and","somente em escola privada SEM bolsa de estudo integral","and","C","and","C","and","C","and","C","->","E",
                 "adolescente","and","masculino","and","preta","and","de R$1.874,01 ate R$2.342,50","and","parte em escola publica e parte em escola privada sem bolsa de estudo integral","and","C","and","E","and","C","and","E","->","E"),
               nrow = 25, byrow = TRUE)
varinp.mf <- matrix(c(2,0,0,10,15,
                      1,12,16,21,NA,
                      4,18,25,60,65,
                      3,60,65,100,100,
                      1,1,1,2,NA,
                      1,1,2,2,NA,
                      1,1,1,2,NA,
                      1,1,2,3,NA,
                      1,2,3,4,NA,
                      1,3,4,5,NA,
                      1,4,5,5,NA,
                      1,1,1,2,NA,
                      1,1,2,3,NA,
                      1,2,3,4,NA,
                      1,3,4,5,NA,
                      1,4,5,6,NA,
                      1,5,6,7,NA,
                      1,6,7,8,NA,
                      1,7,8,9,NA,
                      1,8,9,10,NA,
                      1,9,10,11,NA,
                      1,10,11,12,NA,
                      1,11,12,13,NA,
                      1,12,13,14,NA,
                      1,13,14,15,NA,
                      1,14,15,16,NA,
                      1,15,16,17,NA,
                      1,16,17,17,NA,
                      1,1,1,2,NA,
                      1,1,2,3,NA,
                      1,2,3,4,NA,
                      1,3,4,5,NA,
                      1,4,5,5,NA,
                      2,0,0,400,500,
                      1,400,500,600,NA,
                      1,500,600,700,NA,
                      1,600,700,800,NA,
                      1,700,800,900,NA,
                      1,850,1000,1000,NA,
                      2,0,0,400,500,
                      1,400,500,600,NA,
                      1,500,600,700,NA,
                      1,600,700,800,NA,
                      1,700,800,900,NA,
                      1,850,1000,1000,NA,
                      2,0,0,400,500,
                      1,400,500,600,NA,
                      1,500,600,700,NA,
                      1,600,700,800,NA,
                      1,700,800,900,NA,
                      1,850,1000,1000,NA,
                      2,0,0,400,500,
                      1,400,500,600,NA,
                      1,500,600,700,NA,
                      1,600,700,800,NA,
                      1,700,800,900,NA,
                      1,850,1000,1000,NA), nrow = 5, byrow = FALSE)
type.model <- "MAMDANI"
type.defuz <- "WAM"
type.tnorm <- "MIN"
type.snorm <- "MAX"
func.tsk <- NULL
colnames.var <- c("idade", "sexo", "cor", "renda familiar", "ensino medio", "ciencias naturais",
                  "ciencias humanas", "linguagens e codigos", "matematica", "redacao")
type.implication.func <- "LUKASIEWICZ"
name <- "Sim-0"

object <- frbs.gen(range.data, num.fvalinput, names.varinput,
                   num.fvaloutput, varout.mf, names.varoutput, rule,
                   varinp.mf, type.model, type.defuz, type.tnorm,
                   type.snorm, func.tsk = NULL, colnames.var, type.implication.func, name)

plotMF(object)

newdata <- matrix(c(17, 2, 3,16, 2, 540.7, 583.0, 472.9,405.1,
                    31, 1, 2,17, 1, 396.4, 401.6, 350.7,494.2,
                    17, 1, 3, 6, 1, 452.8, 451.3, 529.1,439.1,
                    18, 1, 2, 4, 1, 496.0, 484.3, 515.7,531.8), nrow = 4, byrow = TRUE)

res <- predict(object, enem_2017.test)$predicted.val
