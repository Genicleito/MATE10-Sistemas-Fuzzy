library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Filtragem de colunas desnecessárias
MICRODADOS_ENEM_2017_FULL <- read_delim("Datasets/MICRODADOS_ENEM_2017_FULL.csv", 
                                        ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                        trim_ws = TRUE)

MICRODADOS_ENEM_2017_FULL <- MICRODADOS_ENEM_2017_FULL %>% filter(TP_PRESENCA_CH != 0 & 
                                                        TP_PRESENCA_CN != 0 &
                                                        TP_PRESENCA_LC != 0 &
                                                        TP_PRESENCA_MT != 0 &
                                                        TP_STATUS_REDACAO != 4 &
                                                        IN_TREINEIRO != 1)

atributos.selecionados <- c("NU_IDADE", "TP_SEXO", "TP_COR_RACA", "TP_ESCOLA",
                            "Q006", "Q027", "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT",
                            "NU_NOTA_REDACAO")

MICRODADOS_ENEM_2017_FULL <- MICRODADOS_ENEM_2017_FULL[, atributos.selecionados]

MICRODADOS_ENEM_2017_FULL <- na.exclude(MICRODADOS_ENEM_2017_FULL)

Q06 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
names(Q06) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")

Q27 <- c(1, 2, 3, 4, 5)
names(Q27) <- c("A", "B", "C", "D", "E")

MICRODADOS_ENEM_2017_FULL <- MICRODADOS_ENEM_2017_FULL %>% mutate(X1 = NULL) %>% 
                                                           mutate(RENDA_MENSAL = as.numeric(Q06[Q006])) %>%
                                                           mutate(TP_ENSINO_MEDIO = as.numeric(Q27[Q027])) %>%
                                                           mutate(TP_SEXO = if_else(TP_SEXO == "F", 0, 1))

atributos.selecionados <- c("NU_IDADE", "TP_SEXO", "TP_COR_RACA", "TP_ESCOLA",
                            "RENDA_MENSAL", "TP_ENSINO_MEDIO", "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT",
                            "NU_NOTA_REDACAO")

MICRODADOS_ENEM_2017_FULL <- MICRODADOS_ENEM_2017_FULL[, atributos.selecionados]

rm(atributos.selecionados)

write_csv(MICRODADOS_ENEM_2017_FULL, "Datasets/MICRODADOS_ENEM_2017.csv")