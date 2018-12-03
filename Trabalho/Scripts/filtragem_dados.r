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

rm(atributos.selecionados)

MICRODADOS_ENEM_2017_FULL <- na.exclude(MICRODADOS_ENEM_2017_FULL)

# Transformacao de dados numericos
MICRODADOS_ENEM_2017_FULL$TP_SEXO = as.numeric( unlist( as.factor(MICRODADOS_ENEM_2017_FULL$TP_SEXO)))
MICRODADOS_ENEM_2017_FULL$Q006 = as.numeric(unlist( as.factor(MICRODADOS_ENEM_2017_FULL$Q006)))
MICRODADOS_ENEM_2017_FULL$Q027 = as.numeric(unlist( as.factor(MICRODADOS_ENEM_2017_FULL$Q027)))

write_csv(MICRODADOS_ENEM_2017_FULL, "Datasets/MICRODADOS_ENEM_2017.csv")
