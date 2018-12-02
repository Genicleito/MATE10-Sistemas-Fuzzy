library(dplyr)
library(tidyr)
library(purrr)

# Filtragem de colunas desnecessárias
MICRODADOS_ENEM_2017 <- MICRODADOS_ENEM_2017 %>% mutate(TP_ENSINO = NULL, 
                                                        CO_ESCOLA = NULL, 
                                                        CO_MUNICIPIO_ESC = NULL, 
                                                        NO_MUNICIPIO_ESC = NULL, 
                                                        CO_UF_ESC = NULL, 
                                                        SG_UF_ESC = NULL, 
                                                        TP_DEPENDENCIA_ADM_ESC = NULL, 
                                                        TP_LOCALIZACAO_ESC = NULL, 
                                                        TP_SIT_FUNC_ESC = NULL,
                                                        CO_PROVA_CH = NULL,
                                                        CO_PROVA_CN = NULL,
                                                        CO_PROVA_LC = NULL,
                                                        CO_PROVA_MT = NULL,
                                                        TX_RESPOSTAS_CH = NULL,
                                                        TX_RESPOSTAS_CN = NULL,
                                                        TX_RESPOSTAS_LC = NULL,
                                                        TX_RESPOSTAS_MT = NULL,
                                                        TX_GABARITO_CH = NULL,
                                                        TX_GABARITO_CN = NULL,
                                                        TX_GABARITO_LC = NULL,
                                                        TX_GABARITO_MT = NULL,
                                                        Q001 = NULL,
                                                        Q002 = NULL,
                                                        Q003 = NULL,
                                                        Q004 = NULL,
                                                        Q005 = NULL,
                                                        Q006 = NULL,
                                                        Q007 = NULL,
                                                        Q008 = NULL,
                                                        Q009 = NULL,
                                                        Q010 = NULL,
                                                        Q011 = NULL,
                                                        Q012 = NULL,
                                                        Q013 = NULL,
                                                        Q014 = NULL,
                                                        Q015 = NULL,
                                                        Q016 = NULL,
                                                        Q017 = NULL,
                                                        Q018 = NULL,
                                                        Q019 = NULL,
                                                        Q020 = NULL,
                                                        Q021 = NULL,
                                                        Q022 = NULL,
                                                        Q023 = NULL,
                                                        Q024 = NULL,
                                                        Q025 = NULL,
                                                        Q026 = NULL,
                                                        Q027 = NULL)

MICRODADOS_ENEM_2017 <- MICRODADOS_ENEM_2017 %>% filter(TP_PRESENCA_CH != 0 & 
                                                        TP_PRESENCA_CN != 0 &
                                                        TP_PRESENCA_LC != 0 &
                                                        TP_PRESENCA_MT != 0 &
                                                        TP_STATUS_REDACAO != 4 &
                                                        IN_TREINEIRO != 1)

MICRODADOS_ENEM_2017 <- MICRODADOS_ENEM_2017 %>% mutate(TP_PRESENCA_CH = NULL,
                                                        TP_PRESENCA_CN = NULL,
                                                        TP_PRESENCA_LC = NULL,
                                                        TP_PRESENCA_MT = NULL,
                                                        IN_TREINEIRO = NULL)

MICRODADOS_ENEM_2017 <- na.exclude(MICRODADOS_ENEM_2017)
