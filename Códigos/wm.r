# Implementação do Wang&Mendel como descrito no artigo

# Lê a base de dados
db <- read.csv('./GitHub/sistemasFuzzy/Códigos/jogarTenis.csv', sep = ',')

# Função para pre-processar a base de dados em questão
preProcessar <- function() {
  # Converte os dados em numéricos
  for(i in c(1: ncol(db))){
    db[i] = as.numeric( unlist(db[i]) )
  }
  
  # Mistura os dados (menos a coluna 1 que é um identificador do dia)
  db <- db[ sample( nrow(db), nrow(db)), -1]
  
  # Coleta dados de forma aleatória para ser o conjunto de treinamento (1/3 da base)
  db.train <- db[ (1 : ( nrow(db) * 1 / 3 )), ]
  
  # Coleta dados de forma aleatória para ser o conjunto de teste (2/3 da base)
  db.test <- db[ (nrow(db) * 1 / 3) : ( nrow(db) ), ]
}

# Função para definir intervalos dos atributos da base de dados
# TODO
defineIntervalos <- function() {
  for(i in c(1 : ncol(db))) {
    cat("Interval of ", colnames(db)[i], ": [", min(db[ , i]), ", ", max(db[ , i]), "]\n")
  }
}

