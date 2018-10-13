# Implementação do Wang&Mendel como descrito no artigo

# # Lê a base de dados
# data <- read.csv('./GitHub/sistemasFuzzy/Códigos/jogarTenis.csv', sep = ',')

# # A coluna 1 é um identificador do dia
# data <- data[, -1]

data <- iris

# Função para pre-processar a base de dados em questão
preProcessar <- function(db) {
  # Converte os dados em numéricos
  for(i in c(1: ncol(db))){
    db[i] = as.numeric( unlist(db[i]) )
  }
  
  return (db)
}

defineTrainSet <- function(db) {
  # Coleta dados de forma aleatória para ser o conjunto de treinamento (1/3 da base)
  return (db[ (1 : ( nrow(db) * 1 / 3 )), ])
  
}

defineTestSet <- function(db) {
  # Coleta dados de forma aleatória para ser o conjunto de teste (2/3 da base)
  return(db[ (nrow(db) * 1 / 3) : ( nrow(db) ), ])
}

# Função para definir intervalos dos atributos da base de dados
# TODO
defineIntervalos <- function(db) {
  range <- c()
  names <- c()
  for(i in c(1 : ncol(db))) {
    # cat("Interval of ", colnames(db)[i], ": [", min(db[ , i]), ", ", max(db[ , i]), "]\n")
    range <- c(range, min(db[ , i]), max(db[ , i]))
    names <- c(names, colnames(db)[i])
  }
  result <- data.frame(matrix(data = range, nrow = 2, ncol = ncol(db), byrow = FALSE))
  colnames(result) <- names
  return (result)
}


# db é a base com os intervalos de cada variavel
defineRegioesFuzzy <- function(db, nRegions) {
  regions <- c()
  nRows <- length( seq( min(db[ , 0]), max(db[ , 0]), (( (max(db[ , 0]) - min(db[ , 0]) ) / nRegions) / 2 ) ) )
  for(i in c(1 : ncol(db))) {
    regions <- c( regions, seq( min(db[ , i]), max(db[ , i]), (( (max(db[ , i]) - min(db[ , i]) ) / nRegions) / 2 ) ) )
  }
  return (data.frame(matrix(data = regions, nrow = nRows)))
}

# Chamada de funções
data <- preProcessar(data)
data.test <- defineTestSet(data)
data.train <- defineTrainSet(data)
intervalos <- defineIntervalos(data.train)
