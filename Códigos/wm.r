# Implementa��o do Wang&Mendel como descrito no artigo

# # L� a base de dados
# data <- read.csv('./GitHub/sistemasFuzzy/C�digos/jogarTenis.csv', sep = ',')

# # A coluna 1 � um identificador do dia
# data <- data[, -1]

data <- iris

# Fun��o para pre-processar a base de dados em quest�o
preProcessar <- function(db) {
  # Converte os dados em num�ricos
  for(i in c(1: ncol(db))){
    db[i] = as.numeric( unlist(db[i]) )
  }
  
  return (db)
}

defineTrainSet <- function(db) {
  # Coleta dados de forma aleat�ria para ser o conjunto de treinamento (1/3 da base)
  return (db[ (1 : ( nrow(db) * 1 / 3 )), ])
  
}

defineTestSet <- function(db) {
  # Coleta dados de forma aleat�ria para ser o conjunto de teste (2/3 da base)
  return(db[ (nrow(db) * 1 / 3) : ( nrow(db) ), ])
}

# Fun��o para definir intervalos dos atributos da base de dados
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


# db � a base com os intervalos de cada variavel
defineRegioesFuzzy <- function(db, nRegions) {
  regions <- c()
  nRows <- length( seq( min(db[ , 1]), max(db[ , 1]), (( (max(db[ , 1]) - min(db[ , 1]) ) / (nRegions - 1)) ) ) )
  for(i in c(1 : ncol(db))) {
    minimo <- min(db[ , i])
    maximo <- max(db[ , i])
    tamIntervalos <- ( max(db[ , i]) - min(db[ , i]) )
    if(minimo == maximo) {
      regions <- c(regions, (rep(minimo, nRows)))
    } else {
      regions <- c( regions, seq( minimo, maximo, ( tamIntervalos / (nRegions - 1) ) ) )
    }
    # cat("Regions of ", names(db)[i], ":", regions, "\n\n")
  }
  result <- data.frame(matrix(data = regions, nrow = nRows))
  colnames(result) <- names(db)
  return(result)
}

# Chamada de fun��es
data <- preProcessar(data)
data.test <- defineTestSet(data)
data.train <- defineTrainSet(data)
intervalos <- defineIntervalos(data.train)
