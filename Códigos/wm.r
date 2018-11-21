source('funcoesPertinenciaFuzzy.r')

# Implementa��o do Wang&Mendel como descrito no artigo

# # L� a base de dados
# data <- read.csv('./GitHub/sistemasFuzzy/C�digos/jogarTenis.csv', sep = ',')

# # A coluna 1 � um identificador do dia
# data <- data[, -1]

data <- iris


regions <- c("Extremamente Baixa", "Baixissima", "Muito Baixa", "Baixa", "Um Pouco Baixa",
             "Normal",
             "Media", "Alta", "Muito Alta", "Altissima", "Extremamente Alta")

nRegions <- 3

idInicioRegioes <- (length(regions) - nRegions) / 2

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
# Continuar...
# Fazer um metodo para iterar sobre todos os elementos do data.train
# @param fuzzyRegionsXi - Regi�es Fuzzy da vari�vel Xi
generateFuzzyRules <- function(elem, fuzzyRegionsXi) {
  result <- c()
  for( i in seq(1, length(fuzzyRegionsXi)) ) {
    # cat("i = ", i, "\n")
    if(i == 1) {
      grau <- fuzzy_triangular(elem, fuzzyRegionsXi[i], fuzzyRegionsXi[i], fuzzyRegionsXi[i + 1])
      result <- c( result, grau )
    } else if( i == length(fuzzyRegionsXi) ) {
      result <- c(result, fuzzy_triangular(elem, fuzzyRegionsXi[i - 1], fuzzyRegionsXi[i], fuzzyRegionsXi[i]))
    } else {
      result <- c(result, fuzzy_triangular(elem, fuzzyRegionsXi[i - 1], fuzzyRegionsXi[i], fuzzyRegionsXi[i + 1]))
    }
  }
  # cat("\n [", elem, "]", ": Result: ", result, "\n")
  
  # # 1a forma
  # # Retorna o grau m�ximo de cada instancia do dataset
  # return( max(result) )
  
  # 2a forma
  # Retorna o grau m�ximo e a regi�o correspondente a este grau.
  return( c(max(result), regions[idInicioRegioes + which(result == max(result))[1]]) )
}

# Chamada de fun��es
data <- preProcessar(data)
data.test <- defineTestSet(data)
data.train <- defineTrainSet(data)
intervalos <- defineIntervalos(data.train)
fuzzyRegions <- defineRegioesFuzzy(intervalos, nRegions)

# Define a matriz que cont�m os graus de todas as int�ncias das vari�veis
grausMaximosVariaveis = data.frame( matrix(nrow = nrow(data.train), ncol = ncol(data.train) + 1) )
colnames(grausMaximosVariaveis) = c(names(data), "Degree.Rule")

# Define uma matriz com as regi�es correspondente a cada grau m�ximo obtido por cada elemento do dataset
regioesGrausMaximosVariaveis = data.frame( matrix(nrow = nrow(data.train), ncol = ncol(data.train)) )
colnames(regioesGrausMaximosVariaveis) = names(data)

# # 1a forma: Fun��o Inicial usando lapply
# for(i in seq(1, ncol(data.train)) ) {
  # grausMaximosVariaveis[ ,i] = unlist(lapply(data.train[ , i], generateFuzzyRules, fuzzyRegions[, i]))
# }

# # 2a forma
for( i in seq(1, ncol(data.train)) ) {
  for( j in seq(1, nrow(data.train)) ) {
    retorno <- generateFuzzyRules( data.train[j , i],  fuzzyRegions[, i] )
    grausMaximosVariaveis[j, i] = as.numeric(retorno[1])
    regioesGrausMaximosVariaveis[j, i] = retorno[2]
  }
}

# Determina o grau de cada regra e o armazena na coluna Degree.Rule do data.frame
grausMaximosVariaveis[ , 6] = apply(grausMaximosVariaveis[ , c(-6)], 1, prod)
