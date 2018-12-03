source('funcoesPertinenciaFuzzy.r')

# Implementacao do Wang&Mendel como descrito no artigo

## Le o dataset
dataset <- read.csv("dados-sem-nominais.csv", header = TRUE, sep = ",")

# Remove os inscritos que nao informaram o tipo da escola que estudaram
dataset <- dataset[ dataset$TP_ESCOLA != 1,  ]

## Divide o dataset
data.train <- dataset[1 : ( nrow(dataset) * 2 / 3 ), ]
data.tst <- dataset[ ( (nrow(dataset) * 2 / 3) + 1 ) :  nrow(dataset), -ncol(dataset) ]
real.val <- dataset[ ( (nrow(dataset) * 2 / 3) + 1 ) :  nrow(dataset), ncol(dataset) ]

## Define o intervalo dos dados
range.data <- apply(data.train, 2, range)

## data <- iris
data <- data.train


# regions <- c("Extremamente Baixa", "Baixissima", "Muito Baixa", "Baixa", "Um Pouco Baixa",
#              "Normal",
#              "Media", "Alta", "Muito Alta", "Altissima", "Extremamente Alta")

regions <- c("Extremamente.Baixa", "Baixissima", "Muito.Baixa", "Baixa",
             "Media",
             "Alta", "Muito.Alta", "Altissima", "Extremamente.Alta")

ling.terms.NU_IDADE <- c("jovem", "adulto", "idoso")
ling.terms.TP_SEXO <- c("masculino", "feminino")
ling.terms.TP_COR_RACA <- c("branca", "parda", "preta")
ling.terms.TP_ESCOLA <- c("publica", "privada", "exterior")
ling.terms.Q006 <- c("baixa", "media", "alta")  ## Renda A = 0, B = ate 937, ...
ling.terms.Q027 <- c("publica", "publica.e.privada", "privada")
ling.terms.NU_NOTA_CN <- c("baixa", "media", "alta")
ling.terms.NU_NOTA_CH <- c("baixa", "media", "alta")
ling.terms.NU_NOTA_LC <- c("baixa", "media", "alta")
ling.terms.NU_NOTA_MT <- c("baixa", "media", "alta")

## Define o numero de termos linguisticos da variavel de saida NU_NOTA_REDACAO
ling.terms.NU_NOTA_REDACAO <- c("baixa", "media", "alta")

# nRegions <- 3
nRegions <- c(3, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3)

# idInicioRegioes <- (length(regions) - nRegions) / 2
idInicioRegioes <- (length(regions) / 2) - 1

# Funcao para definir intervalos dos atributos da base de dados
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


# db ?? a base com os intervalos de cada variavel
defineRegioesFuzzy <- function(db, nRegions) {
  regions <- c()
  nRows <- length( seq( min(db[ , 1]), max(db[ , 1]), (( (max(db[ , 1]) - min(db[ , 1]) ) / (max(nRegions) - 1)) ) ) )
  for(i in c(1 : ncol(db))) {
    minimo <- min(db[ , i])
    maximo <- max(db[ , i])
    tamIntervalos <- ( max(db[ , i]) - min(db[ , i]) )
    if(minimo == maximo) {
      regions <- c(regions, (rep(minimo, nRegions[i])))
      
      # for(j in nRegions[i] : max(nRegions)) {
        regions <- c(regions, rep("NA", max(nRegions) - nRegions[i]))
      # }
    } else {
      regions <- c( regions, seq( minimo, maximo, ( tamIntervalos / (nRegions[i] - 1) ) ) )
      
      # for(j in nRegions[i] : max(nRegions)) {
        regions <- c(regions, rep("NA", max(nRegions) - nRegions[i]))
      # }
    }
    # cat("Regions of ", names(db)[i], ":", regions, "\n\n")
  }
  result <- data.frame(matrix(data = regions, nrow = nRows))
  colnames(result) <- names(db)
  return(result)
}
# Continuar...
# Fazer um metodo para iterar sobre todos os elementos do data.train
# @param fuzzyRegionsXi - Regi~??es Fuzzy da vari??vel Xi
generateFuzzyRules <- function(elem, fuzzyRegionsXi) {
  result <- c()
  regionsXi <- c()
  for(i in 1:length(fuzzyRegionsXi)) {
    if(!is.na(fuzzyRegionsXi[i])) {
      regionsXi <- c(regionsXi, fuzzyRegionsXi[i])
    }
  }
  fuzzyRegionsXi <- regionsXi
  
  for( i in seq(1, length(fuzzyRegionsXi)) ) {

    if(!is.na(fuzzyRegionsXi)) {
      if(i == 1) {
        grau <- fuzzy_triangular(elem, fuzzyRegionsXi[i], fuzzyRegionsXi[i], fuzzyRegionsXi[i + 1])
        result <- c( result, grau )
      } else if( i == length(fuzzyRegionsXi) ) {
        result <- c(result, fuzzy_triangular(elem, fuzzyRegionsXi[i - 1], fuzzyRegionsXi[i], fuzzyRegionsXi[i]))
      } else {
        result <- c(result, fuzzy_triangular(elem, fuzzyRegionsXi[i - 1], fuzzyRegionsXi[i], fuzzyRegionsXi[i + 1]))
      }
    }
  }
  # cat("\n [", elem, "]", ": Result: ", result, "\n")
  
  # # 1a forma
  # # Retorna o grau m??ximo de cada instancia do dataset
  # return( max(result) )
  
  # 2a forma
  # Retorna o grau maximo e a regiao correspondente a este grau.
  # return( c(max(result), regions[idInicioRegioes + which(result == max(result))[1]]) )
  return( c(max(result), regions[ which( result == max(result) )[1] ]) )
}

# Chamada de funcoes
data <- preProcessar(data)
data.test <- defineTestSet(data)
data.train <- defineTrainSet(data)
fuzzy.intervals <- defineIntervalos(data.train)
fuzzy.regions <- defineRegioesFuzzy(fuzzy.intervals, nRegions)

# Define a matriz que cont??m os graus de todas as int??ncias das vari??veis
grausMaximosVariaveis = data.frame( matrix(nrow = nrow(data.train), ncol = ncol(data.train) + 1) )
colnames(grausMaximosVariaveis) = c(names(data), "Degree.Rule")

# Define uma matriz com as regi??es correspondente a cada grau m??ximo obtido por cada elemento do dataset
regioesGrausMaximosVariaveis = data.frame( matrix(nrow = nrow(data.train), ncol = ncol(data.train)) )
colnames(regioesGrausMaximosVariaveis) = names(data)

# Define a base de regras
rules.data <- c()

# # 1a forma: Fun????o Inicial usando lapply
# for(i in seq(1, ncol(data.train)) ) {
  # grausMaximosVariaveis[ ,i] = unlist(lapply(data.train[ , i], generateFuzzyRules, fuzzyRegions[, i]))
# }

# # 2a forma
for( i in seq(1, ncol(data.train)) ) {
  for( j in seq(1, nrow(data.train)) ) {
    retorno <- generateFuzzyRules( data.train[j , i],  fuzzy.regions[, i] )
    grausMaximosVariaveis[j, i] = as.numeric(retorno[1])
    regioesGrausMaximosVariaveis[j, i] = retorno[2]
  }
  cat("")
}

# Determina o grau de cada regra e o armazena na coluna Degree.Rule do data.frame
grausMaximosVariaveis[ , 6] = apply(grausMaximosVariaveis[ , c(-6)], 1, prod)

# Cria strings que representam as regras
for(linha in seq(1, nrow(regioesGrausMaximosVariaveis))) {
  regra <- paste("IF ")
  for( coluna in seq( 1, ncol(regioesGrausMaximosVariaveis) ) ) {
    if(coluna == ncol(regioesGrausMaximosVariaveis)) {
      # regra <- paste( regra, names(data)[coluna], " IS ", regioesGrausMaximosVariaveis[linha, coluna], "\n\n" )
      regra <- paste( regra, names(data)[coluna], " IS ", regioesGrausMaximosVariaveis[linha, coluna] )
    } else if( coluna == ncol(regioesGrausMaximosVariaveis) - 1 ) {
      regra <- paste( regra, names(data)[coluna], " IS ", regioesGrausMaximosVariaveis[linha, coluna], " THEN " )
    } else {
      regra <- paste( regra, names(data)[coluna], " IS ", regioesGrausMaximosVariaveis[linha, coluna], " AND " )
    }
  }
  # cat(regra)
  rules.data <- c(rules.data, regra)
}
