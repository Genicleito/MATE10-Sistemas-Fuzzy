library('frbs')

# load('./GitHub/sistemasFuzzy/C�digos/bancoufba.rData')
dt <- read.csv('./GitHub/sistemasFuzzy/C�digos/jogarTenis.csv', sep = ',')

for(i in c(1: ncol(dt))){
  dt[i] = as.numeric( unlist(dt[i]) )
}

# Mistura os dados (menos a coluna 1 que � um identificador do dia)
dt <- dt[ sample( nrow(dt), nrow(dt)), -1]

# Coleta dados de forma aleat�ria para ser o conjunto de treinamento
dt.train <- dt[ (1 : ( nrow(dt) * 1 / 3 )), ]

# Coleta dados de forma aleat�ria para ser o conjunto de teste
dt.test <- dt[ (nrow(dt) * 1 / 3) : ( nrow(dt) ), ]


wm <- function() {
  range.data <- apply(dt.train, 2, range)
  method.type <- "WM"
  
  control <- list(num.labels = 5, type.mf = "TRIANGLE", type.defuz = "WAM",
                  type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH",
                  name = "sim-jogarTenis")
  a <- frbs.learn(dt.train, range.data, method.type, control)
  return (a)
}


