linhasComNA <- function(dataSet) {
  cat("Construindo lista de NAs...\n")
  linhasRemovidas <- c()
  for( linha in seq(1, nrow(dataSet)) ) {
    
    # for(coluna in seq_along(dataSet)) {
    for(coluna in seq( 1:ncol(dataSet)) ) {
      if(is.na(dataSet[linha, coluna])) {
        linhasRemovidas <- c(linhasRemovidas, linha)
        break
      }
    }
  }
  cat("Lista de NAs construída.\n")
  return(linhasRemovidas)
}

# dados.atributos.relevantes <- read.csv(file = "base-enem-pre-processada.csv", header = TRUE, sep = ",")

# dados.sem.na <- dados.atributos.relevantes[-removerLinhasComNA(dados.atributos.relevantes), ]
# dados.sem.na