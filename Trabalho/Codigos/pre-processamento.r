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

# Normaliza os dados entre [0, 1]
normalizarDados <- function(dadosSemNA) {
  return( ( dadosSemNA - min(dadosSemNA) ) / ( max(dadosSemNA) - min(dadosSemNA) ) )
}