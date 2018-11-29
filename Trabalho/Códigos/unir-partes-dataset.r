# As notas nas competencias podem nao ser interessantes pois elas já dão uma noção da nota final da redação
competencias.redacao <- c("NU_NOTA_COMP1", "NU_NOTA_COMP2", "NU_NOTA_COMP3", "NU_NOTA_COMP4", "NU_NOTA_COMP5")

###############################################################################

enem2017 <- read.csv("aa.csv", header = TRUE, sep = ";")

# Nomes das colunas
atributos.dataset <- colnames(enem2017)

# Atributos realmente selecionados
atributos.selecionados <- c("NO_MUNICIPIO_RESIDENCIA", "NU_IDADE", "TP_SEXO", "TP_COR_RACA", "TP_ESCOLA", 
                          "Q006", "Q027", "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", 
                          "NU_NOTA_REDACAO")

enem2017 <- enem2017[, atributos.selecionados]


files.names <- paste("a", letters[seq(2, 26)], sep = "")

dataset.atributos.relevantes <- enem2017
#colnames(dataset.atributos.relevantes) <- atributos.selecionados

for(file.name in files.names[c(1)]) {
  cat("Lendo arquivo...", file.name, "\n")
  file <- read.csv(file.name, header = FALSE, sep = ";")
  cat("Terminou de ler o arquivo", file.name, "\n")
  colnames(file) = colnames(enem2017)
  
  # Aplica o filtro de atributos selecionados
  file <- file[ , atributos.selecionados]
  
  dataset.atributos.relevantes <- rbind(dataset.atributos.relevantes, file)
  
  # dataset.atributos.relevantes[, atributos.selecionados] = file[, atributos.selecionados]
}

a <- matrix(nrow = 2, ncol = 2)
a[1, ] = c(11, 12)
a[2, ] = c(21, 22)

b <- matrix(nrow = 2, ncol = 2)
b[1, ] = c(-11, -12)
b[2, ] = c(-21, -22)
