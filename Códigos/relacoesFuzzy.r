source('operacoesConjuntosFuzzy.r')
source('funcoesPertinenciaFuzzy.r')

# Variável Temperatura
termoTempBaixa <- seq(0, 25)
termoTempMedia <- seq(20, 35)
termoTempAlta <- seq(30, 60)

# Variável Umidade
termoUmidMuitoBaixa <- seq(0, 12)
termoUmidBaixa <- seq(12, 20)
termoUmidMedia <- seq(20, 30)

# # Variável Velocidade
# termoVelBaixa <- seq(0, 30, seq = 1)
# termoVelMedia <- seq(30, 60, seq = 1)
# termoVelAlta <- seq(60, 120, seq = 1)

temp <- list(termoTempBaixa, termoTempMedia, termoTempAlta)
umid <- list(termoUmidMuitoBaixa, termoUmidBaixa, termoUmidMedia)

for(i in c(1:length(temp))) {
  uniaoRelacao <- c()
  x <- unlist(temp[i])
  aA <- min(unlist(temp[i]))
  mA <- (min(unlist(temp[i])) + max(unlist(temp[i])) ) / 2
  bA <- max(unlist(temp[i]))
  for(j in c(1:length(umid))) {
    aB <- min(unlist(umid[j]))
    mB <- ( min(unlist(umid[j])) + max(unlist(umid[j])) ) / 2
    bB <- max(unlist(umid[j]))
    uniaoRelacao <- c( uniaoRelacao, calculaUniaoAxB(x, aA, mA, bA, aB, mB, bB) )
    # cat("Fuzzy Set A: ", unlist(temp[i]), "\n")
    # cat("Fuzzy Set B: ", unlist(umid[j]), "\n")
    # cat("Uniao: ", uniaoRelacao, "\n")
  }
  plot(c(0:(length(uniaoRelacao) - 1)), uniaoRelacao, type = "l", xlim = c(min(uniaoRelacao), length(uniaoRelacao)), ylim = c(0.039, 1), ylab = "", xlab = "Uniao AxB")
}
