triangular <- function(x, a, m, b) {
    if (x >= a && x < m) {
        return ((x - a) / (m - a))
    } else if (x >= m && x < b) {
        return ((b - x) / (b - m))
    } else {
        return (0)
    }
}

trapezoidal <- function(x, a, m, n, b) {
    if (x < a || x > b) {
        return(0)
    } else if (x >= a & x < m) {
        return((x - a) / (m - a))
    } else if (x >= m & x < n) {
        return(1)
    } else if (x >= n & x < b) {
        return((b - x) / (b - n))
    }
}

# Variação de vendas
# Diminuindo
resultado_conjunto_fuzzy <- c()
conjunto_fuzzy <- seq(-100, 0, 1)
for (i in seq(-100, 1, 1)) {
    resultado_conjunto_fuzzy <- c(resultado_conjunto_fuzzy,
                                  trapezoidal(i, -100, -100, -50, 0))
}
plot(conjunto_fuzzy, resultado_conjunto_fuzzy, type = "l")

# Estável
resultado_conjunto_estavel <- c()
conjunto_estavel <- seq(-50, 50, 1)
for (i in conjunto_estavel) {
    resultado_conjunto_estavel <- c(resultado_conjunto_estavel,
                                    triangular(i, -50, 0, 50))
}
plot(conjunto_estavel, resultado_conjunto_estavel, type = "l")

# Aumentando
resultado_conjunto_aumentando <- c()
conjunto_aumentando <- seq(0, 100, 1)
for (i in seq(0, 101, 1)) {
    resultado_conjunto_aumentando <- c(resultado_conjunto_aumentando,
                                  trapezoidal(i, 0, 50, 100, 100))
}
plot(conjunto_aumentando, resultado_conjunto_aumentando, type = "l")

# Servicos alta [Serviços]
resultado_conjunto_media <- c()
conjunto_media <- seq(50, 100, 1)
for (i in conjunto_media) {
  resultado_conjunto_media <- c(resultado_conjunto_media,
                                  triangular(i, 0, 50, 100))
}
plot(conjunto_alta, resultado_conjunto_alta, type = "l")

# Servicos alta [Serviços]
resultado_conjunto_alta <- c()
conjunto_alta <- seq(50, 100, 1)
for (i in conjunto_alta) {
  resultado_conjunto_alta <- c(resultado_conjunto_alta,
                               triangular(i, 50, 100, 100))
}
plot(conjunto_alta, resultado_conjunto_alta, type = "l")

# Informatizacao bom [Informatizacao]
resultado_conjunto_bom <- c()
conjunto_bom <- seq(50, 100, 1)
for (i in conjunto_bom) {
  resultado_conjunto_bom <- c(resultado_conjunto_bom,
                               triangular(i, 50, 100, 100))
}
plot(conjunto_bom, resultado_conjunto_bom, type = "l")

# PRIMEIRA REGRA
# V aumentando
v1 <- trapezoidal(55, 0, 50, 100, 100)
# S alta
s1 <- triangular(60, 50, 100, 100)
# I bom
i1 <- triangular(85, 50, 100, 100)

# SEGUNDA REGRA
# V aumentando
v2 <- trapezoidal(55, 0, 50, 100, 100)
# S média
s2 <- triangular(60, 0, 50, 100)
# I bom
i2 <- triangular(85, 50, 100, 100)

# TERCEIRA REGRA
# V aumentando
v3 <- trapezoidal(55, 0, 50, 100, 100)
# S baixa
s3 <- triangular(60, 0, 0, 50)
# I bom
i3 <- triangular(85, 50, 100, 100)

# QUARTA REGRA
# V aumentando
v4 <- trapezoidal(55, 0, 50, 100, 100)
# S média
s4 <- triangular(60, 0, 50, 100)
# I ruim
i4 <- triangular(85, 0, 0, 50)

# Conjuntos variável de saída
a_r_leve <- 0
m_r_leve <- 0
b_r_leve <- 50

a_r_media <- 0
m_r_media <- 50
b_r_media <- 100

a_r_forte <- 50
m_r_forte <- 100
b_r_forte <- 100

# INFERENCIA COM MAMDANI
# primeira regra
r1 <- min(v1, s1, i1)

# segunda regra
r2 <- min(v2, s2, i2)

# terceira regra
r3 <- min(v3, s3, i3)

# quarta regra
r4 <- min(v4, s4, i4)

# max dos resultados
r_max <- max(r1, r2, r3, r4)

# INFERENCIA COM LARSEN
# primeira regra
r1l <- prod(v1, s1, i1)

# segunda regra
r2l <- prod(v2, s2, i2)

# terceira regra
r3l <- prod(v3, s3, i3)

# quarta regra
r4l <- prod(v4, s4, i4)

# produto dos resultados
r_prod <- prod(r1, r2, r3, r4)

# DEFUZZIFICANDO
# CENTRO DE MAXIMOS
c_o_m <- ( (r1 * m_r_forte) + (r2 * m_r_media) + (r3 * m_r_leve) + (r4 * m_r_forte) ) / ( r1 + r2 + r3 + r4 )

regra_1 <- c()
vetorValoresMaximos <- c()   # FAzer isso para a agregação, aqui não é preciso
for(i in seq(0, 100)) {
  minimo <- min(trapezoidal(i, 0, 50, 100, 100), triangular(i, 50, 100, 100), triangular(i, 50, 100, 100))
  if(minimo <= 0.2)
    regra_1 <- c(regra_1, minimo)
  else
    regra_1 <- c(regra_1, 0.2)
}

regra_2 <- c()
vetorValoresMaximos <- c()   # FAzer isso para a agregação, aqui não é preciso
for(i in seq(0, 100)) {
  minimo <- min(trapezoidal(i, 0, 50, 100, 100), triangular(i, 0, 50, 100), triangular(i, 50, 100, 100))
  if(minimo <= 0.7)
    regra_2 <- c(regra_2, minimo)
  else
    regra_2 <- c(regra_2, 0.7)
}

resultado <- c()
vetorValoresMaximos <- c()   # FAzer isso para a agregação, aqui não é preciso
for(i in seq(0, 100)) {
  maximo <- max(
    min(trapezoidal(i, 0, 50, 100, 100), triangular(i, 50, 100, 100), triangular(i, 50, 100, 100)),
    min(trapezoidal(i, 0, 50, 100, 100), triangular(i, 0, 50, 100), triangular(i, 50, 100, 100))
  )
  resultado <- c(resultado,  maximo)
}

# TODO: regra_3 e regra_4 dão 0, fazer depois

plot (seq(0,100, 1), resultado_conjunto_aumentando, xlim = c(0,100), type = "l", col = "red")
lines(seq(50,100, 1), resultado_conjunto_media, col = "blue")
lines(seq(50,100, 1), resultado_conjunto_bom, col = "green")
lines(seq(0,100,1), regra_1, col = "orange")

resultado <- c()
vetorValoresMaximos <- c()   # FAzer isso para a agregação, aqui não é preciso
for(i in seq(0, 100)) {
  maximo <- max(regra_1, regra_2)
  resultado <- c(resultado, maximo)
}

plot(seq(0, 100), regra_2, type = "l", col = "orange", xlim = c(0,100))
lines(seq(0, 100), regra_2, col = "red")
lines (seq(0,100, 1), resultado, xlim = c(0,100), col = "green")


# TODO: A resposta começa daqui
resultado <- c()
vetorValoresMaximos <- c()   # FAzer isso para a agregação, aqui não é preciso
for(i in seq(0, 100)) {
  grauR1 <- triangular(i, 50, 100, 100)
  grauR2 <- triangular(i, 0, 50, 100)
  
  if(grauR1 > 0.2)
    grauR1 = 0.2
  if(grauR2 > 0.7)
    grauR2 = 0.7
  
  cat("Grau1: ", grauR1, "\n")
  
  maximo <- max(
    grauR1, grauR2
  )
  if(maximo == 0.7) {
    vetorValoresMaximos <- c(vetorValoresMaximos, i)
  }
  resultado <- c(resultado,  maximo)
}

plot(seq(0, 100), resultado, type = "l", col = "orange", xlim = c(0,100))
