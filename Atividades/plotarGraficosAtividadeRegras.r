pdf("graficosRegras.pdf")
# 4 linhas e 5 colunas
par(mfrow = c(4, 5))

# Plota vendas aumentando
plot (seq(0,100, 1), resultado_conjunto_aumentando, xlim = c(0,100), type = "l", col = "blue", xlab = "Aumentando", ylab = "", main = "Vendas")
lines(seq(0:100), rep(1, 101), col = "red", lty=2)

# Plota Servicos Alta
plot (seq(50,100, 1), resultado_conjunto_alta, xlim = c(0,100), type = "l", col = "green", xlab = "Alta", ylab = "", main = "Servicos")
lines(seq(0:100), rep(0.2, 101), col = "red", lty=2)

# Plot informatizacao Bom
plot (seq(50,100, 1), resultado_conjunto_bom, xlim = c(0,100), type = "l", col = "orange", xlab = "Bom", ylab = "", main = "Informatizacao")
lines(seq(0:100), rep(i1, 101), col = "red", lty=2)

# Plota investimento Forte
plot (c(seq(0, 49), 50, 100, 100, 110), c(rep(0, 50), 0, 1, 0, 0), xlim = c(0,100), type = "l", xlab = "Forte", ylab = "", main = "R. Investimento")
lines(seq(0:110), rep(r1, 111), col = "red", lty=2)

# Plota o resultado da regra 1
plot(seq(0,100,1), regra_1, col = "red", type = "l", xlab = "Forte (alfa cut)", ylab = "", main = "Regra 1", ylim = c(0, 1))

############################### Regra 2 ##################################

# Plota vendas aumentando
plot (seq(0,100, 1), resultado_conjunto_aumentando, xlim = c(0,100), type = "l", col = "blue", xlab = "Aumentando", ylab = "", main = "Vendas")
lines(seq(0:100), rep(1, 101), col = "red", lty=2)

# Plota Servicos Media
plot (conjunto_media, resultado_conjunto_media, xlim = c(0,100), type = "l", col = "green", xlab = "Media", ylab = "", main = "Servicos")
lines(conjunto_media, rep(s2, 101), col = "red", lty=2)

# Plot informatizacao Bom
plot (seq(50,100, 1), resultado_conjunto_bom, xlim = c(0,100), type = "l", col = "orange", xlab = "Bom", ylab = "", main = "Informatizacao")
lines(seq(0:100), rep(i1, 101), col = "red", lty=2)

# Plota investimento Media
plot (c(0, 50, 100, 110), c(0, 1, 0, 0), xlim = c(0, 100), type = "l", xlab = "Media", ylab = "", main = "R. Investimento")
lines(seq(0:110), rep(r2, 111), col = "red", lty=2)

# Plota o resultado da regra 2
plot(seq(0,100,1), regra_2, col = "red", type = "l", xlab = "Media (alfa cut)", ylab = "", main = "Regra 2", ylim = c(0, 1))

############################### Regra 3 ##################################

# Plota vendas aumentando
plot (seq(0,100, 1), resultado_conjunto_aumentando, xlim = c(0,100), type = "l", col = "blue", xlab = "Aumentando", ylab = "", main = "Vendas")
lines(seq(0:100), rep(1, 101), col = "red", lty=2)

# Plota Servicos Baixa
plot (conjunto_baixa, resultado_conjunto_baixa, xlim = c(0,100), type = "l", col = "green", xlab = "Baixa", ylab = "", main = "Servicos")
lines(conjunto_baixa, rep(s2, 101), col = "red", lty=2)

# Plot informatizacao Bom
plot (seq(50,100, 1), resultado_conjunto_bom, xlim = c(0,100), type = "l", col = "orange", xlab = "Bom", ylab = "", main = "Informatizacao")
lines(seq(0:100), rep(i1, 101), col = "red", lty=2)

# Plota investimento Leve
plot (c(0, 0, 50, 110), c(0, 1, 0, 0), xlim = c(0, 100), type = "l", xlab = "Leve", ylab = "", main = "R. Investimento")
lines(seq(0:110), rep(r3, 111), col = "red", lty=2)

# Plota o resultado da regra 3
plot(seq(0,100,1), regra_3, col = "red", type = "l", xlab = "Leve (alfa cut)", ylab = "", main = "Regra 3", ylim = c(0, 1))

############################### Regra 4 ##################################

# Plota vendas aumentando
plot (seq(0,100, 1), resultado_conjunto_aumentando, xlim = c(0,100), type = "l", col = "blue", xlab = "Aumentando", ylab = "", main = "Vendas")
lines(seq(0:100), rep(r4, 101), col = "red", lty=2)

# Plota Servicos Media
plot (conjunto_media, resultado_conjunto_media, xlim = c(0,100), type = "l", col = "green", xlab = "Media", ylab = "", main = "Servicos")
lines(conjunto_media, rep(r4, 101), col = "red", lty=2)

# Plot informatizacao Ruim
plot (seq(0,100, 1), resultado_conjunto_ruim, xlim = c(0,100), type = "l", col = "orange", xlab = "Ruim", ylab = "", main = "Informatizacao")
lines(seq(0:100), rep(r4, 101), col = "red", lty=2)

# Plota investimento Forte
plot (c(50, 100, 100, 110), c(0, 1, 0, 0), xlim = c(0, 100), type = "l", xlab = "Forte", ylab = "", main = "R. Investimento")
lines(seq(0:110), rep(r4, 111), col = "red", lty=2)

# Plota o resultado da regra 4
plot(seq(0,100,1), regra_4, col = "red", type = "l", xlab = "Forte (alfa cut)", ylab = "", main = "Regra 4", ylim = c(0, 1))

################################ Fim Regras #####################################

par(mfrow = c(2, 2))

############## Mamdani ####################

plot(seq(0,100,1), resultado, col = "green", type = "l", xlab = "Mamdani", ylab = "", main = "Agregacao", ylim = c(0, 1))
lines(seq(0:110), rep(r_max, 111), col = "red", lty=2)


################ larsen ##########################

# Com Larsen
# r1: 0.14
# r2: 0.56
# r3: 0
# r4: 0

# valorDefuzzificadoSumMax
# 50

# CoA
# 51.48797


plot(seq(0,100,1), resultLarsen, col = "green", type = "l", xlab = "Larsen", ylab = "", main = "Agregacao", ylim = c(0, 1))
lines(seq(0:110), rep(0.56, 111), col = "red", lty=2)

dev.off()
