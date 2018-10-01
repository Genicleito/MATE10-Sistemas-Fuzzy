# Exemplo 2.9. Consideramos os subconjuntos fuzzy:
# A = 0.4/x1 + 1.0/x2 + 0.6 /x3
# e
# B = 0.8/y1 + 0.4/y2

A <- c(0.4, 1, 0.6)
B <- c(0.8, 0.4)

lukasiewicz <- function(A, B) {
  R <- c()
  for (i in seq(1, length(A))) {
    for(j in seq(1, length(B))) 
      R <- c(R, min(1 - A[i] + B[j], 1))
  }
  return (R)
}

lukasiewicz(A, B)