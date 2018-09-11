# Função Triangular
fT <- function (x, a, m, b) {
  if (x >= a && x < m) {
    return ( (x - a) / (m - a)  )
  } else if (x >= m && x < b) {
    return ( (b - x) / (b - m) )
  } else {
    return (0)
  } 
}