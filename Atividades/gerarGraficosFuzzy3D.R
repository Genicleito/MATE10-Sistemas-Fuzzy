x <- termoTempAlta
y <- termoUmidMedia
f <- function(x, y) { calculaUniaoAxB(x, min(x), (min(x) + max(x)) / 2, max(y), min(y), (min(y) + max(y)) / 2, max(y)) }
z <- outer(x, y, f)
z[is.na(z)] <- 1
op <- par(bg = "white")
persp(x, y, z, theta = 10, phi = 20, expand = 0.5, col = "lightblue")