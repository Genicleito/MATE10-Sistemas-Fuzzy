library("ggplot2")
eq = function(x){x*x}
ggplot(data.frame(x=c(1, 50)), aes(x=x)) + stat_function(fun=eq, geom="line") + xlab("x") + ylab("y")