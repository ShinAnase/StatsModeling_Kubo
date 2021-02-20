y <- 0:9
prob <- dpois(y, lambda =3.56)
cbind(y, prob)
#plot(y, prob, type = "b", lty = 2)

load("C:/Users/hfuis/modeling_R/data.RData")
hist(data, breaks = seq(-0.5, 9.5, 1))
lines(y, 50*prob)

load("C:/Users/hfuis/modeling_R/data.RData")
logL <- function(m) sum(dpois(data, m ,log=TRUE))
lambda <-seq(1, 5, 0.1)
plot(lambda, sapply(lambda, logL), type="l")
