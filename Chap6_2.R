#種子データ
d <- read.csv("C:/Users/hfuis/StatsModeling_Kubo/data4a.csv")
head(d,10)
summary(d)

#列ごとに表示。
d_x <- d$x
d_y <- d$y
d_f <- d$f

#種子数と体サイズ
plot(d_x,d_y,pch = c(21,19)[d_f])
legend("topleft", legend = c("C","T"), pch = c(21,19))

#肥料処理の有無と種子数
plot(d_f,d_y)

#二項分布
x <- 0:8
y_1 <- dbinom(x, 8, 0.1)
y_3 <- dbinom(x, 8, 0.3)
y_8 <- dbinom(x, 8, 0.8)
plot(x, y_1, xlim=c(0,8),type="b",  ylim = c(0, 0.5), xlab="", ylab="", pch = 15)
par(new=T)
plot(x, y_3, xlim=c(0,8),type="b",  ylim = c(0, 0.5), xlab="", ylab="", pch = 1)
par(new=T)
plot(x, y_8, xlim=c(0,8),type="b",  ylim = c(0, 0.5), xlab="y_i", ylab="p", pch = 8)

#ロジスティック関数
logistic <- function(z) 1/(1+ exp(-z))
z <- seq(-6, 6, 0.1)
plot(z, logistic(z), type = "l")
#線形予測子を変化させる
logistic_ <- function(x, b1, b2) 1/(1+ exp(-(b1+b2*x)))
x <- seq(-6, 6, 0.1)
#プロット(平行移動：切片を変更)
b1 <- 0
b2 <- 1
plot(x, logistic_(x, b1, b2), type = "l",col = 1, ylim = c(0, 1))
par(new=T)
b1 <- 1
plot(x, logistic_(x, b1, b2), type = "l", col = 2, ylim = c(0, 1))
par(new=T)
b1 <- -3
plot(x, logistic_(x, b1, b2), type = "l", col = 3, ylim = c(0, 1))
#プロット(xの係数を変更)
b2 <- 1
b1 <- 0
plot(x, logistic_(x, b1, b2), type = "l",col = 1, ylim = c(0, 1))
par(new=T)
b2 <- 4
plot(x, logistic_(x, b1, b2), type = "l", col = 2, ylim = c(0, 1))
par(new=T)
b2 <- -1
plot(x, logistic_(x, b1, b2), type = "l", col = 3, ylim = c(0, 1))

#二項分布を使った一般線形モデル(GLM)
glm(cbind(y, N-y)~x+f, data = d, family = binomial)
