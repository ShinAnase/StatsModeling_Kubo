#種子データ
load("C:/modeling_R/data.RData")
plant.seed <- data

#データ概要
length(plant.seed)
summary(data)
table(data)


#ポワソン分布
y <- c(0:9)

prob <- dpois(y,lambda=3.56)

cbind(y,prob)

#ヒストグラム図と分布図
hist(plant.seed,breaks=seq(-0.5,9.5,1))
lines(y,prob*50)

print(y[5])

