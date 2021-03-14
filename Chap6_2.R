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
