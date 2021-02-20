#種子データ
d <- read.csv("C:/Users/hfuis/modeling_R/data3a.csv")
head(d,10)

#列ごとに表示。
d_x <- d$x
d_y <- d$y
d_f <- d$f

#データの型確認
class(d)
class(d_x)
class(d_y)
class(d_f)

#summary
summary(d)

#種子数と体サイズ
plot(d_x,d_y,pch = c(21,19)[d_f])
legend("topleft", legend = c("C","T"), pch = c(21,19))

#肥料処理の有無と種子数
plot(d_f,d_y)

#体サイズxのみを使った統計モデル
fit <- glm(y ~ x, data = d, family = poisson)
summary(fit)
#fit後の最大対数尤度
logLik(fit)

#予測
plot(d_x,d_y,pch = c(21,19)[d_f])
xx <- seq(min(d_x), max(d_x), length=100)
lines(xx, exp(1.29172+0.07566*xx), lwd = 2)

#施肥処理fのみを使った統計モデル
fit.f <- glm(y~f, data = d, family = poisson)
summary(fit.f)
#fit後の最大対数尤度
logLik(fit.f)

#体サイズx,施肥処理f両方を使った統計モデル
fit.all <- glm(y~x + f, data = d, family = poisson)
summary(fit.all)
#fit後の最大対数尤度
logLik(fit.all)

#体サイズx,施肥処理f両方を使った統計モデル(恒等リンク関数)
fit.id <- glm(y~x + f, data = d, family = poisson(link = "identity"))
summary(fit.id)
#fit後の最大対数尤度
logLik(fit.id)
