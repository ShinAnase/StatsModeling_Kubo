#種子データ
d <- read.csv("C:/Users/hfuis/StatsModeling_Kubo/data3a.csv")
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
#逸脱度
-2*logLik(fit.id)

#フルモデルでの最大対数尤度
sum(log(dpois(d_y, lambda = d_y)))
#最小逸脱度
-2*sum(log(dpois(d_y, lambda = d_y)))

#null model(切片だけのGLM:一般化線形モデル)
fit.null <-glm(formula = y~1, family = poisson, data = d)
summary(fit.null)
#fit後の最大対数尤度
logLik(fit.null)
#逸脱度
-2*logLik(fit.null)


####5章 尤度比検定 ####
#一定モデルとxモデルの逸脱度の差
-2*(logLik(fit.null)-logLik(fit))

#一定モデルでの推定平均種子数
exp(fit.null$coefficients) #大体mean(d$y)と変わらない
mean(d$y)

#PB法(P102)
#一定モデル(mean(d$y)のポアソン分布)を真のモデル(帰無仮説)と仮定して
#100個体のデータを生成
d$y.rnd <- rpois(100, lambda = mean(d$y))
#作ったデータに一定モデル,xモデルを当てはめる
fit1 <- glm(y.rnd~1, data = d, family = poisson)
fit2 <- glm(y.rnd~x, data = d, family = poisson)
fit1$deviance-fit2$deviance #逸脱度の差

#上記ステップを1000回繰り返して検定統計量(逸脱度の差)
#の分布を作る。（ブートストラップ法）
get.dd <- function(d) #データの生成、逸脱度の評価
{
  n.sample <- nrow(d) #データ数
  y.mean <- mean(d$y) #標本平均
  d$y.rnd <- rpois(n.sample, lambda = y.mean)
  fit1 <- glm(y.rnd~1, data = d, family = poisson)
  fit2 <- glm(y.rnd~x, data = d, family = poisson)
  return(fit1$deviance-fit2$deviance) #逸脱度の差を返す
}

pb <- function(d, n.bootstrap)
{
  replicate(n.bootstrap, get.dd(d))
}

#パラメトリックブートストラップ法の実行
source("/Users/hfuis/StatsModeling_Kubo/pd.R") #関数定義ファイルを読み込む
dd12 <- pb(d, n.bootstrap = 1000)
summary(dd12)
#hist(dd12, breaks = seq(-0.5, 19.5, 0.1))
hist(dd12, 100)
abline(v = 4.5, lty=2)

#標本1000個のうちいくつが観測された逸脱度の差(4.5)より大きいか
sum(dd12>=4.5)
41/1000
#優位水準5%の場合の棄却限界
quantile(dd12, 0.95)
#⇒棄却限界が3.997なので逸脱度の差(4.5)により、帰無仮説(一定モデル)は
#　棄却され、xモデルが採択される。
