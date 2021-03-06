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