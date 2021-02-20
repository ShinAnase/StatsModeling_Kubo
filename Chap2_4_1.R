
#最尤推定量の個数指定
Mean_Num <-3000
Mean <-numeric(3000)

for(i in 0:Mean_Num-1){
  #ポアソン関数の生成
  r_num <- rpois(50,3.5)
  Mean[i] <- mean(r_num)
}

#最尤推定量の平均のヒストグラム
hist(Mean, breaks=seq(2,5,0.1))

