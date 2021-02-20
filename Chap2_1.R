#種子データ
  load("C:/Users/hfuis/modeling_R/data.RData")
  data
  length(data)
  
  summary(data)
  table(data)
  hist(data, breaks = seq(-0.5, 9.5, 1))
  #hist(data, breaks = seq(0, 10, 1))
  
  var(data) #分散
  sd(data) #標準偏差
  
　  


