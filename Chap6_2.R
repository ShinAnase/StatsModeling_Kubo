#��q�f�[�^
d <- read.csv("C:/Users/hfuis/StatsModeling_Kubo/data4a.csv")
head(d,10)
summary(d)

#�񂲂Ƃɕ\���B
d_x <- d$x
d_y <- d$y
d_f <- d$f

#��q���Ƒ̃T�C�Y
plot(d_x,d_y,pch = c(21,19)[d_f])
legend("topleft", legend = c("C","T"), pch = c(21,19))

#�엿�����̗L���Ǝ�q��
plot(d_f,d_y)