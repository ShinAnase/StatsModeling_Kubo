#��q�f�[�^
d <- read.csv("C:/Users/hfuis/modeling_R/data3a.csv")
head(d,10)

#�񂲂Ƃɕ\���B
d_x <- d$x
d_y <- d$y
d_f <- d$f

#�f�[�^�̌^�m�F
class(d)
class(d_x)
class(d_y)
class(d_f)

#summary
summary(d)

#��q���Ƒ̃T�C�Y
plot(d_x,d_y,pch = c(21,19)[d_f])
legend("topleft", legend = c("C","T"), pch = c(21,19))

#�엿�����̗L���Ǝ�q��
plot(d_f,d_y)

#�̃T�C�Yx�݂̂��g�������v���f��
fit <- glm(y ~ x, data = d, family = poisson)
summary(fit)
#fit��̍ő�ΐ��ޓx
logLik(fit)

#�\��
plot(d_x,d_y,pch = c(21,19)[d_f])
xx <- seq(min(d_x), max(d_x), length=100)
lines(xx, exp(1.29172+0.07566*xx), lwd = 2)

#�{�쏈��f�݂̂��g�������v���f��
fit.f <- glm(y~f, data = d, family = poisson)
summary(fit.f)
#fit��̍ő�ΐ��ޓx
logLik(fit.f)

#�̃T�C�Yx,�{�쏈��f�������g�������v���f��
fit.all <- glm(y~x + f, data = d, family = poisson)
summary(fit.all)
#fit��̍ő�ΐ��ޓx
logLik(fit.all)

#�̃T�C�Yx,�{�쏈��f�������g�������v���f��(�P�������N�֐�)
fit.id <- glm(y~x + f, data = d, family = poisson(link = "identity"))
summary(fit.id)
#fit��̍ő�ΐ��ޓx
logLik(fit.id)