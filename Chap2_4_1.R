
#�Ŗސ���ʂ̌��w��
Mean_Num <-3000
Mean <-numeric(3000)

for(i in 0:Mean_Num-1){
  #�|�A�\���֐��̐���
  r_num <- rpois(50,3.5)
  Mean[i] <- mean(r_num)
}

#�Ŗސ���ʂ̕��ς̃q�X�g�O����
hist(Mean, breaks=seq(2,5,0.1))
