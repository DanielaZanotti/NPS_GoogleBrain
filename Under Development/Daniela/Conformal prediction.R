################################ LIBRARIES ######################
library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)
library(gamm4)
library(knitr)
library(mgcv)
library(conformalInference)

################################ CONFORMAL PREDICTION ######################

n=length(unique(train$breath_id))
pres_row=c()
for(i in unique(test$breath_id)){
  p <- test[which(test$breath_id==i), 'pressure']
  pres_row <- rbind(pres_row, p)
}
pres_row_train=c()
for(i in unique(train$breath_id)){
  p <- train[which(train$breath_id==i), 'pressure']
  pres_row_train <- rbind(pres_row_train, p)
}

pred_gam <- matrix(pred_gam, ncol = 30 , byrow = TRUE)

alpha = 0.1


res = abs(pres_row - pred_gam)
ncm=apply(res,2,max)
ncm_sort=c(sort(ncm))
m <- min(n, 30)
d=ncm_sort[ceiling((m/2 + 1)*(1-alpha))]


matplot(cbind(mu,mu+d,mu-d),type='l')

i = 1
plot(new_obs$time_step[((i-1)*30 + 1):(i*30)], pred_gam[((i-1)*30 + 1):(i*30)], type = "l", col ="red", ylim = c(-30, 60))
lines(new_obs$time_step[((i-1)*30 + 1):(i*30)], pred_gam[((i-1)*30 + 1):(i*30)]+d, type='l')
lines(new_obs$time_step[((i-1)*30 + 1):(i*30)], pred_gam[((i-1)*30 + 1):(i*30)]-d, type='l')
#lines(new_obs$time_step[((i-1)*30 + 1):(i*30)],pred_gam[((i-1)*30 + 1):(i*30)], col='blue')


#scale the amplitude
res=abs(pres_row - pred_gam)
ncm = apply(res,2,max)
ncm_sort=c(sort(ncm))
m = min(n, 30)
k = ncm_sort[ceiling((m/2 + 1)*(1-alpha))]
x = seq(0, 1, by=1/29)
line_integral = function(x, y) {
  dx <- diff(x)
  end <- length(y)
  my <- (y[1:(end - 1)] + y[2:end]) / 2
  sum(dx *my)
} 
den = line_integral(x, ncm)
s = ncm/den

matplot(cbind(mu,mu+(d*S),mu-(d*S)),type='l')

matplot(cbind(mu,mu+(d*SS),mu-(d*SS)),type='l')

i = 1
plot(new_obs$time_step[((i-1)*30 + 1):(i*30)], new_obs$pressure[((i-1)*30 + 1):(i*30)], type = "l", col ="red", ylim = c(0,40))
lines(new_obs$time_step[((i-1)*30 + 1):(i*30)], new_obs$pressure[((i-1)*30 + 1):(i*30)]+(k*s), type='l')
lines(new_obs$time_step[((i-1)*30 + 1):(i*30)], new_obs$pressure[((i-1)*30 + 1):(i*30)]-(k*s), type='l')










