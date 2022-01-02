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

############################### LOAD DATA ########################
train_until1 = read.table("Data/trainset_GAMM_full3.csv",header=TRUE,sep=",")

#Try with different clusters
clus1 = train_until1[which(train_until1$clust == 9),]

#Plot

func_data1=data.frame()
for(i in unique(clus1$breath_id)  ){
  x1=clus1[which(clus1$breath_id==i),'time_step']
  func_data1 = rbind(func_data1, t(x1))
}

func_data4=data.frame()
for(i in unique(clus1$breath_id) ){
  x4=clus1[which(clus1$breath_id==i), 'pressure']
  func_data4 = rbind(func_data4, t(x4))
}

func_data2=data.frame()
for(i in unique(clus1$breath_id) ){
  x2=clus1[which(clus1$breath_id==i), 'u_in']
  func_data2 = rbind(func_data2, t(x2))
}

times = as.matrix(func_data1)
pr = as.matrix(func_data4)
uin = as.matrix(func_data2)

matplot(t(times),t(uin), type='l', xlab='x', ylab='orig.func')
matplot(t(times),t(pr), type='l', xlab='x', ylab='orig.func')


#Create training set
d <- unique(clus1[,'breath_id'])
rn <- sample(d, length(d)*0.75, replace = F)

# sampling
train <- clus1 %>% 
  filter(breath_id %in% rn)

#Create test set
test  <- anti_join(clus1, train, by = 'id')

# New obs
d2 <- unique(test[,'breath_id'])
rn2 <- sample(d2, 1, replace = F)
new_obs <- test %>% 
  filter(breath_id %in% rn2)
test  <- anti_join(test, new_obs, by = 'id')

################################ GAM  #############################

m1 <- gam(pressure ~ time_step + s(u_in) + s(tot_u_in) +
                 + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(u_in_diff4) + s(u_in_diff5) +
                 + s(u_in_shift1) + s(u_in_shift2) + s(u_in_shift3) + 
                 + s(u_in_shift4) + s(u_in_shift5) +
                 + factor(R_C),
           data=train)

m2 <- gam(pressure ~ time_step + max_u_in + s(u_in) + s(tot_u_in) +
            + (last_u_in) + s(n_change_sign_u_in) + s(area) +
            + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(u_in_diff4) + s(u_in_diff5) +
            + s(u_in_shift1) + s(u_in_shift2) + s(u_in_shift3) + s(u_in_shift4) + s(u_in_shift5) +
            + factor(R_C),
          data=train)

summary(m1) ## summary of gam
plot(m1, ylim = c(-40, 40))


hist(m1$residuals)
qqnorm(m1$residuals)
qqline(m1$residuals)
shapiro.test(m1$residuals)

pred_gam = predict(m1, newdata= test)

score = norm(as.matrix(pred_gam - test$pressure), type="2")

i=5
plot(test$time_step[((i-1)*30 + 1):(i*30)],test$pressure[((i-1)*30 + 1):((i)*30)], type = "l", col ="black",ylim =c(-20,100))
lines(test$time_step[((i-1)*30 + 1):(i*30)],test$u_in[((i-1)*30 + 1):(i*30)], col ="blue")
lines(test$time_step[((i-1)*30 + 1):(i*30)],pred_gam[i,], col="red")

lines(test$time_step[((i-1)*30 + 1):(i*30)], test$pressure[((i-1)*30 + 1):(i*30)]+(k*s), type='l')
lines(test$time_step[((i-1)*30 + 1):(i*30)], test$pressure[((i-1)*30 + 1):(i*30)]-(k*s), type='l')



