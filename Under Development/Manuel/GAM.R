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
train_until1 = read.table("Data/trainset_GAMM.csv",header=TRUE,sep=",")

#Try with different clusters
clus1 = train_until1[which(train_until1$clust == 3),]

#Create training set
d <- unique(clus1[,'breath_id'])
rn <- sample(d, length(d)*0.75, replace = F)

# sampling
train <- clus1 %>% 
  filter(breath_id %in% rn)

#Create test set
test  <- anti_join(clus1, train, by = 'id')


################################ GAMM  #############################

m1 <- gam(pressure ~ time_step + s(u_in) + s(tot_u_in) +
              + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(u_in_diff4) + s(u_in_diff5) +
                s(u_in_shift1) + s(u_in_shift2) + s(u_in_shift3) + s(u_in_shift4) + s(u_in_shift5) +
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train)

summary(m1) ## summary of gam
plot(m1)


hist(m1$residuals)
qqnorm(m1$residuals)
qqline(m1$residuals)
shapiro.test(m1$residuals)

pred_gam = predict(m1, newdata= test)

score = norm(as.matrix(pred_gam - test$pressure), type="2")
i=24
plot(test$time_step[((i-1)*30 + 1):(i*30)],test$pressure[((i-1)*30 + 1):((i)*30)], type = "l", col ="red",ylim =c(0,30))
lines(test$time_step[((i-1)*30 + 1):(i*30)],test$u_in[((i-1)*30 + 1):(i*30)], col ="blue")
lines(test$time_step[((i-1)*30 + 1):(i*30)],pred_gam[((i-1)*30 + 1):(i*30)])


#### AIC and BIC of random effects term

