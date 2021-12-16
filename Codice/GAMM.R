################################ LIBRARIES ######################
library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)
library(gamm4)
library(knitr)

############################### LOAD DATA ########################
train_until1 = read.table("Data/trainset_GAMM.csv",header=TRUE,sep=",")

################################ GAMM  #############################

m1 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(n_change_sign_u_in) + s(area)
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id))

summary(m1$gam) ## summary of gam
summary(m1$mer) ## underlying mixed model
plot(m1$gam)



