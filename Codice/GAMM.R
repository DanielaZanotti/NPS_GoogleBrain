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

m1 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) +  n_change_sign_u_in + s(area) +
              + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(u_in_diff4) +
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id))

summary(m3$gam) ## summary of gam
summary(m1$mer) ## underlying mixed model
plot(m1$gam)

m3 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) +  n_change_sign_u_in + s(area) +
              + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(u_in_diff4) +
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50') +
                +factor(clust==8) + factor(clust==24) + factor(clust==25) ,
            data=train_until1,
            random = ~ (1| breath_id))

m2 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + n_change_sign_u_in + s(area) +
              + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(u_in_diff4) + 
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust) )

summary(m2$gam) ## summary of gam
summary(m2$mer) ## underlying mixed model
plot(m2$gam)


mods <- list(m1, m2)
names(mods) <- c("m1", "m2")

#### residuals gam (uguale per tutti i modelli)
res <- lapply(mods, function(x) data.frame(res = residuals(x$gam), pred = predict(x$gam)))
mod <- rep(names(mods), unlist(lapply(res, nrow)))
res <- do.call("rbind", res)
res$mod <- mod

#GAM term residuals distribution
x11()
par(mfrow=c(1,1))
hist(res[which(res$mod=='m1'), 'res'], xlab = 'Residuals', main = 'Model 1')
hist(res[which(res$mod=='m2'), 'res'], xlab = 'Residuals', main = 'Model 2')


#GAM term residuals quantile plot
x11()
par(mfrow=c(1,1))
qqnorm(res[which(res$mod=='m1'), 'res'], main = 'Model 1')
qqline(res[which(res$mod=='m1'), 'res'], col='blue')
qqnorm(res[which(res$mod=='m2'), 'res'], main = 'Model 2')
qqline(res[which(res$mod=='m2'), 'res'], col='blue')

#GAM term residuals versus predictions
x11()
par(mfrow=c(1,1))
plot(res[which(res$mod=='m1'), 'pred'], res[which(res$mod=='m1'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 1')
plot(res[which(res$mod=='m2'), 'pred'], res[which(res$mod=='m2'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 2')

#### residuals random effects
res_re <- lapply(mods, function(x) data.frame(res_re = residuals(x$mer), pred = predict(x$mer)))
mod_re <- rep(names(mods), unlist(lapply(res_re, nrow)))
res_re <- do.call("rbind", res_re)
res_re$mod <- mod_re

#Random effect residuals distribution by model
x11()
par(mfrow=c(1,2))
hist(res_re[which(res_re$mod=='m1'), 'res_re'], xlab = 'Residuals', main = 'Model 1')
hist(res_re[which(res_re$mod=='m2'), 'res_re'], xlab = 'Residuals', main = 'Model 2')

#Random effect residuals quantile plot by model
x11()
par(mfrow=c(1,2))
qqnorm(res_re[which(res_re$mod=='m1'), 'res_re'], main = 'Model 1')
qqline(res_re[which(res_re$mod=='m1'), 'res_re'], col='blue')
qqnorm(res_re[which(res_re$mod=='m2'), 'res_re'], main = 'Model 2')
qqline(res_re[which(res_re$mod=='m2'), 'res_re'], col='blue')

#Random effect residuals versus predictions by model
x11()
par(mfrow=c(1,2))
plot(res_re[which(res_re$mod=='m1'), 'pred'], res_re[which(res_re$mod=='m1'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 1')
plot(res_re[which(res_re$mod=='m2'), 'pred'], res_re[which(res_re$mod=='m2'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 2')


#### AIC and BIC of random effects term
aic <- AIC(m1$mer, m2$mer)
aic$BIC <- BIC(m1$mer, m2$mer)$BIC

kable(aic[order(aic$BIC), ])
