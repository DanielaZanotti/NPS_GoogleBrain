library(gamm4)
library(knitr)

###########
### GAMM4 with 3 clusters
###########

#gamm with random intercept by breath_id
m1 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(n_change_sign_u_in) +
                       + factor(clust) + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
             data=train_until1,
             random = ~ (1| breath_id))

#gamm with random intercept by breath_id & clust
m2 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + s(n_change_sign_u_in) +
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust) )

#gamm with random intercept by breath_id & clust pearson
m3 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + s(n_change_sign_u_in)+
                       + factor(clust) + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust_pearson) )

#gamm with random intercept by breath_id & clust  & clust pearson
m4 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) +
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust) + (1| clust_pearson) )

summary(m4$gam) ## summary of gam
summary(m4$mer) ## underlying mixed model


#F RATIO 
(27.4+19.06)/19.06

plot(m1$gam)

mods <- list(m1, m2, m3, m4)
names(mods) <- c("m1", "m2", "m3", "m4")

#### residuals gam (uguale per tutti i modelli)
res <- lapply(mods, function(x) data.frame(res = residuals(x$gam), pred = predict(x$gam)))
mod <- rep(names(mods), unlist(lapply(res, nrow)))
res <- do.call("rbind", res)
res$mod <- mod

#GAM term residuals distribution
x11()
par(mfrow=c(2,2))
hist(res[which(res$mod=='m1'), 'res'], xlab = 'Residuals', main = 'Model 1')
hist(res[which(res$mod=='m2'), 'res'], xlab = 'Residuals', main = 'Model 2')
hist(res[which(res$mod=='m3'), 'res'], xlab = 'Residuals', main = 'Model 3')
hist(res[which(res$mod=='m4'), 'res'], xlab = 'Residuals', main = 'Model 4')

#GAM term residuals quantile plot
x11()
par(mfrow=c(2,2))
qqnorm(res[which(res$mod=='m1'), 'res'], main = 'Model 1')
qqline(res[which(res$mod=='m1'), 'res'], col='blue')
qqnorm(res[which(res$mod=='m2'), 'res'], main = 'Model 2')
qqline(res[which(res$mod=='m2'), 'res'], col='blue')
qqnorm(res[which(res$mod=='m3'), 'res'], main = 'Model 3')
qqline(res[which(res$mod=='m3'), 'res'], col='blue')
qqnorm(res[which(res$mod=='m4'), 'res'], main = 'Model 4')
qqline(res[which(res$mod=='m4'), 'res'], col='blue')

#GAM term residuals versus predictions
x11()
par(mfrow=c(2,2))
plot(res[which(res$mod=='m1'), 'pred'], res[which(res$mod=='m1'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 1')
plot(res[which(res$mod=='m3'), 'pred'], res[which(res$mod=='m2'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 2')
plot(res[which(res$mod=='m3'), 'pred'], res[which(res$mod=='m3'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 3')
plot(res[which(res$mod=='m4'), 'pred'], res[which(res$mod=='m4'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 4')

#### residuals random effects
res_re <- lapply(mods, function(x) data.frame(res_re = residuals(x$mer), pred = predict(x$mer)))
mod_re <- rep(names(mods), unlist(lapply(res_re, nrow)))
res_re <- do.call("rbind", res_re)
res_re$mod <- mod_re

#Random effect residuals distribution by model
x11()
par(mfrow=c(2,2))
hist(res_re[which(res_re$mod=='m1'), 'res_re'], xlab = 'Residuals', main = 'Model 1')
hist(res_re[which(res_re$mod=='m2'), 'res_re'], xlab = 'Residuals', main = 'Model 2')
hist(res_re[which(res_re$mod=='m3'), 'res_re'], xlab = 'Residuals', main = 'Model 3')
hist(res_re[which(res_re$mod=='m4'), 'res_re'], xlab = 'Residuals', main = 'Model 4')

#Random effect residuals quantile plot by model
x11()
par(mfrow=c(2,2))
qqnorm(res_re[which(res_re$mod=='m1'), 'res_re'], main = 'Model 1')
qqline(res_re[which(res_re$mod=='m1'), 'res_re'], col='blue')
qqnorm(res_re[which(res_re$mod=='m2'), 'res_re'], main = 'Model 2')
qqline(res_re[which(res_re$mod=='m2'), 'res_re'], col='blue')
qqnorm(res_re[which(res_re$mod=='m3'), 'res_re'], main = 'Model 3')
qqline(res_re[which(res_re$mod=='m3'), 'res_re'], col='blue')
qqnorm(res_re[which(res_re$mod=='m4'), 'res_re'], main = 'Model 4')
qqline(res_re[which(res_re$mod=='m4'), 'res_re'], col='blue')

#Random effect residuals versus predictions by model
x11()
par(mfrow=c(2,2))
plot(res_re[which(res_re$mod=='m3'), 'pred'], res_re[which(res_re$mod=='m1'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 1')
plot(res_re[which(res_re$mod=='m3'), 'pred'], res_re[which(res_re$mod=='m2'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 2')
plot(res_re[which(res_re$mod=='m3'), 'pred'], res_re[which(res_re$mod=='m3'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 3')
plot(res_re[which(res_re$mod=='m4'), 'pred'], res_re[which(res_re$mod=='m4'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 4')

#### AIC and BIC of random effects term
aic <- AIC(m1$mer, m2$mer, m3$mer, m4$mer)
aic$BIC <- BIC(m1$mer, m2$mer, m3$mer, m4$mer)$BIC

kable(aic[order(aic$BIC), ])

###########
### GAMM4 with 10 clusters
###########

#gamm with random intercept by breath_id
m11 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + 
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50') +
              + factor(clust_tanti == 3) + factor(clust_tanti == 8) + factor(clust_tanti == 9) + factor(clust_tanti == 10) 
              + factor(clust_pearson_tanti == 2) + factor(clust_tanti==8)  ,
            data=train_until1,
            random = ~ (1| breath_id))

#gamm with random intercept by breath_id & clust
m22 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + 
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50') +
              + factor(clust_pearson_tanti==2) +factor(clust_pearson_tanti==8) ,
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust_tanti) )

#gamm with random intercept by breath_id & clust pearson
m33 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + 
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50') +
               + factor(clust_tanti==3) +factor(clust_tanti==8) + factor(clust_tanti==9) +factor(clust_tanti==10),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust_pearson_tanti) )

#gamm with random intercept by breath_id & clust & clust pearson
m44 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + 
               + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50') +
               + factor(clust_pearson_tanti== 2),
             data=train_until1,
             random = ~ (1| breath_id) + (1| clust_tanti) + (1| clust_pearson_tanti) )

summary(m44$gam) ## summary of gam
summary(m44$mer) ## underlying mixed model

#F RATIO 
(27.4+19.06)/19.06

plot(m22$gam)

mods <- list(m11, m22, m33, m44)
names(mods) <- c("m11", "m22", "m33", "m44")

#### residuals gam (uguale per tutti i modelli)
res <- lapply(mods, function(x) data.frame(res = residuals(x$gam), pred = predict(x$gam)))
mod <- rep(names(mods), unlist(lapply(res, nrow)))
res <- do.call("rbind", res)
res$mod <- mod

#GAM term residuals distribution
x11()
par(mfrow=c(2,2))
hist(res[which(res$mod=='m11'), 'res'], xlab = 'Residuals', main = 'Model 1')
hist(res[which(res$mod=='m22'), 'res'], xlab = 'Residuals', main = 'Model 2')
hist(res[which(res$mod=='m33'), 'res'], xlab = 'Residuals', main = 'Model 3')
hist(res[which(res$mod=='m44'), 'res'], xlab = 'Residuals', main = 'Model 4')

#GAM term residuals quantile plot
x11()
par(mfrow=c(2,2))
qqnorm(res[which(res$mod=='m11'), 'res'], main = 'Model 1')
qqline(res[which(res$mod=='m11'), 'res'], col='blue')
qqnorm(res[which(res$mod=='m22'), 'res'], main = 'Model 2')
qqline(res[which(res$mod=='m22'), 'res'], col='blue')
qqnorm(res[which(res$mod=='m33'), 'res'], main = 'Model 3')
qqline(res[which(res$mod=='m33'), 'res'], col='blue')
qqnorm(res[which(res$mod=='m44'), 'res'], main = 'Model 4')
qqline(res[which(res$mod=='m44'), 'res'], col='blue')

#GAM term residuals versus predictions
x11()
par(mfrow=c(2,2))
plot(res[which(res$mod=='m11'), 'pred'], res[which(res$mod=='m11'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 1')
plot(res[which(res$mod=='m22'), 'pred'], res[which(res$mod=='m22'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 2')
plot(res[which(res$mod=='m33'), 'pred'], res[which(res$mod=='m33'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 3')
plot(res[which(res$mod=='m44'), 'pred'], res[which(res$mod=='m44'), 'res'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 4')

#### residuals random effects
res_re <- lapply(mods, function(x) data.frame(res_re = residuals(x$mer), pred = predict(x$mer)))
mod_re <- rep(names(mods), unlist(lapply(res_re, nrow)))
res_re <- do.call("rbind", res_re)
res_re$mod <- mod_re

#Random effect residuals distribution by model
x11()
par(mfrow=c(2,2))
hist(res_re[which(res_re$mod=='m11'), 'res_re'], xlab = 'Residuals', main = 'Model 1')
hist(res_re[which(res_re$mod=='m22'), 'res_re'], xlab = 'Residuals', main = 'Model 2')
hist(res_re[which(res_re$mod=='m33'), 'res_re'], xlab = 'Residuals', main = 'Model 3')
hist(res_re[which(res_re$mod=='m44'), 'res_re'], xlab = 'Residuals', main = 'Model 4')

#Random effect residuals quantile plot by model
x11()
par(mfrow=c(2,2))
qqnorm(res_re[which(res_re$mod=='m11'), 'res_re'], main = 'Model 1')
qqline(res_re[which(res_re$mod=='m11'), 'res_re'], col='blue')
qqnorm(res_re[which(res_re$mod=='m22'), 'res_re'], main = 'Model 2')
qqline(res_re[which(res_re$mod=='m22'), 'res_re'], col='blue')
qqnorm(res_re[which(res_re$mod=='m33'), 'res_re'], main = 'Model 3')
qqline(res_re[which(res_re$mod=='m33'), 'res_re'], col='blue')
qqnorm(res_re[which(res_re$mod=='m44'), 'res_re'], main = 'Model 4')
qqline(res_re[which(res_re$mod=='m44'), 'res_re'], col='blue')

#Random effect residuals versus predictions by model
x11()
par(mfrow=c(2,2))
plot(res_re[which(res_re$mod=='m11'), 'pred'], res_re[which(res_re$mod=='m11'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 1')
plot(res_re[which(res_re$mod=='m22'), 'pred'], res_re[which(res_re$mod=='m22'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 2')
plot(res_re[which(res_re$mod=='m33'), 'pred'], res_re[which(res_re$mod=='m33'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 3')
plot(res_re[which(res_re$mod=='m44'), 'pred'], res_re[which(res_re$mod=='m44'), 'res_re'], 
     xlab = 'prediction', ylab = 'res', main = 'Model 4')

### R^2 for fixed effects
summary(m11$gam) #0.785 
summary(m22$gam) #0.747
summary(m33$gam) #0.773 
summary(m44$gam) #0.75 

#### AIC and BIC of random effects term
aic <- AIC(m11$mer, m22$mer, m33$mer, m44$mer)
aic$BIC <- BIC(m11$mer, m22$mer, m33$mer, m44$mer)$BIC

kable(aic[order(aic$BIC), ])


#########
### CONFRONTO TRA TUTTI 
#########

### R^2 for fixed effects
summary(m1$gam)  #0.778
summary(m2$gam)  #0.747
summary(m3$gam)  #0.778
summary(m4$gam)  #0.736
summary(m11$gam) #0.785 
summary(m22$gam) #0.747
summary(m33$gam) #0.773 
summary(m44$gam) #0.75 

#### AIC and BIC of random effects term
aic <- AIC(m1$mer, m2$mer, m3$mer, m4$mer, m11$mer, m22$mer, m33$mer, m44$mer)
aic$BIC <- BIC(m1$mer, m2$mer, m3$mer, m4$mer, m11$mer, m22$mer, m33$mer, m44$mer)$BIC

kable(aic[order(aic$BIC), ])



