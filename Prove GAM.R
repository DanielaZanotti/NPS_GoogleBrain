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

#gamm with random intercept by clust
m2 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3)
                       + s(area) + s(n_change_sign_u_in) +
                       + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| clust))

#gamm with random intercept by breath_id & clust
m3 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + s(n_change_sign_u_in) +
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust) )

#gamm with random intercept by breath_id & clust pearson
m4 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + s(n_change_sign_u_in)+
                       + factor(clust) + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust_pearson) )

#gamm with random intercept by breath_id & clust  & clust pearson
m5 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + s(area) +
              + factor(clust) + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust) + (1| clust_pearson) )

summary(m1$gam) ## summary of gam
summary(m1$mer) ## underlying mixed model


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

####
# plot
####

#####
ts = data.frame()
for(i in unique(train_until1$breath_id))
{
  x1 = train_until1[which(train_until1$breath_id==i), ]
  l = dim(x1)[1]
  ts = rbind(ts, x1[l,])
}

ts_test = data.frame()
for(i in unique(test_until1$breath_id))
{
  x1 = test_until1[which(test_until1$breath_id==i), ]
  l = dim(x1)[1]
  ts_test = rbind(ts_test, x1[l,])
}
#####

#time
time_test = data.frame()
for(i in unique(test_until1$breath_id)  ){
  x1=test_until1[which(test_until1$breath_id==i),'time_step']
  time_test = rbind(time_test, t(x1))
}

# true pressure
true_press_test = data.frame()
for(i in unique(test_until1$breath_id) ){
  x2=test_until1[which(test_until1$breath_id==i), 'pressure']
  true_press_test = rbind(true_press_test, t(x2))
}

# predicted pressure
pred_press_test = data.frame()
for(i in unique(test_until1$breath_id) ){
  idx = which(test_until1$breath_id==i)
  x3 = pr[idx]
  pred_press_test = rbind(pred_press_test, t(unname(x3)))
}

times = as.matrix(time_test)
true_press = as.matrix(true_press_test)
pred_press = as.matrix(pred_press_test)

x11()
par(mfrow=c(2,1))
matplot(t(times), t(true_press), type='l', xlab='time step', ylab='pressure', col='black')
matplot(t(times), t(pred_press), type='l', xlab='time step', ylab='pressure', col='red')


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

#gamm with random intercept by clust
m22 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3)
              + s(area) + s(n_change_sign_u_in) +
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50')
              + factor(clust_pearson_tanti==2) + factor(clust_pearson_tanti==4) + 
              + factor(clust_pearson_tanti==5) + factor(clust_pearson_tanti==8) + factor(clust_pearson_tanti==9),
            data=train_until1,
            random = ~ (1| clust_tanti))

#gamm with random intercept by breath_id & clust
m33 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + 
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50') +
              + factor(clust_pearson_tanti==2) +factor(clust_pearson_tanti==8) ,
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust_tanti) )

#gamm with random intercept by breath_id & clust pearson
m44 <- gamm4(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(u_in_diff1) +s(u_in_diff2) + s(u_in_diff3) + 
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50') +
               + factor(clust_tanti==3) +factor(clust_tanti==8) +
               + factor(clust_tanti==9) +factor(clust_tanti==10),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust_pearson_tanti) )

summary(m33$gam) ## summary of gam
summary(m44$mer) ## underlying mixed model


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


#########
### CONFRONTO TRA TUTTI 
#########

mods <- list(m1, m2, m3, m4, m11, m22, m33, m44)
names(mods) <- c("m1", "m2", "m3", "m4", "m11", "m22", "m33", "m44")

#### residuals gam (uguale per tutti i modelli)
restutti <- lapply(mods, function(x) data.frame(res = residuals(x$gam), pred = predict(x$gam)))
modtutti <- rep(names(mods), unlist(lapply(restutti, nrow)))
restutti <- do.call("rbind", restutti)
restutti$mod <- modtutti

#GAM term residuals distribution
x11()
par(mfrow=c(2,4))
hist(restutti[which(restutti$mod=='m1'), 'res'], xlab = 'Residuals', main = 'Model 1')
hist(restutti[which(restutti$mod=='m2'), 'res'], xlab = 'Residuals', main = 'Model 2')
hist(restutti[which(restutti$mod=='m3'), 'res'], xlab = 'Residuals', main = 'Model 3')
hist(restutti[which(restutti$mod=='m4'), 'res'], xlab = 'Residuals', main = 'Model 4')
hist(restutti[which(restutti$mod=='m11'), 'res'], xlab = 'Residuals', main = 'Model 1')
hist(restutti[which(restutti$mod=='m22'), 'res'], xlab = 'Residuals', main = 'Model 2')
hist(restutti[which(restutti$mod=='m33'), 'res'], xlab = 'Residuals', main = 'Model 3')
hist(restutti[which(restutti$mod=='m44'), 'res'], xlab = 'Residuals', main = 'Model 4')

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
aic <- AIC(m1$mer, m2$mer, m3$mer, m4$mer, m11$mer, m22$mer, m33$mer, m44$mer)
aic$BIC <- BIC(m1$mer, m2$mer, m3$mer, m4$mer, m11$mer, m22$mer, m33$mer, m44$mer)$BIC

kable(aic[order(aic$BIC), ])





