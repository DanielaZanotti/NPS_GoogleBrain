library(gamm4)
library(RLRsim)

############
### GAM
############

# bs = 're' o 'cr'

m <- gam(pressure ~ time_step + s(u_in) + R_C + tot_u_in + max_u_in + u_in_diff2 +
           + s(time_step, by = breath_id) + s(u_in, by = breath_id) + 
           + s(tot_u_in, by = breath_id) +  s(last_u_in, by = breath_id) + s(first_u_in, by = breath_id) +
           + s(max_u_in, by = breath_id) + s(u_in_diff1, by = breath_id) + s(u_in_diff2, by = breath_id) +
           + s(u_in_diff_max, by = breath_id) + s(n_change_sign_u_in, by = breath_id) ,
         family=gaussian, data=train_until1, method = "REML")
summary(m)
#Rank: 112/122
#R-sq.(adj) =  0.816   Deviance explained = 81.8%
#-REML =  25508  Scale est. = 16.526    n = 8940

pr <- predict.gam(m, test_until1)


###########
### GAMM4 
###########

#gamm with random intercept by breath_id
m1 <- gamm4(pressure ~ s(u_in) + tot_u_in + u_in_diff1 + u_in_diff2 + factor(clust) +
                       + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
             data=train_until1,
             random = ~ (1| breath_id))

#gamm with random intercept by clust
m2 <- gamm4(pressure ~ s(u_in) + tot_u_in + u_in_diff1 + u_in_diff2 + factor(clust) +
              + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| clust))

#gamm with random intercept by breath_id & clust
m3 <- gamm4(pressure ~ s(u_in) + tot_u_in + u_in_diff1 + u_in_diff2 + factor(clust) +
                       + factor(R_C == '20_20') + factor(R_C == '20_50') + factor(R_C == '5_20') + factor(R_C == '5_50'),
            data=train_until1,
            random = ~ (1| breath_id) + (1| clust))

summary(m3$gam) ## summary of gam
summary(m3$mer) ## underlying mixed model


#F RATIO 
(27.4+19.06)/19.06

mods <- list(m1, m2, m3)
names(mods) <- c("m1", "m2", "m3")

#### residuals gam (uguale per tutti i modelli)
res <- lapply(mods, function(x) data.frame(res = residuals(x$gam), pred = predict(x$gam)))
mod <- rep(names(mods), unlist(lapply(res, nrow)))
res <- do.call("rbind", res)
res$mod <- mod

x11()
hist(res[which(res$mod=='m3'), 'res'], xlab = '', main = 'GAM term residuals distribution')

x11()
qqnorm(res[which(res$mod=='m3'), 'res'], main = 'GAM term residuals quantile plot')
qqline(res[which(res$mod=='m3'), 'res'], col='blue')

x11()
plot(res[which(res$mod=='m3'), 'pred'], res[which(res$mod=='m3'), 'res'], 
     xlab = 'pred', ylab = 'res', main = 'GAM term residuals versus predictions')

#### residuals random effects
res_re <- lapply(mods, function(x) data.frame(res_re = residuals(x$mer), pred = predict(x$mer)))
mod_re <- rep(names(mods), unlist(lapply(res_re, nrow)))
res_re <- do.call("rbind", res_re)
res_re$mod <- mod_re

x11()
par(mfrow=c(1,3))
hist(res_re[which(res_re$mod=='m1'), 'res_re'], xlab = 'Model 1', main = '')
hist(res_re[which(res_re$mod=='m2'), 'res_re'], xlab = 'Model 2', main = 'Random effect residuals distribution by model')
hist(res_re[which(res_re$mod=='m3'), 'res_re'], xlab = 'Model 3', main = '')

x11()
par(mfrow=c(3,1))
qqnorm(res_re[which(res_re$mod=='m1'), 'res_re'], main = '')
qqline(res_re[which(res_re$mod=='m1'), 'res_re'], col='blue')
qqnorm(res_re[which(res_re$mod=='m2'), 'res_re'], main = 'Random effect residuals quantile plot by model')
qqline(res_re[which(res_re$mod=='m2'), 'res_re'], col='blue')
qqnorm(res_re[which(res_re$mod=='m3'), 'res_re'], main = '')
qqline(res_re[which(res_re$mod=='m3'), 'res_re'], col='blue')

x11()
par(mfrow=c(1,3))
plot(res[which(res$mod=='m3'), 'pred'], res[which(res$mod=='m1'), 'res_re'], 
     xlab = 'pred model 1', ylab = 'res', main = '')
plot(res[which(res$mod=='m3'), 'pred'], res[which(res$mod=='m2'), 'res_re'], 
     xlab = 'pred model 2', ylab = 'res', main = 'Random effect residuals versus predictions by model')
plot(res[which(res$mod=='m3'), 'pred'], res[which(res$mod=='m3'), 'res_re'], 
     xlab = 'pred model 3', ylab = 'res', main = '')

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




