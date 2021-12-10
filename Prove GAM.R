library(gamm4)
library(RLRsim)

#fgamm <- gamm(pressure ~ s(u_in, bs="cr") + s(time_step, bs="cr"), random = list(breath_id=~1), data = ts)


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

#+ last_u_in + first_u_in + max_u_in + u_in_diff1 + 
#+ u_in_diff2 + u_in_diff_max + n_change_sign_u_in 

lin <- lm(pressure ~  R + C + tot_u_in + u_in_diff1 + u_in_diff2 + u_in_diff_max,
          data=train_until1)

fm1 <- gamm4(pressure ~ s(u_in) + R + C + tot_u_in + u_in_diff1 + u_in_diff2 + u_in_diff_max,
             data=train_until1,
             random = ~ (1 | breath_id))

summary(fm1$gam) ## summary of gam
summary(fm1$mer) ## underlying mixed model

exactLRT(fm1, lin)

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




