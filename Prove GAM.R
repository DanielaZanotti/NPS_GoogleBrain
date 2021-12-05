library(gamm4)

#fgamm <- gamm(pressure ~ s(u_in, bs="cr") + s(time_step, bs="cr"), random = list(breath_id=~1), data = ts)


#m <- gam(pressure ~  max_u_in + tot_u_in + R_C + s(u_in, bs = 're') + s(u_in, by = breath_id) + s(time_step, by = breath_id) +
#           + s(tot_u_in, by = breath_id) + s(max_u_in, by = breath_id),
#         family=gaussian, data=train_until1, method = "REML")
#summary(m)
#R_adj = 0.714   deviance explained = 71.4%
#plot(m)


fm1<-gamm4(pressure ~ max_u_in + tot_u_in + R_C + s(u_in) ,data=train_until1,
           random = ~ (u_in + time_step + tot_u_in + max_u_in | breath_id))

summary(fm1$gam) ## summary of gam
summary(fm1$mer) ## underlying mixed model



