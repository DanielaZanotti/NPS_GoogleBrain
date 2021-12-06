library(gamm4)

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
# R-sq.(adj) =  0.811   Deviance explained = 81.3%
# -REML =  25293  Scale est. = 15.721    n = 8940


###########
### GAMM4 
###########

fm<-gamm4(pressure ~ time_step + s(u_in) + R_C + tot_u_in + max_u_in + u_in_diff2 ,
           data=train_until1,
           random = ~ (time_step + u_in + u_out + tot_u_in + last_u_in + max_u_in | breath_id))

fm1<-gamm4(pressure ~ time_step + s(u_in) + R_C + tot_u_in + max_u_in + u_in_diff2 ,
           data=train_until1,
           random = ~ (time_step + u_in + u_out + tot_u_in + max_u_in | breath_id))

summary(fm$gam) ## summary of gam
summary(fm$mer) ## underlying mixed model

anova(fm$mer, fm1$mer)   #fm1 meglio 



