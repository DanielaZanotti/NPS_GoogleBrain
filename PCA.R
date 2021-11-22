### PCA

pc <- princomp(ts[,9:16], scores=T)
pc
summary(pc)

load <- pc$loadings
load

# graphical representation of the loadings of the first 3 principal components
x11()
par(mfcol = c(3,1))
for(i in 1:3) barplot(load[,i], ylim = c(-1, 1), main=paste("PC",i))


x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc, las=2, main='Principal components', ylim=c(0,4.5e4))
barplot(sapply(ts[,9:16],sd)^2, las=2, main='Original Variables', ylim=c(0,4.5e4), ylab='Variances')
plot(cumsum(pc$sd^2)/sum(pc$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(ts[,9:16]),labels=1:ncol(ts[,9:16]),las=2)

scores.wea <- pc$scores
scores.wea




