library(leaps)
library(bestglm)

cement = read.csv("c:/data/cement.csv")
fit1 = lm(y ~ ., cement)
summary(fit1)
step(fit1,direction="backward")
step(fit1,direction="both")
fit2 = lm(y ~ 1, cement)
summary(fit2)
step(fit2,direction="forward",scope=~x1+x2+x3+x4)
step(fit2,direction="both",scope=~x1+x2+x3+x4)

# Best Subset Regression
fit3=leaps(cement[,1:4], cement[,5], method="Cp", nbest=2, names=names(cement)[1:4])
data.frame(size=fit3$size,Cp=fit3$Cp,fit3$which)

fit4=bestglm(cement,IC="AIC")
fit4
fit4$BestModels