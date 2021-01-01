library(car)
library(glmnet)
library(MASS)

cement=read.csv("cement.csv")
cement
cor(cement)
plot(cement)
fit=lm(y~x1+x2+x3+x4,data=cement)
summary(fit)
vif(fit)

## Ridge regression using lm.ridge
ridgefit=lm.ridge(y~.,data=cement,lambda=seq(0,5,0.001))
plot( ridgefit, main="Coeffs of Ridge Regression", type="l", 
  xlab=expression(lambda), ylab="Coeff")
grid()
lambda.ridge =seq(0,10,0.1)[which.min(ridgefit$GCV)]
print(lambda.ridge)
betacoef=ridgefit$coef[,which.min(ridgefit$GCV)]
print(betacoef)

##  Ridge regression using glmnet
set.seed(12345)
y=cement$y
x=model.matrix(y~.,cement)
ridgefit=glmnet(x, y, alpha=0,lambda=seq(0,5,0.001))
ridgecv=cv.glmnet(x, y, alpha=0,lambda=seq(0,5,0.001),nfold=3)
lambdaridge=ridgecv$lambda.min
print(lambdaridge)
pdf("c:/users/atamhane/desktop/cement_ridgecv.pdf", height=6, width=6)
plot(ridgecv)
dev.off()
pdf("c:/users/atamhane/desktop/cement_ridge_trace.pdf", height=6, width=6)
plot(ridgefit,xvar="lambda", main="Coeffs of Ridge Regression", type="l", 
    xlab=expression("log_lambda"), ylab="Coeff")
abline(h=0); abline(v=log(ridgecv$lambda.min))
dev.off()
small.lambda.index <- which(ridgecv$lambda == ridgecv$lambda.min)
small.lambda.betas <- coef(ridgecv$glmnet.fit)[,small.lambda.index]
print(small.lambda.betas)

##  Lasso regression using glmnet
set.seed(12345)
lassofit=glmnet(x, y, alpha=1,lambda=seq(0,5,0.001))
lassocv=cv.glmnet(x,y,alpha=1,lambda=seq(0,5,0.001),nfold=3)
lambdalasso=lassocv$lambda.min
print(lambdalasso)
pdf("c:/users/atamhane/desktop/cement_lassocv.pdf", height=6, width=6)
plot(lassocv)
dev.off()
pdf("c:/users/atamhane/desktop/cement_lasso_trace.pdf", height=6, width=6)
plot(lassofit,xvar="lambda",label=TRUE, main="Coeffs of Lasso Regression", type="l", 
    xlab=expression("log_lambda"), ylab="Coeff")
abline(h=0); abline(v=log(lassocv$lambda.min))
dev.off()
small.lambda.index <- which(lassocv$lambda == lassocv$lambda.min)
small.lambda.betas <- coef(lassocv$glmnet.fit)[,small.lambda.index]
print(small.lambda.betas)

####################################################################################

