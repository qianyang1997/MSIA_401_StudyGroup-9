# 3rd try - regression

reg_train2 <- reg_train
reg_train2['consistency'] <- reg_train$frequency/reg_train$tof
reg_train2['amount'] <- reg_train$amount + 1
reg_train2['consistency2'] <- reg_train2$amount/reg_train$tof
reg_train2['log_rec'] <- log(reg_train$recency)
reg_train2['log_freq'] <- log(reg_train$frequency)
reg_train2['log_amt'] <- log(reg_train2$amount)
reg_train2['log_con'] <- log(reg_train$consistency)
reg_train2['log_con2'] <- log(reg_train2$consistency2)
hist(reg_test2$log_con)

reg_test2 <- reg_test
reg_test2['consistency'] <- reg_test$frequency/reg_test$tof
reg_test2['amount'] <- reg_test2['amount'] + 1
reg_test2['consistency2'] <- reg_test2$amount/reg_test$tof
reg_test2['log_con'] <- log(reg_test2$consistency)
reg_test2['log_rec'] <- log(reg_test$recency)
reg_test2['log_con2'] <- log(reg_test2$consistency2)

# check predictor distribution
hist(log(reg_train$recency))
hist(log(reg_train$consistency))

cor(reg_train2[,names(reg_train2)%in%c('log_con', 'log_rec', 'log_amt', 'log_freq')])
ls_fit <- lm(logtargamt~log_rec+log_con, reg_train2)
ls_fit2 <- lm(logtargamt~log_rec+log_con+log_amt, reg_train2)
summary(ls_fit)
summary(ls_fit2)

# MSE of test set with only buyers
MSE <- function(y, fit, n) 1/n * sum((y - fit)^2)
reg_test2_buyers <- reg_test2[reg_test2$logtargamt!=0,]

MSE(reg_test2_buyers$logtargamt, predict(ls_fit, reg_test2_buyers), nrow(reg_test2_buyers))
MSE(reg_test2_buyers$logtargamt, predict(ls_fit2, reg_test2_buyers), nrow(reg_test2_buyers))
