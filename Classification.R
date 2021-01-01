library(pROC)
library(e1071)
library(randomForest)
library(gbm)
library(caret)
library(DMwR)

# Check class balance
table(class_train$logtargamt)
table(class_test$logtargamt)

# to achieve a 3:7 ratio in the training set, we need 10296, or approx 1200% more 1's.
class_train$logtargamt <- factor(class_train$logtargamt)
newData = SMOTE(logtargamt~., class_train, perc.over=1200, perc.under=0)
new_class_train <- rbind(class_train[class_train$logtargamt==0,], newData)
table(new_class_train$logtargamt)

# transform the predictors
new_new_class_train <- new_class_train
new_new_class_train['consistency'] <- new_class_train$frequency/new_class_train$tof
new_new_class_train['log_con'] <- log(new_new_class_train$consistency)
new_new_class_train['log_rec'] <- log(new_class_train$recency)

new_new_class_test <- class_test
new_new_class_test['consistency'] <- class_test$frequency/class_test$tof
new_new_class_test['log_con'] <- log(new_new_class_test$consistency)
new_new_class_test['log_rec'] <- log(class_test$recency)

# Logistic Regression on old training set
logistic = glm(logtargamt~log_con+log_rec, family = binomial, data = new_new_class_train)
logistic_pred = predict(logistic, newdata = new_new_class_test, type = "response")

hist(logistic_pred)

final <- data.frame("predicted"=predict(ls_fit, reg_test2_buyers)*logistic_pred, "observed"=test$logtargamt)
MSE(final[[2]], final[[1]], nrow(final))



# ignore for now
table(class_test$logtargamt, logistic_pred > 0.05)


auc(roc(class_test$logtargamt, logistic_pred))

class_test[class_test$amount_sum > 1000,]

logistic_small = glm(logtargamt~recency+frequency+amount+tof+Fclassics3+
                         Fconthist20+Feconomy21+FGamesRiddles38+Meconomy21+
                         MGamesRiddles38+frequency_orig+amount_orig, 
                     family = binomial, data = class_train)
logistic_small_pred = predict(logistic_small, newdata = class_test, type = "response")
table(class_test$logtargamt, logistic_small_pred > 0.5)
auc(roc(class_test$logtargamt, logistic_small_pred))

# SVM
svm_fit = svm(logtargamt~recency+frequency+amount+tof+Fclassics3+
                  Fconthist20+Feconomy21+FGamesRiddles38+Meconomy21+
                  MGamesRiddles38+frequency_orig+amount_orig, data = class_train)
svm_pred = predict(svm_fit, newdata = class_test, type = "response")
table(class_test$logtargamt, svm_pred > 0.5)
auc(roc(class_test$logtargamt, svm_pred))

# Random Forest
rf = randomForest(factor(logtargamt)~.-Fnonbooks99-Mnonbooks99, data = class_train, ntree = 200)
rf_pred = predict(rf, newdata = class_test, type = "prob")[,2]
table(class_test$logtargamt, rf_pred > 0.2)
auc(roc(class_test$logtargamt, rf_pred))

rf_small = randomForest(factor(logtargamt)~recency+frequency+amount+tof+Fclassics3+
                            Fconthist20+Feconomy21+FGamesRiddles38+Meconomy21+
                            MGamesRiddles38+frequency_orig+amount_orig, 
                        data = class_train, ntree = 200)
rf_small_pred = predict(rf_small, newdata = class_test, type = "prob")[,2]
table(class_test$logtargamt, rf_small_pred > 0.2)
auc(roc(class_test$logtargamt, as.numeric(rf_small_pred)))

feature_imp = importance(rf)
feature_imp[order(feature_imp, decreasing = TRUE),]
