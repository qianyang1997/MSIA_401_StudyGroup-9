data = read.csv("c:/data/carprices.csv")
names(data)
summary(data)
data$Make <- relevel(data$Make, "Saturn")
data$Type <- relevel(data$Type, "Convertible")
train_indices <- seq(1, nrow(data), by=2)
train <- data[train_indices, ]
test <- data[-train_indices, ]

train_indices <- sample(1:nrow(data), floor(0.5*nrow(data)), replace=F) # 50:50 split
train <- data[train_indices, ]
test <- data[-train_indices, ]

# Use Price as response variable
fit1 = lm(Price ~ I(Mileage/1000)+Liter+Cylinder+Cruise+factor(Make)+factor(Type), data = train)
summary(fit1)
plot(fit1,which=1:2)
plot(fit1,which=c(4,6))


# Use log(Price) as response variable
logfit1 = lm(log10(Price) ~ I(Mileage/1000)+Liter+Cylinder+Cruise+factor(Make)+factor(Type), data = train)
summary(logfit1)
plot(logfit1,which=1:2)
plot(logfit1,which=c(4,6))

# SSE and R^2 for test data with train model
SSE_test <- sum((test$Price - predict(fit1, test))^2)
SST_test <- sum((test$Price - mean(test$Price))^2)
R2_test <- 1 - SSE_test/SST_test
SSE_test
R2_test

# SSE and R^2 for test data with logfit train model
SSE_logfit1_test <- sum((test$Price - exp(predict(logfit1, test)))^2)
SST_test <- sum((test$Price - mean(test$Price))^2)
R2_logfit1_test <- 1 - SSE_logfit1_test/SST_test
SSE_logfit1_test 
R2_logfit1_test



# Cross-Validation SSE
n_folds <- 5
CV_indices <- as.list(rep(NA, n_folds)) # a list with n_folds empty elements
shuffled_indices <- sample(1:nrow(data))
for(i in 1:n_folds){
	CV_indices[[i]] <- shuffled_indices[(floor(nrow(data)*(i-1)/n_folds)+1):floor(nrow(data)*i/n_folds)]
}
SSE_fit1_CV <- 0
for(i in 1:n_folds){
	fit1_CV <- lm(Price ~ I(Mileage/1000)+Liter+factor(Make)+factor(Type), data = data[-CV_indices[[i]],])
	SSE_fit1_CV <- SSE_fit1_CV + sum((predict(fit1_CV, data[CV_indices[[i]],]) - data$Price[CV_indices[[i]]])^2)
}
SST_CV <- sum((data$Price-mean(data$Price))^2)
R2_fit1_CV <- 1 - SSE_fit1_CV1/SST_CV
R2_fit1_CV 

# Cross-Validation SSE for logfit
SSE_logfit1_CV <- 0
for(i in 1:n_folds){
	logfit1_CV <- lm(log(Price) ~ I(Mileage/1000)+Liter+factor(Make)+factor(Type), data = data[-CV_indices[[i]],])
	SSE_logfit1_CV <- SSE_logfit1_CV + sum((exp(predict(logfit1_CV, data[CV_indices[[i]],])) - data$Price[CV_indices[[i]]])^2)
}
R2_logfit1_CV <- 1 - SSE_logfit1_CV/SST_CV
R2_logfit1_CV

logfit2 = lm(log10(Price) ~ I(Mileage/1000)+Liter+Cylinder+Cruise+factor(Make)+factor(Type), data = train)
summary(logfit1)

