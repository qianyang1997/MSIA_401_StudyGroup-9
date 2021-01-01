library(ggplot2)

## Read datasets
data = read.csv("Project3data.csv")
train = read.csv("Project3train.csv")
test = read.csv("Project3test.csv")

# Remove extra columns in the csv files
train$X = NULL
test$X = NULL

## Aggregate frequency and amount
# Get the column names
columns = colnames(data)
F_columns = columns[grepl("^F", columns)]
M_columns = columns[grepl("^M", columns)]

# Sum Fxx and Mxx variables
frequency_sum = rowSums(data[,F_columns])
amount_sum = rowSums(data[,M_columns])
sum(data$frequency != frequency_sum)  # 29881 rows don't match
sum(data$amount != amount_sum)  # 27082 rows don't match

# Update data
data$frequency_orig = data$frequency
data$amount_orig = data$amount
data$frequency = frequency_sum
data$amount = amount_sum

# Histograms of full data set
hist(data$recency)
hist(data$frequency, breaks=200, xlim=range(0,200))
hist(data$amount, breaks=200, xlim=range(0,1500))
hist(data$tof)

## Institutional customers
# The original distribution of amount has a very long right tail
ggplot(data, aes(x = "", y = amount)) + 
    geom_boxplot(fill = "#0c4c8a") +
    labs(x = "amount", y = "") +
    theme_minimal()

# Remove approximately 1% of the large observations
nrow(data[data$amount > 1500,])/nrow(data)
data = data[data$amount <= 1500,]

# Final boxplots for frequency and amount
ggplot(data, aes(x = "", y = frequency)) + 
    geom_boxplot(fill = "#0c4c8a") +
    labs(x = "frequency", y = "") +
    theme_minimal()

ggplot(data, aes(x = "", y = amount)) + 
    geom_boxplot(fill = "#0c4c8a") +
    labs(x = "amount", y = "") +
    theme_minimal()

## Join datasets
train = merge(train, data, by = "id")
test = merge(test, data, by = "id")

## First-time customers
nrow(train[train$frequency == 0,])  # 271 in the training set
nrow(test[test$frequency == 0,])  # 87 in the test set

# Update train and test by separating first-time customers
first_time_train = train[train$frequency == 0,]
train = train[train$frequency != 0,]
first_time_test = test[test$frequency == 0,]
test = test[test$frequency != 0,]

## EDA on training set
# Histogram
hist(train[train$logtargamt!=0, "logtargamt"])

# QQ plot
par(cex=0.7); qqnorm(full_data[full_data$logtargamt!=0, "logtargamt"])
abline(a=mean(full_data[full_data$logtargamt!=0, "logtargamt"]), 
       b=sd(full_data[full_data$logtargamt!=0, "logtargamt"]), 
       col="red")
# logtargamt (omitting the observations with value 0) is normally distributed

# Find correlation among variables
cor(train[c(2:6, ncol(train)-1, ncol(train))])
# frequency and amount are highly correlated (0.90)
# original frequency and amount are highly correlated (0.79)

## Develop training and test sets for classification and regression
# Classification (converting logtargamt greater than 0 to 1)
class_train = train
class_train$logtargamt = as.integer(class_train$logtargamt > 0)
class_test = test
class_test$logtargamt = as.integer(class_test$logtargamt > 0)

# Regression (removing logtargamt that equals 0)
reg_train = train[train$logtargamt != 0,]
reg_test = test
