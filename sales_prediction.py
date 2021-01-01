from sklearn.linear_model import LinearRegression, \
    LogisticRegression, Lasso, Ridge
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import mean_squared_error
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# import csv files
data = pd.read_csv('project3data.csv')
test = pd.read_csv('project3test.csv')
train = pd.read_csv('project3train.csv')

# explore csv files
print(data.head())
print(test.head())
print(train.head())

"""# eda
for columns in data.columns[1:5]:
    # histograms
    plt.figure()
    data[[columns]].hist()
    plt.legend(columns)
    plt.show()
    # box plots
    plt.figure()
    data[[columns]].boxplot()
    plt.legend(columns)
    plt.show()"""

# check for NaNs for each column
data.isna().sum()
train.isna().sum()
test.isna().sum()

# update train and test data frames
train = train.iloc[:, 0:2]
test = test.iloc[:, 0:2]

# update frequency and amount in 'data'
data['new_freq'] = np.sum(data.filter(regex='F.*'), axis=1)
data['new_amt'] = np.sum(data.filter(regex='M.*'), axis=1)

# get rid of institutional customers (total 385 observations)
non_institutional = data.loc[data.new_amt <= 1500, :]

# check the dimensions of data and non_institutional
print(data.shape)
print(non_institutional.shape)

# put first-time customers in a new data set (total 891 observations)
non_first_time = non_institutional.loc[non_institutional.new_amt != 0, :]
print(non_first_time.shape)
first_time = non_institutional.loc[non_institutional.new_amt == 0, :]
print(first_time.shape)

# add a new column 'consistency'
non_first_time['consistency'] = non_first_time.new_freq.divide(non_first_time.tof)

# apply log transformation to right-skewed entities
col = ['new_freq', 'new_amt', 'recency', 'consistency']
for i in col:
    new_col = 'log_' + i
    non_first_time[new_col] = np.log(non_first_time[i])

# merge non_first_time with training and test sets
train_non_first = pd.merge(non_first_time, train, on='id').reset_index(drop=True)
test_non_first = pd.merge(non_first_time, test, on='id').reset_index(drop=True)
train_first = pd.merge(first_time, train, on='id').reset_index(drop=True)
test_first = pd.merge(first_time, test, on='id').reset_index(drop=True)

# check dimensions of train_non_first and test_non_first
print(train_non_first.shape)
print(test_non_first.shape)

# modify train and test sets for classification and regression
class_train = train_non_first.copy()
class_test = test_non_first.copy()
class_train.loc[class_train.logtargamt != 0, :] = 1
class_test.loc[class_test.logtargamt != 0, :] = 1
reg_train = train_non_first.loc[train_non_first.logtargamt != 0]
reg_test = test_non_first.loc[test_non_first.logtargamt != 0]

# linear regression variable setup
features = ['log_new_amt', 'log_recency', 'consistency']
target = ['logtargamt']
X_train = reg_train[features].values
y_train = reg_train[target].values
X_test = reg_test[features].values
y_test = reg_test[target].values

# least square estimate
ls = LinearRegression()
ls.fit(X_train, y_train)
y_pred_ls = ls.predict(X_test)
score_ls = ls.score(X_train, y_train)  # R^2 on training data = 0.0739
mse_ls = mean_squared_error(y_pred_ls, y_test)  # MSE on test data = 0.4325

# ridge & lasso estimate
param_grid = {'alpha': np.arange(0.1, 2, 0.1)}
ridge = Ridge()
lasso = Lasso()

ridge_cv = GridSearchCV(ridge, param_grid, cv=10)
ridge_cv.fit(X_train, y_train)
y_pred_ridge = ridge_cv.predict(X_test)
mse_ridge = mean_squared_error(y_pred_ridge, y_test)  # MSE on test data = 0.4352

lasso_cv = GridSearchCV(lasso, param_grid, cv=10)
lasso_cv.fit(X_train, y_train)
y_pred_lasso = lasso_cv.predict(X_test)
mse_lasso = mean_squared_error(y_pred_lasso, y_test)  # MSE on test data = 0.4827




