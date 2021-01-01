import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from imblearn.over_sampling import SMOTE
from sklearn.linear_model import LogisticRegression, LinearRegression, ElasticNetCV
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import GridSearchCV
from sklearn.feature_selection import RFECV

# read csv
data = pd.read_csv('project3data.csv')
train = pd.read_csv('project3train.csv')
test = pd.read_csv('project3test.csv')
files = [data, train, test]

# look at first few rows
for file in files:
    print(file.head())
    print(file.columns)

# look at missing values
for file in files:
    print(np.sum(file.isna().sum()))

# clean data - fill NAs
train = train.iloc[:, 0:2]
test = test.iloc[:, 0:2]

# clean data - update frequency and amount columns
data['freq_new'] = data[data.columns[data.columns.str.contains('^F')]].apply(sum, axis=1)
data['amt_new'] = data[data.columns[data.columns.str.contains('^M')]].apply(sum, axis=1)

# clean data - set aside first time customers
data_non_first_time = data.loc[data.amt_new != 0, :]
data_first_time = data.loc[data.amt_new == 0, :]

# clean data - set aside institutional customers
data_non_first_time.hist('amt_new', range=[0, 2000])
data_non_first_time.boxplot('amt_new')
plt.show()
data_non_first_time.amt_new.quantile(0.995)  # around $2000
data_normal_customers = data_non_first_time.loc[data_non_first_time.amt_new <= 2000, :]

# EDA
data_normal_customers[['amount', 'frequency', 'amt_new', 'freq_new', 'tof', 'recency']].corr()
plt.figure()
data_normal_customers.hist(['amount', 'frequency', 'amt_new', 'freq_new', 'tof', 'recency'])
plt.show()

# update train and test sets
train_clean = pd.merge(train, data_normal_customers, how='inner', on='id')
test_clean = pd.merge(test, data_normal_customers, how='inner', on='id')

# tweak train and test sets for regression
train_reg = train_clean[train_clean.logtargamt != 0]
test_reg = test_clean.copy()

# tweak train and test sets for classification
train_cla = train_clean.copy()
train_cla.loc[train_cla.logtargamt != 0, 'logtargamt'] = 1
test_cla = test_clean.copy()
test_cla.loc[test_cla.logtargamt != 0, 'logtargamt'] = 1

# check classification class balance
print(train_cla.value_counts(subset='logtargamt'))
print(test_cla.value_counts(subset='logtargamt'))
print(train_cla.value_counts('logtargamt')[0] / train_cla.value_counts('logtargamt')[1])  # 29
print(test_cla.value_counts('logtargamt')[0] / test_cla.value_counts('logtargamt')[1])  # 28

# over sample - classification training set only
X_init = train_cla.drop(['id', 'logtargamt'], axis=1)
y_init = train_cla.logtargamt
oversample = SMOTE(random_state=42)
X_cla, y_cla = oversample.fit_resample(X_init, y_init)
print(y_cla.value_counts())  # 50-50 balance

# fit logistic regression, svm, and decision tree models with hyper-parameter tuning
# X_cla_test, y_cla_test = test_cla.drop(['logtargamt', 'id'], axis=1), test_cla.logtargamt
# log_reg = LogisticRegression()
# rfe_cv = RFECV(log_reg, step=1, cv=10)
# log_reg.fit(X[['freq_new', 'amt_new', 'recency', 'tof']], y)  # accuracy rate is 55%

# fit multiple regression models with least square and elastic net
# set X and y
X_reg, y_reg = train_reg.drop(['id', 'logtargamt', 'frequency', 'amount'], axis=1), train_reg.logtargamt
X_reg_test, y_reg_test = test_reg.drop(['id', 'logtargamt', 'frequency', 'amount'], axis=1), test_reg.logtargamt
# linear regression
lin_reg = LinearRegression()
lin_reg.fit(X_reg, y_reg)
y_pred_lin = lin_reg.predict(X_reg_test)
print(mean_squared_error(y_reg_test, y_pred_lin))  # 10.40
# elastic net
elastic_net = ElasticNetCV(l1_ratio=[.1, .5, .7, .9, .95, .99, 1], max_iter=20000)
elastic_net.fit(X_reg, y_reg)
y_pred_elastic = elastic_net.predict(X_reg_test)
print(mean_squared_error(y_reg_test, y_pred_elastic))  # 10.37

# select linear features according to elastic net
print(elastic_net.alpha_)  # 25.35
print(elastic_net.l1_ratio_)  # 0.1
print(elastic_net.intercept_)  # 3.19
elastic_coefs = elastic_net.coef_
features = X_reg.columns[elastic_coefs != 0]  # recency, tof, amt_new, 3 others
linear_coefficients = elastic_coefs[elastic_coefs != 0]

# take different combinations of expected values

# compare the top 500 customers from the prediction (on the test set) to the actual 500 customers
# confusion matrix
