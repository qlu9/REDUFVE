library(car)
library(caret)
library(corrplot)
library(dplyr)
library(e1071)
library(glmnet)
library(MASS)
library(openair)
library(randomForest)
library(xlsx)



#------2019 TEST----------------------------------------------------------------------------------------------------

#-----Linear Multivariate Regression--------------------------------------------------------------------------------------------------------

setwd('X:/Data/NDVH')

data <- read.xlsx('combined_NDVH_weekly.xlsx', sheetIndex = 2, header = TRUE)
train = data %>% slice(1:104)
test = data %>% slice(105:156)

model_forward_lr = lm(Actual.Firearm ~ firearm
                      ,data = train)
pred.train_flr = predict(model_forward_lr, train)
pred.test_flr = predict(model_forward_lr, test)
rmse_train_flr <- sqrt((sum((train$Actual.Firearm - pred.train_flr)^2))/104)
rmse_test_flr <- sqrt((sum((test$Actual.Firearm - pred.test_flr)^2))/52)
rmse_train_flr
rmse_test_flr




#-----Support Vector Regression-----------------------------------------------------------------------------------------------------------

data2 <- read.xlsx('combined_NDVH_weekly.xlsx', sheetIndex = 2, header = TRUE)
train2 = data2 %>% slice(1:104)
test2 = data2 %>% slice(105:156)

model_forward_svr = svm(Actual.Firearm ~ firearm
                        ,data = train2)
pred.train_svr = predict(model_forward_svr, train2)
pred.test_svr = predict(model_forward_svr, test2)
rmse_train_svr <- sqrt((sum((train2$Actual.Firearm - pred.train_svr)^2))/104)
rmse_test_svr <- sqrt((sum((test2$Actual.Firearm - pred.test_svr)^2))/52)
rmse_train_svr
rmse_test_svr



#----Random Forest--------------------------------------------------------------------------------------------------------------------

data3 <- read.xlsx('combined_NDVH_weekly.xlsx', sheetIndex = 2, header = TRUE)
train3 = data3 %>% slice(1:104)
test3 = data3 %>% slice(105:156)

model_forward_rf = randomForest(Actual.Firearm ~ firearm
                                ,data = train3)
pred.train_rf = predict(model_forward_rf, train3)
pred.test_rf = predict(model_forward_rf, test3)
rmse_train_rf <- sqrt((sum((train3$Actual.Firearm - pred.train_rf)^2))/104)
rmse_test_rf <- sqrt((sum((test3$Actual.Firearm - pred.test_rf)^2))/52)
rmse_train_rf
rmse_test_rf

