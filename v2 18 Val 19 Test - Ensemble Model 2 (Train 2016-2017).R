library(car)
library(caret)
library(corrplot)
library(dplyr)
library(tidyr)
library(e1071)
library(glmnet)
library(MASS)
library(openair)
library(randomForest)
library(xlsx)
library(gbm)
library(neuralnet)
library(forecast)
library(Metrics)


#----Baseline Forecast--------

setwd('X:/Analysis/Ensemble')

data_baseline <- read.xlsx('combined_baseline_weekly.xlsx', sheetIndex = 2, header = TRUE)

train_baseline = ts(data_baseline[,"Actual.Firearm"], frequency = 52, start = c(2016,1), end = c(2018,52))
test_baseline = ts(data_baseline[,"Actual.Firearm"], frequency = 52, start = c(2019,1), end = c(2019,52))

autoplot(train_baseline)

model_baseline <- auto.arima(train_baseline)

summary(model_baseline)

forecast_2019 <- forecast(model_baseline, h = 52)

autoplot

plot(forecast_2019)

#write.xlsx(forecast_2019, 'C:/Users/qlu9/Desktop/pred.xlsx')

#--------Google------------

setwd('X:/Data/GoogleTrends')

data <- read.xlsx('combined_google_weekly.xlsx', sheetIndex = 2, header = TRUE)
train = data %>% slice(105:208)
test = data %>% slice(261:312)

model_backward_lr = lm(Actual.Firearm ~ Gun + Rifle + Pistol. + Homicide + Murder + 
                         Gun.shot + Gun.violence
                       ,data = train)
pred.train_blr = predict(model_backward_lr, train)
pred.test_blr = predict(model_backward_lr, test)
rmse_train_blr <- sqrt((sum((train$Actual.Firearm - pred.train_blr)^2))/104)
rmse_test_blr <- sqrt((sum((test$Actual.Firearm - pred.test_blr)^2))/52)
rmse_train_blr
rmse_test_blr

#-----Youtube---------

setwd('X:/Data/YouTubeTrends')

data2 <- read.xlsx('combined_youtube_weekly.xlsx', sheetIndex = 2, header = TRUE)
train2 = data2 %>% slice(105:208)
test2 = data2 %>% slice(261:312)

model_rf_svr = svm(Actual.Firearm ~ Shooting + Rifle + X9mm + Pistol. + Gun.shot
                   ,data = train2)
pred.train_rf_svr = predict(model_rf_svr, train2)
pred.test_rf_svr = predict(model_rf_svr, test2)
rmse_train_rf_svr <- sqrt((sum((train2$Actual.Firearm - pred.train_rf_svr)^2))/104)
rmse_test_rf_svr <- sqrt((sum((test2$Actual.Firearm - pred.test_rf_svr)^2))/52)
rmse_train_rf_svr
rmse_test_rf_svr

#-----Biospatial------

setwd('X:/Data/Biospatial')

data3 <- read.xlsx('combined_biospatial_weekly_rates.xlsx', sheetIndex = 2, header = TRUE)
train3 = data3 %>% slice(105:208)
test3 = data3 %>% slice(261:312)

model_rf_lr = lm(Actual.Firearm ~ Overall
                 ,data = train3)
pred.train_rf_lr = predict(model_rf_lr, train3)
pred.test_rf_lr = predict(model_rf_lr, test3)
rmse_train_rf_lr <- sqrt((sum((train3$Actual.Firearm - pred.train_rf_lr)^2))/104)
rmse_test_rf_lr <- sqrt((sum((test3$Actual.Firearm - pred.test_rf_lr)^2))/52)
rmse_train_rf_lr
rmse_test_rf_lr

#-----NSSP-------

setwd('X:/Data/NSSP')

data4 <- read.xlsx('combined_NSSP_weekly_rates_DQ_filters.xlsx', sheetIndex = 2, header = TRUE)
train4 = data4 %>% slice(1:104)
test4 = data4 %>% slice(157:208)

model_rf_lr2 = lm(Actual.Firearm ~ Overall
                 ,data = train4)
pred.train_rf_lr2 = predict(model_rf_lr2, train4)
pred.test_rf_lr2 = predict(model_rf_lr2, test4)
rmse_train_rf_lr2 <- sqrt((sum((train4$Actual.Firearm - pred.train_rf_lr2)^2))/104)
rmse_test_rf_lr2 <- sqrt((sum((test4$Actual.Firearm - pred.test_rf_lr2)^2))/52)
rmse_train_rf_lr2
rmse_test_rf_lr2

#-----NDVH-------

setwd('X:/Data/NDVH')

data5 <- read.xlsx('combined_NDVH_weekly.xlsx', sheetIndex = 2, header = TRUE)
train5 = data5 %>% slice(1:104)
test5 = data5 %>% slice(157:208)

model_forward_svr = svm(Actual.Firearm ~ firearm
                        ,data = train5)
pred.train_svr = predict(model_forward_svr, train5)
pred.test_svr = predict(model_forward_svr, test5)
rmse_train_svr <- sqrt((sum((train5$Actual.Firearm - pred.train_svr)^2))/104)
rmse_test_svr <- sqrt((sum((test5$Actual.Firearm - pred.test_svr)^2))/52)
rmse_train_svr
rmse_test_svr


#----stacked Ensemble------

set.seed(8)

Stacked_Data1 = data.frame(pred.train_blr, pred.train_rf_svr, pred.train_rf_lr, 
                           pred.train_rf_lr2, pred.train_svr, AS = train$Actual.Firearm)


Stacked_Data2 = data.frame(pred.train_blr = pred.test_blr, pred.train_rf_svr = pred.test_rf_svr, 
                           pred.train_rf_lr = pred.test_rf_lr, 
                           pred.train_rf_lr2 = pred.test_rf_lr2, 
                           pred.train_svr = pred.test_svr,
                           AS = test$Actual.Firearm)


#GBM
GBMStackModel = gbm(AS ~ ., data = Stacked_Data1)

StackPredTrain = predict(GBMStackModel, Stacked_Data1)
rmse_train_gbm <- sqrt((sum((Stacked_Data1$AS - StackPredTrain)^2))/104)
rmse_train_gbm

GBMStackPredTest18v = predict(GBMStackModel, Stacked_Data2)
rmse_test_gbm18v <- sqrt((sum((Stacked_Data2$AS - GBMStackPredTest18v)^2))/52)
rmse_test_gbm18v

sum(GBMStackPredTest18v)
cor(Stacked_Data2$AS, GBMStackPredTest18v)

#NeuralNet
GBMStackModel = neuralnet(AS ~ ., data = Stacked_Data1)

StackPredTrain = predict(GBMStackModel, Stacked_Data1)
rmse_train_gbm <- sqrt((sum((Stacked_Data1$AS - StackPredTrain)^2))/104)
rmse_train_gbm

GBMStackPredTest18v = predict(GBMStackModel, Stacked_Data2)
rmse_test_gbm18v <- sqrt((sum((Stacked_Data2$AS - GBMStackPredTest18v)^2))/52)
rmse_test_gbm18v

sum(GBMStackPredTest18v)
cor(Stacked_Data2$AS, GBMStackPredTest18v)

#GLM
GBMStackModel = glm(AS ~ ., data = Stacked_Data1)

StackPredTrain = predict(GBMStackModel, Stacked_Data1)
rmse_train_gbm <- sqrt((sum((Stacked_Data1$AS - StackPredTrain)^2))/104)
rmse_train_gbm

GBMStackPredTest18v = predict(GBMStackModel, Stacked_Data2)
rmse_test_gbm18v <- sqrt((sum((Stacked_Data2$AS - GBMStackPredTest18v)^2))/52)
rmse_test_gbm18v

sum(GBMStackPredTest18v)
cor(Stacked_Data2$AS, GBMStackPredTest18v)

#write.xlsx(GBMStackPredTest18v, 'C:/Users/qlu9/Desktop/predict.xlsx') 

#SVM
GBMStackModel = svm(AS ~ ., data = Stacked_Data1)

StackPredTrain = predict(GBMStackModel, Stacked_Data1)
rmse_train_gbm <- sqrt((sum((Stacked_Data1$AS - StackPredTrain)^2))/104)
rmse_train_gbm

GBMStackPredTest18v = predict(GBMStackModel, Stacked_Data2)
rmse_test_gbm18v <- sqrt((sum((Stacked_Data2$AS - GBMStackPredTest18v)^2))/52)
rmse_test_gbm18v

sum(GBMStackPredTest18v)
cor(Stacked_Data2$AS, GBMStackPredTest18v)

#RandomForest
GBMStackModel = randomForest(AS ~ ., data = Stacked_Data1)

StackPredTrain = predict(GBMStackModel, Stacked_Data1)
rmse_train_gbm <- sqrt((sum((Stacked_Data1$AS - StackPredTrain)^2))/104)
rmse_train_gbm

GBMStackPredTest18v = predict(GBMStackModel, Stacked_Data2)
rmse_test_gbm18v <- sqrt((sum((Stacked_Data2$AS - GBMStackPredTest18v)^2))/52)
rmse_test_gbm18v

sum(GBMStackPredTest18v)
cor(Stacked_Data2$AS, GBMStackPredTest18v)

#Tuned RandomForest

set.seed(8)

Stacked_Data3 = data.frame(pred.train_blr, pred.train_rf_svr, pred.train_rf_lr, 
                           pred.train_rf_lr2, pred.train_svr)

y = train$Actual.Firearm


tunedRF = tuneRF(Stacked_Data3, y, stepFactor=1.5, improve=1e-5, ntree=1000)

GBMStackModel = randomForest(AS ~ ., data = Stacked_Data1, ntree=1000, importance=TRUE, mtry=2)

StackPredTrain = predict(GBMStackModel, Stacked_Data1)
rmse_train_gbm <- sqrt((sum((Stacked_Data1$AS - StackPredTrain)^2))/104)
rmse_train_gbm

GBMStackPredTest18v = predict(GBMStackModel, Stacked_Data2)
rmse_test_gbm18v <- sqrt((sum((Stacked_Data2$AS - GBMStackPredTest18v)^2))/52)
rmse_test_gbm18v

sum(GBMStackPredTest18v)
cor(Stacked_Data2$AS, GBMStackPredTest18v)

#Lasso 

x_train = model.matrix(AS ~., Stacked_Data1)[,-1]
y_train = train$Actual.Firearm

x_test = model.matrix(AS ~., Stacked_Data2)[,-1]
y_test = test$Actual.Firearm

lambdas <- 10^seq(10, -2, length = 100)

lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, 
                       lambda = lambdas, standardize = FALSE,  nfolds = 5, type.measure = 'mse', family = 'gaussian')

lambda_best <- lasso_reg$lambda.min 

GBMStackModel <- glmnet(x_train, y_train, alpha = 1, 
                        lambda = lambda_best, standardize = FALSE, nfolds = 5, type.measure = 'mse', family = 'gaussian')

StackPredTrain = predict(GBMStackModel, s = lambda_best, newx = x_train)
rmse_train_gbm <- sqrt((sum((Stacked_Data1$AS - StackPredTrain)^2))/104)
rmse_train_gbm

GBMStackPredTest18v = predict(GBMStackModel, s = lambda_best, newx = x_test)
rmse_test_gbm18v <- sqrt((sum((Stacked_Data2$AS - GBMStackPredTest18v)^2))/52)
rmse_test_gbm18v

sum(GBMStackPredTest18v)
cor(Stacked_Data2$AS, GBMStackPredTest18v)
