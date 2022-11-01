library(car)
library(caret)
library(corrplot)
library(dplyr)
library(e1071)
library(glmnet)
library(MASS)
library(openair)
library(randomForest)
library(VIF)
library(xlsx)


setwd('X:/Data/Biospatial')
data_orig <- read.xlsx('combined_biospatial_weekly_rates.xlsx', sheetIndex = 2, header = TRUE)
data_full <- data_orig %>% slice(105:208)
data_full <- data_full[,c(-1)]

full_model <- lm(Actual.Firearm ~., data = data_full)

forward <- stepAIC(full_model, direction = "forward")
forward$anova
backward <- stepAIC(full_model, direction = "backward")
backward$anova
both <- stepAIC(full_model, direction = "both")
both$anova

lasso = cv.glmnet(x=as.matrix(data_full[,-53]), y=as.matrix(data_full$Actual.Firearm), alpha = 1,
                  nfolds = 5, type.measure = 'mse', family = 'gaussian')
coef(lasso, s = lasso$lambda.min)

R2 = c()
for(i in 1:10) {
  mod_elastic = cv.glmnet(x=as.matrix(data_full[,-53]), y=as.matrix(data_full$Actual.Firearm), alpha = i/10,
                          nfolds = 5, type.measure = 'mse', family = 'gaussian' )
  R2 = cbind(R2, mod_elastic$glmnet.fit$dev.ratio[which(mod_elastic$glmnet.fit$lambda == mod_elastic$lambda.min)])
}
alpha_best = (which.max(R2)-1)/10
alpha_best
elastic = cv.glmnet(x=as.matrix(data_full[,-53]), y=as.matrix(data_full$Actual.Firearm), alpha = alpha_best,
                    nfolds = 5, type.measure = 'mse', family = 'gaussian' )
coef(elastic, s = elastic$lambda.min)

rf = randomForest(Actual.Firearm ~., data = data_full, ntree = 100, importance = TRUE)
varImp(rf)


#------2019 TEST----------------------------------------------------------------------------------------------------

#-----Linear Multivariate Regression--------------------------------------------------------------------------------------------------------

setwd('X:/Data/Biospatial')

data <- read.xlsx('combined_biospatial_weekly_rates.xlsx', sheetIndex = 2, header = TRUE)
train = data %>% slice(105:208)
test = data %>% slice(209:260)

model_forward_lr = lm(Actual.Firearm ~ Assault + Intentional + Overall + Unintentional
                      ,data = train)
pred.train_flr = predict(model_forward_lr, train)
pred.test_flr = predict(model_forward_lr, test)
rmse_train_flr <- sqrt((sum((train$Actual.Firearm - pred.train_flr)^2))/104)
rmse_test_flr <- sqrt((sum((test$Actual.Firearm - pred.test_flr)^2))/52)
rmse_train_flr
rmse_test_flr

model_backward_lr = lm(Actual.Firearm ~ Assault + Overall
                       ,data = train)
pred.train_blr = predict(model_backward_lr, train)
pred.test_blr = predict(model_backward_lr, test)
rmse_train_blr <- sqrt((sum((train$Actual.Firearm - pred.train_blr)^2))/104)
rmse_test_blr <- sqrt((sum((test$Actual.Firearm - pred.test_blr)^2))/52)
rmse_train_blr
rmse_test_blr

#plot(test$Actual.Firearm, pch = 16)
#points(pred.test_blr, col = 'blue', pch = 16)
#lines(pred.test_blr, col = 'blue', pch = 16)


#write.xlsx(pred.test_blr, 'C:/Users/qlu9/Desktop/prediction_res.xlsx') 


model_rf_lr = lm(Actual.Firearm ~ Overall
                 ,data = train)
pred.train_rf_lr = predict(model_rf_lr, train)
pred.test_rf_lr = predict(model_rf_lr, test)
rmse_train_rf_lr <- sqrt((sum((train$Actual.Firearm - pred.train_rf_lr)^2))/104)
rmse_test_rf_lr <- sqrt((sum((test$Actual.Firearm - pred.test_rf_lr)^2))/52)
rmse_train_rf_lr
rmse_test_rf_lr

#write.xlsx(pred.test_rf_lr, 'C:/Users/qlu9/Desktop/prediction_res.xlsx')

#-----Support Vector Regression-----------------------------------------------------------------------------------------------------------

data2 <- read.xlsx('combined_biospatial_weekly_rates.xlsx', sheetIndex = 2, header = TRUE)
train2 = data2 %>% slice(105:208)
test2 = data2 %>% slice(209:260)

model_forward_svr = svm(Actual.Firearm ~ Assault + Intentional + Overall + Unintentional
                        ,data = train2)
pred.train_svr = predict(model_forward_svr, train2)
pred.test_svr = predict(model_forward_svr, test2)
rmse_train_svr <- sqrt((sum((train2$Actual.Firearm - pred.train_svr)^2))/104)
rmse_test_svr <- sqrt((sum((test2$Actual.Firearm - pred.test_svr)^2))/52)
rmse_train_svr
rmse_test_svr

model_backward_svr = svm(Actual.Firearm ~ Assault + Overall
                         ,data = train2)
pred.train_bsvr = predict(model_backward_svr, train2)
pred.test_bsvr = predict(model_backward_svr, test2)
rmse_train_bsvr <- sqrt((sum((train2$Actual.Firearm - pred.train_bsvr)^2))/104)
rmse_test_bsvr <- sqrt((sum((test2$Actual.Firearm - pred.test_bsvr)^2))/52)
rmse_train_bsvr
rmse_test_bsvr

tuned_svr = tune(svm, Actual.Firearm ~ Assault + Overall
                 ,data = train2, ranges=list(epsilon=seq(0,1,0.1), cost = 2^(2:9)))
best_svr = tuned_svr$best.model
pred.train_bsvr_best = predict(best_svr, train2)
pred.test_bsvr_best = predict(best_svr, test2)
rmse_train_bsvr_best <- sqrt((sum((train2$Actual.Firearm - pred.train_bsvr_best)^2))/104)
rmse_test_bsvr_best <- sqrt((sum((test2$Actual.Firearm - pred.test_bsvr_best)^2))/52)
rmse_train_bsvr_best
rmse_test_bsvr_best


model_rf_svr = svm(Actual.Firearm ~ Overall
                   ,data = train2)
pred.train_rf_svr = predict(model_rf_svr, train2)
pred.test_rf_svr = predict(model_rf_svr, test2)
rmse_train_rf_svr <- sqrt((sum((train2$Actual.Firearm - pred.train_rf_svr)^2))/104)
rmse_test_rf_svr <- sqrt((sum((test2$Actual.Firearm - pred.test_rf_svr)^2))/52)
rmse_train_rf_svr
rmse_test_rf_svr


#----Random Forest--------------------------------------------------------------------------------------------------------------------

data3 <- read.xlsx('combined_biospatial_weekly_rates.xlsx', sheetIndex = 2, header = TRUE)
train3 = data3 %>% slice(105:208)
test3 = data3 %>% slice(209:260)

model_forward_rf = randomForest(Actual.Firearm ~ Assault + Intentional + Overall + Unintentional
                                ,data = train3)
pred.train_rf = predict(model_forward_rf, train3)
pred.test_rf = predict(model_forward_rf, test3)
rmse_train_rf <- sqrt((sum((train3$Actual.Firearm - pred.train_rf)^2))/104)
rmse_test_rf <- sqrt((sum((test3$Actual.Firearm - pred.test_rf)^2))/52)
rmse_train_rf
rmse_test_rf

model_backward_rf = randomForest(Actual.Firearm ~ Assault + Overall
                                 ,data = train3)
pred.train_brf = predict(model_backward_rf, train3)
pred.test_brf = predict(model_backward_rf, test3)
rmse_train_brf <- sqrt((sum((train3$Actual.Firearm - pred.train_brf)^2))/104)
rmse_test_brf <- sqrt((sum((test3$Actual.Firearm - pred.test_brf)^2))/52)
rmse_train_brf
rmse_test_brf


model_rf_rf = randomForest(Actual.Firearm ~ Overall
                           ,data = train3)
pred.train_rf_rf = predict(model_rf_rf, train3)
pred.test_rf_rf = predict(model_rf_rf, test3)
rmse_train_rf_rf <- sqrt((sum((train3$Actual.Firearm - pred.train_rf_rf)^2))/104)
rmse_test_rf_rf <- sqrt((sum((test3$Actual.Firearm - pred.test_rf_rf)^2))/52)
rmse_train_rf_rf
rmse_test_rf_rf

