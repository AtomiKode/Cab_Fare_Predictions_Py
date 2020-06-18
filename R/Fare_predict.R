#Remove all the objects stored
rm(list=ls())

#Set current working directory
setwd('C:/Users/SAGAR/Downloads/Data Science/Cab_Fare_Prediction')

#Loading important libraries
x = c('ggplot2', 'corrgram', 'DataCombine','lubridate','dplyr','geosphere','gridExtra',
      'DataExplorer','tidyr')

lapply(x, require, character.only = TRUE)


df=read.csv("train_clean.csv", header = T)

#Removing longitude and latitude variables 
df=subset(df,select=-c(timeofday))

str(df)

#Separating data into train and validation
set.seed(1234)
train_index = sample(1:nrow(df), 0.8 * nrow(df))
train = df[train_index,]
y1=train$fare_amount
traincheck=subset(train,select=-fare_amount)

valid = df[-train_index,]
y2=valid$fare_amount
valid=subset(valid,select=-fare_amount)

#backup = cbind(valid,y2)

################################### Function for Error Metrics ######################################

library(Metrics)
errors = function(y, yhat){
  sse=sse(y,yhat)
  n=length(y)
  k=8
  AIC = 2*k + n*log(sse/n)
  
  print("RMSE:") ; print(rmse(y, yhat)); 
  print("MAE:" ) ; print(mae(y, yhat));
  print("MAPE:") ; print(mape(y, yhat));
  print("AIC:") ; print(AIC);
  
}



############################################## Modelling ##############################################
#Checking variable inflation factor for multicollinearity among independent variables
library(usdm)
vif(traincheck)

#Multiple Linear Regression Model

lm_model = lm(fare_amount ~., data = train)
summary(lm_model)

#On train data
predictions_LR = predict(lm_model, traincheck)
errors(y1,predictions_LR)

#On test data
predictions_LR = predict(lm_model, valid)
errors(y2,predictions_LR)



#Lasso Regression
#install.packages("glmnet")
library(glmnet)

X=model.matrix(fare_amount~. , train)[,-1]
lambda_seq <- 10^seq(2, -2, by = -.1)

cv_output <- cv.glmnet(X, y1,alpha = 0.5, lambda = lambda_seq)

# identifying best lambda
best_lam <- cv_output$lambda.min

lasso_best <- glmnet(X, y1, alpha = 1, lambda = best_lam)
lasso_best
#The best value for lambda is 0.015. But such a small value of lambda will give result similar to 
#linear regression. Thus, we won't use lasso



#Decision tree model
fit = rpart(fare_amount ~ ., data = train, method = "anova")

predictions_DT = predict(fit,traincheck)
errors(y1,predictions_DT)

predictions_DT = predict(fit,valid)
errors(y2,predictions_DT)

#This dosent provide better result than our baselinr MLR model and thus, we won't use it.


#Random Forest
RF_model = randomForest(fare_amount ~ ., train, importance = TRUE, ntree = 200)

RF_Predictions = predict(RF_model, traincheck)
errors(y1,RF_Predictions)

RF_Predictions = predict(RF_model, valid)
errors(y2,RF_Predictions)

#This model performs better than previous models. RMSE for validation data is 3.714. 
#Also, variance between RMSEs of train and validation data is 2.06


#XGBoosting
dtrain <- xgb.DMatrix(data = as.matrix(traincheck), label =y1)
dvalid <- xgb.DMatrix(data = as.matrix(valid), label = y2)

p <- list(objective = "reg:linear",
          eval_metric = "rmse",
          max_depth = 8 ,
          eta = .05, 
          subsample=1,
          colsample_bytree=0.8,
          num_boost_round=1000,
          nrounds = 6000)


set.seed(0)
m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dvalid), print_every_n = 10, early_stopping_rounds = 100)

pred_xgb <- predict(m_xgb, as.matrix(traincheck))
errors(y1,pred_xgb)

pred_xgb <- predict(m_xgb, as.matrix(valid))
errors(y2,pred_xgb)

#We are getting best value for validation dataset as 4.02 which is slightly more compared to random forest.
#Thus, not using this model


#LGBoosting
library(lightgbm)



lgb.grid = list(objective = "regression",
                metric="rmse",
                learning_rate=0.05,
                num_leaves=31,
                feature_fraction = 0.7,
                bagging_fraction = 0.7,
                bagging_freq = 5)

lgb.train = lgb.Dataset(data=as.matrix(traincheck), label=y1)


m_lgb = lgb.train(params = lgb.grid, data = lgb.train, nrounds = 1947,nthread=-1,eval_freq=100)

pred_lgb <- predict(m_lgb, as.matrix(traincheck))
errors(y1,pred_lgb)


pred_lgb <- predict(m_lgb, as.matrix(valid))
errors(y2,pred_lgb)

#Out of all the models we have used, RF, XGB and LGB gave us good results. But the best among them were RF and XGB
#With their respective RMSEs for validation data as 3.714 and 3.697. Since RMSEs are so close to each other, we 
#select the model with least variance between train and test RMSEs and least AIC. Thus, we selected XGB. Now,
#even though XGB doesn't have least variance, it's AIC is less than RF which describes goodness of fit. 



df=read.csv("test_clean.csv", header = T)

df=subset(df,select=-c(timeofday))

summary(df)

pred_xgb <- predict(m_xgb, as.matrix(df))

predicted=cbind(df,pred_xgb)

write.csv(predicted,"Final Predictions.csv",row.names=F)
