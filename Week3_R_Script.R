# Part A
install.packages("ncvreg")
install.packages("bigmemory")
install.packages("biglasso")
library(ncvreg)
library(bigmemory)
library(biglasso)

install.packages("lars")
library(lars)

install.packages("glmnet")
library(glmnet)

data(diabetes)
attach(diabetes)

summary(x)

par(mfrow=c(2,5))
for(i in 1:10){
  +     plot(x[,i], y)
  +     abline(lm(y~x[,i]))
  + }

model_ols <- lm(y ~ x)
summary(model_ols)

lasso_model <- glmnet(x, y)
plot.glmnet(lasso_model, xvar = "norm", label = TRUE)

fit_cvglem <- cv.glmnet(x=x, y=y, alpha = 1, nlambda = 1000)
plot(fit_cvglem)

fit1 <- glmnet(x=x, y=y, alpha = 1, lambda=fit_cvglem$lambda.min)
fit1$beta

fit_cvglem$lambda.1se

fit <- glmnet(x=x, y=y, alpha = 1, lambda=fit_cvglem$lambda.1se)
fit$beta

ols_model <- lm(y~x2)
summary(ols_model)

lasso_model1 <- glmnet(x2, y)
plot(lasso_model1, xvar = "norm", label = TRUE)

fit_cv1 <- cv.glmnet(x=x2, y=y, alpha = 1, nlambda = 1000)
plot(fit_cv1)

fit2 <- glmnet(x=x2, y=y, alpha = 1, lambda=fit_cv1$lambda.min)
fit2$beta

# Part B 

library(MASS)
data <- Boston

train <- data[1:400,]
test <- data[401:506,]

library(glmnet)

library(Metrics)

x.train <- model.matrix (medv~., train) [,-1]

set.seed (123)
cv <- cv.glmnet (x.train, y.train, alpha = 0)
plot (cv)

cv$lambda.min

model_ridge <- glmnet (x.train, y.train, alpha = 0, lambda = cv$lambda.min)
coef (model_ridge)

x.test <- model.matrix (medv ~., test)[,-1]

prediction_ridge <- as.vector(predict(model_ridge,x.test))

Accuracy_ridge <- data.frame(
  +     RMSE = rmse (prediction_ridge, test$medv),
  +     SSE = sse (test$medv, prediction_ridge),
  +     Mape = mape (test$medv, prediction_ridge))

names (train)

model_lm <- lm (medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat, data=train)

summary (model_lm)

model_lm <- lm (medv ~ crim+zn+nox+rm+dis+rad+tax+ptratio+lstat, data=train)
 summary (model_lm)

prediction_lm <- predict (model_lm, test [,-14])

Accuracy_lm <-data.frame (
  +     RMSE = rmse (prediction_lm, test$medv),
  +     SSE = sse (test$medv, prediction_lm),
  +     MAPE = mape (test$medv, prediction_lm))

Accuracy_ridge

Accuracy_lm



