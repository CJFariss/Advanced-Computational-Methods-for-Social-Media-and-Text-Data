#### R_Demo_Simulation_Inference_2_Sample_Hold_out.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-09
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
##
## Goal: Improve the predictive power or predictive validity of a model
##
##########################################################################
## Introduction to tutorial:
##
## (1) Begin building towards and learning about cross-validation
## (NOTE: There is no "crossing" yet)
##
## For this R tutorial we will simulate a dataset and then randomly divide it into two subsets.
##
## We will fit a model using the observations from one of the subsets of data (training data).
##
## We will then use the model estimates to predict the value of the dependent variable for the remaining out-of-sample data subset (testing data).
##
##########################################################################


#set.seed(940)


## set number of observations for simulation
n <- 100

## simulation of variables (This model is one of Anscombe's quartets)
x <- sample(4:14,n,replace=TRUE)
table(x)
y <- -5.996 + 2.781*x -0.127*x^2 + rnorm(n,0,1)
#y <- -5.996 + 2.781*x -0.127*x^2 + rnorm(n,0,2)

## plot the simulated relationship
par(mfrow=c(1,1))
plot(x=x, y=y)

## create a subject/unit ID variable with one values for each unit
## here the indicator values takes on 2-Fold values {1,2}
folds <- sample(rep(1:2, n/2), size=n, replace=FALSE)
folds
table(folds)

## doesn't always yield 50/50 ratio of 1s and 2s
#folds <- sample(1:2, size=n, replace=TRUE)
#folds
#table(folds)

## create a data frame with the dependent variable, independent variable, and randomly created ID
dat <- data.frame(y, x, folds)

summary(dat)

head(dat)

## fit a linear model to the full dataset
model <- lm(y ~ x, data=dat)
summary(model)


## subset the full dataset into to subsets based on the ID variable
train <- subset(dat, folds==1)
test <- subset(dat, folds==2)

train <- dat[dat$folds==1,]
test <- dat[dat$folds==2,]

nrow(train)
nrow(test)


## Model 0: fit a linear model
fit <- lm(y ~ 1, data=train)
in_sample_rmse <- sqrt(mean((as.numeric(predict(fit))-train$y)^2))
in_sample_rmse

pred <- predict(fit, newdata=test)
rmse <- sqrt(mean((as.numeric(pred)-test$y)^2))
rmse

## Model 1: fit a linear model
fit <- lm(y ~ x, data=train)
in_sample_rmse <- sqrt(mean((as.numeric(predict(fit))-train$y)^2))
in_sample_rmse

pred <- predict(fit, newdata=test)
rmse <- sqrt(mean((as.numeric(pred)-test$y)^2))
rmse

## this is what predict function is doing under the hood
y_hat <- fit$coefficients[1] + fit$coefficients[2] * test$x 

## Model 2: fit a linear model with a squared term
fit <- lm(y ~ x + I(x^2), data=train)
in_sample_rmse <- sqrt(mean((as.numeric(predict(fit))-train$y)^2))
in_sample_rmse

pred <- predict(fit, newdata=test)
rmse <- sqrt(mean((as.numeric(pred)-test$y)^2))
rmse


## Model 3: fit a linear model with a squared term and a cubic term
fit <- lm(y ~ x + I(x^2) + I(x^3), data=train)
in_sample_rmse <- sqrt(mean((as.numeric(predict(fit))-train$y)^2))
in_sample_rmse

pred <- predict(fit, newdata=test)
rmse <- sqrt(mean((as.numeric(pred)-test$y)^2))
rmse


## Model 4: fit a linear model with a squared term and a cubic term and a 4th order term
fit <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4), data=train)
in_sample_rmse <- sqrt(mean((as.numeric(predict(fit))-train$y)^2))
in_sample_rmse

pred <- predict(fit, newdata=test)
rmse <- sqrt(mean((as.numeric(pred)-test$y)^2))
rmse




