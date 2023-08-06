## R_Demo_Simulation_Inference_2_Sample_Cross_Validation.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2022
##
## Date: 2022-08-28
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
##
## Goal: Improve the predictive power or predictive validity of a model when applied to new observed values
##
##########################################################################
##
## (1) Begin building towards and learning about cross-validation
## (NOTE: There is now "crossings" in this example)
##
## For this R tutorial we will simulate a dataset and then randomly divide it into two subsets.
##
## We will fit a model using the observations from one of the subsets of data (training data).
##
## We will then use the model estimates to predict the value of the dependent variable for the remaining out of sample data subset (testing data).
##
## After that step, we will repeat the above process and make a cross over the two folds of data.
##
## Specifically, we will use the other half of the data to fit the model and then predict the other hold out sample.
##
## The training data will become the test data and the test data will become the training data.
##
## In this way, both halves of the data will be predicted using a model estimated from other data not used in the fitting of the model.
##
## This increases the probability that we do not overfit our model parameters to our dataset and makes use of all the data for fitting and evaluation instead of just half of the data.
##
## This process is increasingly common and required in almost all Machine Learning and predictive tasks in data science and increasingly so in the social sciences.
##
##########################################################################

set.seed(940)

## set number of observations for simulation
n <- 100

## simulation of variables (This model is one of Anscombe's quartets)
x <- sample(4:14,n,replace=TRUE)
y <- -5.996 + 2.781*x -0.127*x^2 + rnorm(n,0,1)
#y <- -5.996 + 2.781*x -0.127*x^2 + rnorm(n,0,2)

## plot the simulated relationship
par(mfrow=c(1,1))
plot(x=x, y=y)

## create a subject/unit ID variable with one values for each unit
## here the indicator values takes on 2-Fold values {1,2}
folds <- sample(rep(1:2, n/2), n, replace=FALSE)

## create a data frame with the dependent varaible, independent variable, and randomly created ID
dat <- data.frame(y, x, folds)


## Model 1: fit a linear model
fit <- lm(y ~ x, data=subset(dat, folds==1))
pred <- predict(fit, newdata=subset(dat, folds==2))
y.hat.fold2 <- as.numeric(pred)

fit <- lm(y ~ x, data=subset(dat, folds==2))
pred <- predict(fit, newdata=subset(dat, folds==1))
y.hat.fold1 <- as.numeric(pred)

dat$y.hat[dat$fold==2] <- y.hat.fold2
dat$y.hat[dat$fold==1] <- y.hat.fold1

summary(dat)

rmse <- sqrt(mean((dat$y.hat-dat$y)^2))
rmse


## Model 2: fit a linear model with a squared term
fit <- lm(y ~ x + I(x^2), data=subset(dat, folds==1))
pred <- predict(fit, newdata=subset(dat, folds==2))
y.hat.fold2 <- as.numeric(pred)

fit <- lm(y ~ x + I(x^2), data=subset(dat, folds==2))
pred <- predict(fit, newdata=subset(dat, folds==1))
y.hat.fold1 <- as.numeric(pred)

dat$y.hat[dat$fold==2] <- y.hat.fold2
dat$y.hat[dat$fold==1] <- y.hat.fold1

rmse <- sqrt(mean((dat$y.hat-dat$y)^2))
rmse


## more compact version of Model 1 above using a for loop
for(i in 1:2){
    fit <- lm(y ~ 1, data=subset(dat, folds!=i))
    pred <- predict(fit, newdata=subset(dat, folds==i))
    dat$y.hat[dat$fold==i] <- as.numeric(pred)
}

rmse <- sqrt(mean((dat$y.hat-dat$y)^2))
rmse

for(i in 1:2){
    fit <- lm(y ~ x, data=subset(dat, folds!=i))
    pred <- predict(fit, newdata=subset(dat, folds==i))
    dat$y.hat[dat$fold==i] <- as.numeric(pred)
}

rmse <- sqrt(mean((dat$y.hat-dat$y)^2))
rmse


for(i in 1:2){
  fit <- lm(y ~ x + I(x^2), data=subset(dat, folds!=i))
  pred <- predict(fit, newdata=subset(dat, folds==i))
  dat$y.hat[dat$fold==i] <- as.numeric(pred)
}

rmse <- sqrt(mean((dat$y.hat-dat$y)^2))
rmse




