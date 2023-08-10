#### R_Demo_Simulation_Inference_10_Fold_Cross_Validation.R
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
## Goal: Improve the predictive power or predictive validity of a model when applied to new observed values
##
##########################################################################
## Introduction to tutorial:
##
## For this R tutorial we will simulate a dataset and then randomly divide it into ten subsets. 
##
## We will then fit a model using the observations from 1 of the subsets of data (test data) and then use the model estimates to predict the value of the dependent variable for the remaining out of sample data subset (test data).
##
## After that step, we will use the other half of the data to fit the model and then predict the other hold out sample.
##
## In this way, both halves of the data will be predicted using a model estimated from other data not used in the fitting
##
## This process is increasingly common and required in almost all Machine Learning and predictive tasks in data science and increasingly so in the social sciences.
##
##########################################################################


#rm(list = ls())

set.seed(940)

## set number of observations for simulation
n <- 100

## number of folds (randomly created sub samples of data)
k <- 10

## simulation of variables
x <- sample(4:14,n,replace=TRUE)
y <- -5.996 + 2.781*x -0.127*x^2   + rnorm(n,0,1)
#y <- -5.996 + 2.781*x -0.127*x^2   + rnorm(n,0,2)

plot(x,y)

## create a subject/unit ID variable with one values for each unit
## here the indicator values takes on 2-Fold values {1,2}
folds <- sample(rep(1:k, k), n, replace=FALSE)

table(folds)

## create a data frame with the dependent varaible, independent variable, and randomly created ID
dat <- data.frame(y, x, folds)

## create vectors for storing predictions
dat$y.hat0 <- NA
dat$y.hat1 <- NA
dat$y.hat2 <- NA
dat$y.hat3 <- NA

#test <- matrix(NA, nrow=k, ncol=2)

##  function to
for(i in 1:k){
    
    ## fit a linear model
    fit0 <- lm(y ~ 1, data=subset(dat, folds!=i))
    pred0 <- predict(fit0, newdata=subset(dat, folds==i))
    y.hat0 <- as.numeric(pred0)
  
    dat$y.hat0[dat$fold==i] <- y.hat0
  
  ## fit a linear model
    fit1 <- lm(y ~ x, data=subset(dat, folds!=i))
    pred1 <- predict(fit1, newdata=subset(dat, folds==i))
    y.hat1 <- as.numeric(pred1)
    
    dat$y.hat1[dat$fold==i] <- y.hat1
    
    
    ## fit a linear model with a squared term
    fit2 <- lm(y ~ x + I(x^2), data=subset(dat, folds!=i))
    pred2 <- predict(fit2, newdata=subset(dat, folds==i))
    y.hat2 <- as.numeric(pred2)
    
    dat$y.hat2[dat$fold==i] <- y.hat2

    ## fit a linear model with a squared term and cubic term
    fit2 <- lm(y ~ x + I(x^2) + I(x^3), data=subset(dat, folds!=i))
    pred2 <- predict(fit2, newdata=subset(dat, folds==i))
    y.hat3 <- as.numeric(pred2)
    
    dat$y.hat3[dat$fold==i] <- y.hat3
    
    print(summary(dat))
}

rmse.fit0 <- sqrt(mean((dat$y.hat0-dat$y)^2))
rmse.fit0

rmse.fit1 <- sqrt(mean((dat$y.hat1-dat$y)^2))
rmse.fit1

rmse.fit2 <- sqrt(mean((dat$y.hat2-dat$y)^2))
rmse.fit2

rmse.fit3 <- sqrt(mean((dat$y.hat3-dat$y)^2))
rmse.fit3

c(rmse.fit0, rmse.fit1, rmse.fit2, rmse.fit3)


cor.fit0 <- cor(dat$y.hat0, dat$y, method="spearman")
cor.fit0

cor.fit1 <- cor(dat$y.hat1, dat$y, method="spearman")
cor.fit1

cor.fit2 <- cor(dat$y.hat2, dat$y, method="spearman")
cor.fit2

cor.fit3 <- cor(dat$y.hat3, dat$y, method="spearman")
cor.fit3

c(cor.fit0, cor.fit1, cor.fit2, cor.fit3)






