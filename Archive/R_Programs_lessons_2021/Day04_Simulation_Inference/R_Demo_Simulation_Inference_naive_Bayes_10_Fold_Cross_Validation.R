#### R_Demo_Simulation_Inference_naive_Bayes_10_Fold_Cross_Validation.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2021
##
## Date: 2021-07-24
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
##
## (1) Program simulates count data that is then used to predict a binary outcome variable.
##
## (2) Three models are evaluated using the count data to predict the outcome:
## (2a) linear model
## (2b) generalized linear model with a logit link function
## (2c) naive Bayes classifier.
##
##########################################################################


## load library
llibrary(e1071)


## simulate x1 and set the "true" population values alpha and beta
n <- 100

## unobserved
x <- runif(n,0,1)

## observed counts
x1 <- rpois(n, lambda=x)
x2 <- rpois(n, lambda=x)
x3 <- rpois(n, lambda=2*x)
x4 <- rpois(n, lambda=2*x)
x5 <- rpois(n, lambda=4*x)

## systematic component of the model based on observed counts
xb <- -2 + x1 + x2 + x3 + x4 + x5

## transform the linear term xb using
## the inverse logit function
## so that theta is bound from 0 to 1
pi <- 1 / (1 + exp(-xb))

## generate the dependent variable y with probability pi and measurement error from a Bernoulli trial
y <- rbinom(n, size=1, prob=pi)


## make data frame
dat <- data.frame(y, x1, x2, x3, x4, x5)


## summarize fit using linear model
summary(lm(y ~ x1 + x2 + x3 + x4 + x5, data=dat))


## summarize fit using glm using the logit link function
summary(glm(y ~ x1 + x2 + x3 + x4 + x5, family=binomial(link="logit")))


## summarize fit using naiveBayes model
naiveBayes(as.factor(y) ~ x1 + x2 + x3 + x4 + x5, data=dat)


## create vectors for storing predictions
dat$y.hat1 <- NA
dat$y.hat2 <- NA
dat$y.hat3 <- NA

## select number of folds
k <- 10

## create vector of folds for cross validation
dat$folds <- sample(rep(1:k, k), n, replace=FALSE)

## lapply function to
for(i in 1:k){
    
    ## fit a linear model
    fit1 <- lm(y ~ x1 + x2 + x3 + x4 + x5, data=subset(dat, folds!=i))
    pred1 <- predict(fit1, newdata=subset(dat, folds==i))
    y.hat1 <- as.numeric(pred1)
    
    dat$y.hat1[dat$fold==i] <- y.hat1
    
    
    ## fit a glm model
    fit2 <- glm(y ~ x1 + x2 + x3 + x4 + x5, binomial(link="logit"), data=subset(dat, folds!=i))
    pred2 <- predict(fit2, newdata=subset(dat, folds==i))
    y.hat2 <- as.numeric(pred2)
    
    dat$y.hat2[dat$fold==i] <- y.hat2
    
    
    ## fit a naiveBayes classifier model
    fit3 <- naiveBayes(as.factor(y) ~ x1 + x2 + x3 + x4 + x5, data=subset(dat, folds!=i))
    pred3 <- predict(fit3, newdata=subset(dat, folds==i))
    y.hat3 <- as.numeric(pred3)
    
    dat$y.hat3[dat$fold==i] <- y.hat3
    
    #print(summary(dat))
}

rmse.fit1 <- sqrt(mean((dat$y.hat1-dat$y)^2))
rmse.fit1

cor.fit1 <- cor(dat$y.hat1, dat$y, method="spearman")

rmse.fit2 <- sqrt(mean((dat$y.hat2-dat$y)^2))
rmse.fit2

cor.fit2 <- cor(dat$y.hat2, dat$y, method="spearman")

rmse.fit3 <- sqrt(mean((dat$y.hat3-dat$y)^2))
rmse.fit3

cor.fit3 <- cor(dat$y.hat3, dat$y, method="spearman")

c(cor.fit1, cor.fit2, cor.fit3)

