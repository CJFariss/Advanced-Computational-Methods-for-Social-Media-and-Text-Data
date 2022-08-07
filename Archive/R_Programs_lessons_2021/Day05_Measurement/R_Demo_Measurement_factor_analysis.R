## R_Demo_factor_analysis.R file
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
## For a factor a analysis model, there is a latent trait theta_i.
##
## The subscript i = 1,... ,N indicates multiple units. y_ik is the observed value for item k for unit i.
##
## For each observed item alpha_k and \beta_k are also estimated.
##
## alpha_k continues to act as "difficulty" parameters, or threshold that benchmarks how likely an indicator is to be observed relative to the values of the latent trait.
##
## In this formulation, this is analogous to an intercept in a traditional linear regression model.
##
## beta_k, often referred to as the "discrimination" parameters and is the analogue of a slope coefficient in a linear regression.
##
##########################################################################


rm(list=ls())

## load library
library(MASS) # load library with truehist function


n <- 100
theta <- rnorm(n,0,1)

## set parameters for each item
## alpha (the intercept) is the difficulty parameter or base-line probability of 1
## beta (the slope) is the discrimination parameter or the strength of the relationship
## between the estimated latent trait theta and the individual item

## item 1 is the most difficult and least informative item
alpha1 <- -1.000000
beta1 <- 1.000000

## item 2 is of medium difficulty relative to the two other items
## it is also the same level of informativeness as item1
alpha2 <- 0.000000
beta2 <- 1.000000

## item 3 is the least difficult item but is also the most informative
## these statements are each relative to the other items in the model
alpha3 <- 1.000000
beta3 <- 3.000000

## define k as the number of items
k <- 3

## linear terms of the item specific models f() that link the latent trait to the items
x1 <- alpha1 + beta1 * theta + rnorm(n)
x2 <- alpha2 + beta2 * theta + rnorm(n)
x3 <- alpha3 + beta3 * theta + rnorm(n)


## create matrix of observed items
x <- cbind(x1, x2, x3)

## fit linear factor model
fit <- factanal(x, factor=1, scores="regression")

fit

## plot true latent variable with posterior mean
par(mar=c(4,4,1,1), font=2, font.lab=2, cex=1.3)
plot(fit$scores, theta, xlim=c(-3,3), ylim=c(-3,3), ylab="true theta", xlab="factor analysis scores")
abline(a=0, b=1, col=2, lwd=2)


D <- nrow(x)
parameters_list <- c(c(1,1,1), runif(D,-1,1))

# user defined function passed to optim
factor_analysis_func <- function(par, y, iterate=TRUE){
    
    b <- par[1:3]
    theta <- par[4:length(par)]
    
    theta <- (theta - mean(theta))/sd(theta)
    
    y.hat1 <- b[1]*theta
    y.hat2 <- b[2]*theta
    y.hat3 <- b[3]*theta
    
    #y.hat1 <- (y.hat1-mean(y.hat1))/sd(y.hat1)
    #y.hat2 <- (y.hat2-mean(y.hat2))/sd(y.hat2)
    #y.hat3 <- (y.hat3-mean(y.hat3))/sd(y.hat3)
    
    y[,1] <- (y[,1] - mean(y[,1]))/sd(y[,1])
    y[,2] <- (y[,2] - mean(y[,2]))/sd(y[,2])
    y[,3] <- (y[,3] - mean(y[,3]))/sd(y[,3])
    
    out <- -sum((y[,1]-y.hat1)^2 + (y[,2]-y.hat2)^2 + (y[,3]-y.hat3)^2)
    
    return(out)
}

out <- optim(par = parameters_list, factor_analysis_func, y=x, method="BFGS", control=list(fnscale = -1), hessian = TRUE)
out
names(out)


out_scores <- out$par[4:length(out$par)]

out$par[1:3]

## plot true latent variable with posterior mean
par(mar=c(4,4,1,1), font=2, font.lab=2, cex=1.3)
plot(out_scores, theta, xlim=c(-3,3), ylim=c(-3,3), ylab="true theta", xlab="factor analysis scores")
abline(a=0, b=1, col=2, lwd=2)

cor(fit$scores, out_scores)

plot(fit$scores, out_scores)



# hat f = Λ' Σ^-1 x

cov(x)
cor(x)
solve(cov(x))

loadings <- c(1,1,1)
t(loadings) %*% solve(cov(x)) %*% t(x)

D <- nrow(x)
parameters_list <- c(1,1,1)

# user defined function passed to optim
factor_analysis_func <- function(par, y, iterate=TRUE){
    
    #a <- par[1:3]
    #b <- par[4:6]
    #theta <- par[7:length(par)]
    y[,1] <- (y[,1] - mean(y[,1]))/sd(y[,1])
    y[,2] <- (y[,2] - mean(y[,2]))/sd(y[,2])
    y[,3] <- (y[,3] - mean(y[,3]))/sd(y[,3])
    
    loadings <- par[1:3]
    theta <- t(loadings) %*% solve(cov(y)) %*% t(y)
    
    theta <- (theta - mean(theta))/sd(theta)
    
    y.hat1 <- loadings[1] * theta
    y.hat2 <- loadings[2] * theta
    y.hat3 <- loadings[3] * theta
    
    #y.hat1 <- (y.hat1-mean(y.hat1))/sd(y.hat1)
    #y.hat2 <- (y.hat2-mean(y.hat2))/sd(y.hat2)
    #y.hat3 <- (y.hat3-mean(y.hat3))/sd(y.hat3)
    
    
    
    out <- -sum((y[,1]-y.hat1)^2 + (y[,2]-y.hat2)^2 + (y[,3]-y.hat3)^2)
    
    return(out)
}

out <- optim(par = parameters_list, factor_analysis_func, y=x, method="BFGS", control=list(fnscale = -1), hessian = TRUE)
out
names(out)
out$par

plot(t(out$par) %*% solve(cov(x)) %*% t(x), theta)

plot(t(out$par) %*% solve(cov(scale(x)[,1:3])) %*% t(scale(x)[,1:3]), theta)
abline(a=0, b=1, col=2, lwd=2)
cor(c(t(out$par) %*% solve(cov(scale(x)[,1:3])) %*% t(scale(x)[,1:3])), theta)

cor(fit$scores, theta)

cor(fit$scores, out_scores)

