## R_Demo_Measurement_factor_analysis.R file
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-11
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

MASS::truehist(fit$scores)

## what are the loadings?
cor(fit$scores, scale(x1))
cor(fit$scores, scale(x2))
cor(fit$scores, scale(x3))

## uniquenesses calculation
sum((fit$scores - scale(x1))^2)
sum((fit$scores - scale(x2))^2)
sum((fit$scores - scale(x3))^2)

## plot true latent variable with posterior mean
par(mar=c(4,4,1,1), font=2, font.lab=2, cex=1.3)
plot(fit$scores, theta, xlim=c(-3,3), ylim=c(-3,3), ylab="true theta", xlab="factor analysis scores")
abline(a=0, b=1, col=2, lwd=2)


D <- nrow(x)
parameters_list <- c(c(1,1,1), runif(D,-1,1))
parameters_list <- c(c(1,1,1), scale(apply(x,1,mean))[,1])

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
    
    ## sse
    #out <- -sum((y[,1]-y.hat1)^2 + (y[,2]-y.hat2)^2 + (y[,3]-y.hat3)^2)
    
    ## mle
    out <- sum(log(dnorm(y[,1], mean=y.hat1, sd=1)) + log(dnorm(y[,2], mean=y.hat2, sd=1)) + log(dnorm(y[,3], mean=y.hat3, sd=1)))
    
    return(out)
}

out <- optim(par = parameters_list, fn=factor_analysis_func, y=x, method="BFGS", control=list(fnscale = -1), hessian = TRUE)
out
names(out)

out_scores <- out$par[4:length(out$par)]

out$par[1:3]

## plot true latent variable with posterior mean
par(mar=c(4,4,1,1), font=2, font.lab=2, cex=1.3)
plot(out_scores, theta, xlim=c(-3,3), ylim=c(-3,3), ylab="true theta", xlab="factor analysis scores")
abline(a=0, b=1, col=2, lwd=2)

cor(fit$scores, theta) ## base factanal output and the true scores
cor(out_scores, theta) ## my function and the true scores
cor(fit$scores, out_scores) ## base factanal output and my function

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
factor_analysis_func_v2 <- function(par, y, iterate=TRUE){
    
    #a <- par[1:3]
    #b <- par[4:6]
    #theta <- par[7:length(par)]
    y[,1] <- (y[,1] - mean(y[,1]))/sd(y[,1])
    y[,2] <- (y[,2] - mean(y[,2]))/sd(y[,2])
    y[,3] <- (y[,3] - mean(y[,3]))/sd(y[,3])
    
    loadings <- par[1:3]
    theta_hat <<- t(loadings) %*% solve(cov(y)) %*% t(y)
    
    theta_hat <<- (theta_hat - mean(theta_hat))/sd(theta_hat)
    
    y.hat1 <- loadings[1] * theta_hat
    y.hat2 <- loadings[2] * theta_hat
    y.hat3 <- loadings[3] * theta_hat
    
    #y.hat1 <- (y.hat1-mean(y.hat1))/sd(y.hat1)
    #y.hat2 <- (y.hat2-mean(y.hat2))/sd(y.hat2)
    #y.hat3 <- (y.hat3-mean(y.hat3))/sd(y.hat3)
    
    ## sse
    #out <- -sum((y[,1]-y.hat1)^2 + (y[,2]-y.hat2)^2 + (y[,3]-y.hat3)^2)
    
    ## mle
    out <- sum(log(dnorm(y[,1], mean=y.hat1, sd=1)) + log(dnorm(y[,2], mean=y.hat2, sd=1)) + log(dnorm(y[,3], mean=y.hat3, sd=1)))    
    return(out)
}

## optim methods: "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
out <- optim(par = parameters_list, fn=factor_analysis_func_v2, y=x, method="BFGS", control=list(fnscale = -1), hessian = TRUE)
out
names(out)
out$par

theta_hat

## this doesn't work because you need to standardize x
plot(t(out$par) %*% solve(cov(x)) %*% t(x), theta_hat)
plot(t(out$par) %*% solve(cov(x)) %*% t(x), theta)

## this works because we are standardizing x
x_standardized <- scale(x)[,1:3]
plot(t(out$par) %*% solve(cov(x_standardized)) %*% t(x_standardized), theta)
abline(a=0, b=1, col=2, lwd=2)
cor(c(t(out$par) %*% solve(cov(x_standardized)) %*% t(x_standardized)), theta)

## these values should be exactly the same up to some rounding errors (theta_hat was made by the second function)
plot(t(out$par) %*% solve(cov(x_standardized)) %*% t(x_standardized), theta_hat)
cor(c(t(out$par) %*% solve(cov(x_standardized)) %*% t(x_standardized)), c(theta_hat))

cor(fit$scores, theta) ## base factanal output and the true scores
cor(out_scores, theta) ## my function and the true scores
cor(fit$scores, out_scores) ## base factanal output and my function





## using the R help example

## function is not generalized to do this yet

# A little demonstration, v2 is just v1 with noise,
# and same for v4 vs. v3 and v6 vs. v5
# Last four cases are there to add noise
# and introduce a positive manifold (g factor)
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 1) # varimax is the default
factanal(m1, factors = 1, rotation = "promax")


out <- optim(par = parameters_list, fn=factor_analysis_func_v2, y=m1, method="BFGS", control=list(fnscale = -1), hessian = TRUE)
out
names(out)
out$par
