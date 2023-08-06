## RSTAN_binary_IRT_simulation.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: A Crash Course in Measurement and Latent Variable Modeling with RSTAN
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
##
##########################################################################
##
## Introduction to tutorial:
##
##For this R tutorial we will simulate 3 binary dependent variables or items and then estimate the units-specific parameters (the latent variable) and item-specific parameters (the difficulty and discrimiation parameters) that generate the variable.
##
## In the IRT framework, there is a latent trait theta_i.
## Where the subscript i = 1,... ,N indicates multiple units. y_ij is the observed value for item j for unit i. For each item alpha_j and beta_j are also estimated. alpha_j continues to act as "difficulty" parameters, or threshold that benchmarks how likely an indicator is to be observed relative to the values of the latent trait.  In this formulation, this is analogous to an intercept in a traditional logistic regression model.  beta_j, often referred to as the "discrimination" parameters and is the analogue of a slope coefficient.
##
##########################################################################

## load library
library(rstan) # load rstan library
library(MASS) # loaed library with truehist function

## -------------------------------------------------- ##
## define STAN model
## -------------------------------------------------- ##
model <- "
data {
    // declared the data in memory
    int<lower=0> n;
    int<lower=0> j;
    int<lower=0, upper=1> y1[n];
    int<lower=0, upper=1> y2[n];
    int<lower=0, upper=1> y3[n];
}
parameters {
    // declared the parameters in memory
    vector[j] alpha;
    real<lower=0> beta[j];
    vector[n] theta;
}
transformed parameters {
    
}
model {
    // priors (these are variances not precision)
    theta ~ normal(0,1); //priors on latent variable
    
    alpha ~ normal(0,10); //priors for the intercepts (these are variances not precision)
    beta ~ normal(0,10); // priors for the slope. This is truncated so that the lowest possible value is 0
    
    // likelihood (link data to some combination of parameters and more data)
    // one equation for each of the observed items
    y1 ~ bernoulli_logit(alpha[1] + beta[1] * theta);
    y2 ~ bernoulli_logit(alpha[2] + beta[2] * theta);
    y3 ~ bernoulli_logit(alpha[3] + beta[3] * theta);
}
"
## -------------------------------------------------- #

## simulated data
n <- 100
theta <- rnorm(n,0,1)
##theta <- runif(n,-3,3)

## set parameters for each item
## alpha (the intercept) is the difficulty parameter or base-line probability of 1
## beta (the slope) is the discrimination parameter or the strength of the relationship
## between the estiamted latent trait theta and the indvidiaul item

## item 1 is the most difficult item (it is centered over the +1/+2 on the latent trait)
alpha1 <- -2.000000
##alpha1 <- -1.000000
beta1 <- 1.000000

## item 2 is of medium difficulty relative to the two other items
alpha2 <- 0.000000
beta2 <- 1.000000

## item 3 is the least difficult item but is also the most informative (it is centered over the -1 on the latent trait)
alpha3 <- 0.000000
beta3 <- 10.000000

## define j as the number of items
j <- 3

## linear terms of the item specific models f() that link the latent trait to the items
xb1 <- alpha1 + beta1 * theta
xb2 <- alpha2 + beta2 * theta
xb3 <- alpha3 + beta3 * theta

## transform the linear xb terms using the logit function
## so that theta is bound from 0 to 1
pi_1 <- 1 / (1 + exp(-xb1))
pi_2 <- 1 / (1 + exp(-xb2))
pi_3 <- 1 / (1 + exp(-xb3))

## generate the items with theta and measurment error
y1 <- rbinom(n, size=1, prob=pi_1)
y2 <- rbinom(n, size=1, prob=pi_2)
y3 <- rbinom(n, size=1, prob=pi_3)

## create matrix of observed items
y <- cbind(y1, y2, y3)

## create data list with each of the observed items
data_list <- list(y1=y1, y2=y2, y3=y3, j=j, n=n)

## fit stan model
fit <- stan(model_code = model, data = data_list, iter = 1000, chains = 4)

## this summarizes the named parameters but not along the dimensions
fit

## extract draws from stan model object
output <- extract(fit, permuted = TRUE)

## print names
names(output)

## this prints the posterior mean for the latent variable
apply(output$theta,2,mean)


## calculate the mean the posterior for the latent variable
latentmean <- apply(output$theta,2,mean)

## plot true latent variable with posterior mean
par(mar=c(4,4,1,1), font=2, font.lab=2, cex=1.3)
plot(latentmean, theta, xlim=c(-3,3), ylim=c(-3,3), ylab="true x", xlab="posterior mean of x")
abline(a=0, b=1, col=2, lwd=2)


## correlate the true latent variable and the additive scale
cor(theta,(y1+y2+y3), method="spearman")

## correlate the estimated latent variable and the additive scale
cor(latentmean,(y1+y2+y3), method="spearman")

## correlate the estimated latent variable and the true latent variable
cor(latentmean,theta, method="spearman")

## how do these correlations compare when the true alpha parameters are spaced equally from one another and when they are not?
