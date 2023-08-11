## RSTAN.binary_IRT_NAs_simulation.r
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-11
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
##
##########################################################################
##
## Introduction to tutorial:
##
## In the IRT framework, there is a latent trait theta_i.
## Where the subscript i = 1,... ,N indicates multiple units. y_itj is the observed value for item j for unit i, in t = 1, ... T, tim periods. For each item alpha_j and beta_j are also estimated. alpha_j continues to act as "difficulty" parameters, or threshold that benchmarks how likely an indicator is to be observed relative to the values of the latent trait.  In this formulation, this is analogous to an intercept in a traditional logistic regression model.  beta_j, often referred to as the "discrimination" parameters and is the analogue of a slope coefficient.
##
## The STAN model below adds a dependency into the relationship between the latent variable in time t+1 and time t. It is dynamic because of this dependency. See Schnakenberg and Fariss (2014) or Reuning, Kenwick, Fariss (2019) for more details (see the Papers folder).
##
##########################################################################

library(reshape)
library(rstan) # load rstan library

rm(list = ls())

# set time start variable
start.time <- Sys.time()
print(Sys.time() - start.time)


# -------------------------------------------------- #
# define STAN model
# -------------------------------------------------- #
model <- "
data {
    int<lower=0> n; // number of country_year observations
    int<lower=0> j; // number of items
    int<lower=0> n_j; // number of non-missing item-subject observations
    
    int<lower=0> item[n_j]; // vector of length n_j that distingishes the value of the k item/model parameters
    
    int<lower=0> id[n]; // vector of length n that distingishes the value of the n latent variable parameters
    int<lower=0> time[n];
    int<lower=0> id_time[n_j];
    
    int<lower=0, upper=1> y[n_j];
}

transformed data{
    
}

parameters {
    vector[j] alpha;
    real<lower=0> beta[j];
    vector[n] theta;
    real<lower=0, upper=1> sigma;
}

transformed parameters {
    vector[n_j] xb; // column vector of linear combination of parameter column vectors
    vector[n] theta_star;
    vector[n] sigma_star;
    
    
    // loop through latent variable vector to create the values for the dyanic prior
    for(i in 1:n){
        if(time[i]==1){
            theta_star[i] = 0;
            sigma_star[i] = 1;
        }
        
        else{
            theta_star[i] = theta[i-1];
            sigma_star[i] = sigma;
        }
    }
    
    // loop through parameters vectors to generate column vector
    for(i in 1:n_j){
        xb[i] = alpha[item[i]] + beta[item[i]] * theta[id_time[i]];
    }
    
}

model {
    theta ~ normal(theta_star, sigma_star); //priors on latent variable
    
    alpha ~ normal(0,10); //priors (these are variances not precision)
    beta ~ gamma(4,3);
    sigma ~ uniform(0,1);
    
    y ~ bernoulli_logit(xb);
}
"
# -------------------------------------------------- #

N <- 30 # units
T <- 10 # time periods
x <- matrix(NA,nrow=N,ncol=T)

x[,1] <- rnorm(N,0,1) # random draw for first value
for(i in 1:N){
    for(t in 2:T){
        x[i,t] <- rnorm(1,x[i,t-1],0.05) # random draw for subsequent time periods
    }
}

x <- melt(x)
x <- x[order(x$X1),]

id <- x$X1
time <- x$X2

## rename x to theta here
##theta <- x$value

alpha1 <- -2.000000
alpha2 <- -2.000000
alpha3 <- -1.000000
alpha4 <- -1.000000

beta1 <- 1.000000
beta2 <- 2.000000
beta3 <- 3.000000
beta4 <- 3.000000

# define j as the number of items
j <- 4

# linear terms of the model
xb1 <- alpha1 + beta1 * theta$value
xb2 <- alpha2 + beta2 * theta$value
xb3 <- alpha3 + beta3 * theta$value
xb4 <- alpha4 + beta4 * theta$value

# transform the linear xb terms using the logit function
# so that theta is bound from 0 to 1
eta1 <- 1 / (1 + exp(-xb1))
eta2 <- 1 / (1 + exp(-xb2))
eta3 <- 1 / (1 + exp(-xb3))
eta4 <- 1 / (1 + exp(-xb4))

# generate the items with theta and measurment error
y1 <- rbinom(N*T, size=1, prob=eta1)
y2 <- rbinom(N*T, size=1, prob=eta2)
y3 <- rbinom(N*T, size=1, prob=eta3)
y4 <- rbinom(N*T, size=1, prob=eta4)

# ADD MISSINGNESS TO y1
MISSING <- round(N*T/10)

y1[sample(1:length(y1), size=MISSING)] <- NA
y2[sample(1:length(y2), size=MISSING)] <- NA
y3[sample(1:length(y3), size=MISSING)] <- NA
y4[sample(1:length(y4), size=MISSING)] <- NA

# make a matrix of the item response
y <- cbind(y1, y2, y3, y4)
dim(y)

# make a column vector of the item response with missing values excluded
y_missing <- which(!is.na(y))
y <- y[y_missing]
length(y)


# scalar for the total number of subjects by observed items
n_j <- length(y_missing)
n_j


# make a column vector of the item numbers with missing values excluded
item <- matrix(c(1:j),ncol=j,nrow=N*T, byrow=T)
item <- c(item)

# make a column vector of the subject ids with missing values excluded
id_time <- matrix(1:n,ncol=j,nrow=N*T, byrow=F)
id_time <- c(id_time)


item <- item[y_missing]
id_time <- id_time[y_missing]


## we are defining n here as N*T because we are vectorizing the STAN process
n <- N*T
n


# create data list to pass to STAN
data <- list(y=y, j=j, n=n, n_j=n_j, item=item, id=id, time=time, id_time=id_time)


# fit stan model
fit <- stan(model_code = model, data = data, iter = 2000, chains = 4)


# this summarizes the named parameters but not along the dimensions
fit

# extract draws from stan model object
output <- extract(fit, permuted = TRUE)

# print names
names(output)

# this prints the posterior mean for the latent variable
apply(output$theta,2,mean)


# plot true latent variable with posterior mean
par(mar=c(4,4,1,1), font=2, font.lab=2, cex=1.3)
plot(apply(output$theta,2,mean), theta, xlim=c(-3,3), ylim=c(-3,3), ylab="true x", xlab="posterior mean of x")
abline(a=0, b=1, col=2, lwd=2)


print(Sys.time() - start.time)

