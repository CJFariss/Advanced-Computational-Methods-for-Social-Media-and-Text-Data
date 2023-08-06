## RSTAN_binary_IRT_NAs_simulation.R
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
## In the IRT framework, there is a latent trait theta_i.
## Where the subscript i = 1,... ,N indicates multiple units. y_ij is the observed value for item j for unit i. For each item alpha_j and beta_j are also estimated. alpha_j continues to act as "difficulty" parameters, or threshold that benchmarks how likely an indicator is to be observed relative to the values of the latent trait.  In this formulation, this is analogous to an intercept in a traditional logistic regression model.  beta_j, often referred to as the "discrimination" parameters and is the analogue of a slope coefficient.
##
## This program also demonstrates a few different formats for the STAN model statement. It also demonstrates how to deal with missing data in R, prior to passing the data list to STAN for sampling.
##
##########################################################################


library(rstan) # load rstan library


# set time start variable
time1 <- Sys.time()
print(Sys.time() - time1)


# -------------------------------------------------- #
# define STAN model
# -------------------------------------------------- #
model <- "
data {
    int<lower=0> n; // number of subject observations
    int<lower=0> j; // number of items
    int<lower=0> n_j; // number of non-missing item-subject observations
    
    int<lower=0> item[n_j]; // vector of length n_k that distingishes the value of the k item/model parameters
    int<lower=0> id[n_j]; // vector of length n_k that distingishes the value of the n latent variable parameters
    
    //int<lower=0, upper=1> y1[n];
    //int<lower=0, upper=1> y2[n];
    //int<lower=0, upper=1> y3[n];
    
    int<lower=0, upper=1> y[n_j];
}

transformed data{
    
}

parameters {
    vector[j] alpha;
    real<lower=0> beta[j];
    vector[n] theta;
}

transformed parameters {
    vector[n_j] alpha_star; // column vector of alpha parameters
    real<lower=0> beta_star[n_j]; // column vector of beta paramets
    vector[n_j] theta_star; // column vector of latent variable parameters
    vector[n_j] xb; // column vector of linear combination of parameter column vectors
    
    for(i in 1:n_j){
        // loop through parameters vectors to generate column vectors
        alpha_star[i] = alpha[item[i]];
        beta_star[i] = beta[item[i]];
        theta_star[i] = theta[id[i]];
        
        // linear component
        xb[i] = alpha_star[i] + beta_star[i] * theta_star[i];

        //xb[i] = alpha[item[i]] + beta[item[i]] * theta[id[i]];
    }
    
}

model {
    theta ~ normal(0,1); //priors on latent variable
    alpha ~ normal(0,10); //priors (these are variances not precision)
    beta ~ normal(0,10);
    
    // likelihood
    y ~ bernoulli_logit(xb);
}
"
# -------------------------------------------------- #

n <- 100
MISSING <- round(n/10)
MISSING

theta <- rnorm(n,0,1)

alpha1 <- -1.000000
alpha2 <- 0.000000
alpha3 <- 1.000000
alpha4 <- 0.000000

beta1 <- 1.000000
beta2 <- 2.000000
beta3 <- 3.000000
beta4 <- 3.000000

# define j as the number of items
j <- 4

# linear terms of the model
xb1 <- alpha1 + beta1 * theta
xb2 <- alpha2 + beta2 * theta
xb3 <- alpha3 + beta3 * theta
xb4 <- alpha4 + beta4 * theta

# transform the linear xb terms using the logit function
# so that theta is bound from 0 to 1
eta1 <- 1 / (1 + exp(-xb1))
eta2 <- 1 / (1 + exp(-xb2))
eta3 <- 1 / (1 + exp(-xb3))
eta4 <- 1 / (1 + exp(-xb4))

# generate the items with theta and measurment error
y1 <- rbinom(n, size=1, prob=eta1)
y2 <- rbinom(n, size=1, prob=eta2)
y3 <- rbinom(n, size=1, prob=eta3)
y4 <- rbinom(n, size=1, prob=eta4)

# ADD MISSINGNESS TO y1
y1[sample(1:length(y1), size=MISSING)] <- NA
y2[sample(1:length(y2), size=MISSING)] <- NA
y3[sample(1:length(y3), size=MISSING)] <- NA
y4[sample(1:length(y4), size=MISSING)] <- NA

# make a matrix of the item response
y <- cbind(y1, y2, y3, y4)

# make a column vector of the item response with missing values excluded
y_missing <- which(!is.na(y))
summary(y)

y <- y[y_missing]
summary(y)


# scalar for the total number of subjects by observed items
n_j <- length(y_missing)
n_j

# make a column vector of the item numbers with missing values excluded
item <- matrix(c(1:j),ncol=j,nrow=n, byrow=T)
item <- c(item)
item <- item[y_missing]


# make a column vector of the subject ids with missing values excluded
id <- matrix(1:n,ncol=j,nrow=n, byrow=F)
id <- c(id)
id <- id[y_missing]

head(cbind(y, item, id), 20)

# create data list to pass to STAN
data_list <- list(y=y, j=j, n=n, n_j=n_j, item=item, id=id)


# fit stan model
fit <- stan(model_code = model, data = data_list, iter = 1000, chains = 4)


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


print(Sys.time() - time1)

