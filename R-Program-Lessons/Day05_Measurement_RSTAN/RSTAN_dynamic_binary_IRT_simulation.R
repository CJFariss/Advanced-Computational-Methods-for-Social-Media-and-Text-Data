## RSTAN_dynamic_binary_IRT_simulation.R
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


# set time start variable
time1 <- Sys.time()
print(Sys.time() - time1)


# -------------------------------------------------- #
# define STAN model
# -------------------------------------------------- #
model <- "
data {
    int<lower=1> N;
    int<lower=1> T;
    int<lower=1> J;
    int<lower=0> N_T;
    int<lower=0> N_T_J;
    int<lower=1> id[N_T_J];
    int<lower=1> year[N_T_J];
    int<lower=0, upper=J> item[N_T_J];
    int<lower=0, upper=1> y[N_T_J];
}

parameters {
    real alpha[J];
    real<lower=0> beta[J];
    real<lower=0> sigma;
    real theta[N_T];
}

transformed parameters {
    real xb[N_T_J];
    real theta_star[N, T];
    for(i in 1:N_T){
        theta_star[id[i], year[i]] = theta[i];
    }
    for(i in 1:N_T_J){
        xb[i] = alpha[item[i]] + beta[item[i]] * theta_star[id[i], year[i]];
    }
}

model {
    theta ~ normal(0,1); //priors on latent variable
    alpha ~ normal(0,10); //priors (variance not precision)
    beta ~ gamma(4,3);
    sigma ~ uniform(0,1);
    y ~ bernoulli_logit(xb);
}
"
# -------------------------------------------------- #

N <- 30 # units
T <- 10 # time periods
theta <- matrix(NA,nrow=N,ncol=T)

theta[,1] <- rnorm(N,0,1) # random draw for first value
for(t in 2:T){
    theta[,t] <- rnorm(N,theta[,t-1],0.05) # random draw for subsequent time periods
}
theta <- reshape::melt(theta)
alpha1 <- -1.000000
alpha2 <- 0.000000
alpha3 <- 1.000000
beta1 <- 1.000000
beta2 <- 2.000000
beta3 <- 3.000000

# define j as the number of items
#J <- 3 # define it below now

# linear terms of the model
xb1 <- alpha1 + beta1 * theta$value
xb2 <- alpha2 + beta2 * theta$value
xb3 <- alpha3 + beta3 * theta$value

# transform the linear xb terms using the logit function
# so that theta is bound from 0 to 1
eta1 <- 1 / (1 + exp(-xb1))
eta2 <- 1 / (1 + exp(-xb2))
eta3 <- 1 / (1 + exp(-xb3))

# generate the items with theta and measurment error
y1 <- rbinom(N*T, size=1, prob=eta1)
y2 <- rbinom(N*T, size=1, prob=eta2)
y3 <- rbinom(N*T, size=1, prob=eta3)
y <- cbind(y1, y2, y3)

temp <- cbind(theta,y)
head(temp)
temp <- as.data.frame(reshape::melt(temp, id.vars=c("X1","X2", "value")))
names(temp) <- c("id", "year", "theta", "item", "y")
head(temp)


item <- as.numeric(temp$item)
y <- temp$y
J <- max(item)
N_T <- N*T
N_T_J <- N*T*J
id <- as.numeric(temp$id)
year <- as.numeric(temp$year)

# create data list
data_list <- list(y=y, id=id, year=year, item=item, N=N, T=T, J=J, N_T=N_T, N_T_J=N_T_J)

# fit stan model
fit <- stan(model_code = model, data = data_list, iter = 1000, chains = 4)

# this summarizes the named parameters but not along the dimensions
#fit

# extract draws from stan model object
output <- extract(fit, permuted = TRUE)

# print names
names(output)


# this prints the posterior mean for the latent variable
apply(output$theta,2,mean)

# plot true latent variable with posterior mean
par(mar=c(4,4,1,1), font=2, font.lab=2, cex=1.3)
plot(apply(output$theta,2,mean), theta$value, xlim=c(-3,3), ylim=c(-3,3), ylab="true theta", xlab="posterior mean of theta")
abline(a=0, b=1, col=2, lwd=2)


print(Sys.time() - time1)

