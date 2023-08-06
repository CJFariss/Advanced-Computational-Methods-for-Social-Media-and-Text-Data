## R_Demo_Simulation_Learning_Estimate_Mean.R
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
##
##########################################################################
## Introduction to tutorial:
##
## (1) This tutorial estimates the mean value from a vector of numeric data using the notion of distance.
## 
## Instead of calculating the mean directly, we will estimate the mean by minimizing a function that compares a range of possible estimates of the mean to the data we obtained in the vector (i.e., distance between possible estimate and each of the data points).
##
## The program below finds a range of possible values.
## These values include the maximum value of the log-likelihood which summarizes the disagreement/distance between a potential estimate of the mean and each of the observed data points in the data vector.
##
## This algorithm will provide the same answer as the algorithm for finding the average, which is taught to each of us in grade school.
##
## We will consider several loss-statistics. 
##
## These loss-statistics simply calculate the distance between a parameter estimate or our best guess AND a set of data points. 
##
## We are calculating distances for each observation in a dataset and summarizing these distances. 
## Sometimes we will call these distances something else: usually error or sometimes difference. 
## We will consider another loss function, gradient descent, later in the course.
##
##########################################################################

x <- 1:5
x


## load packages
pkgs <- c("MASS")
invisible(sapply(pkgs, require, character.only = TRUE))


## define a function that calculates the using the normal distribution function
normal_density <- function(data, mu, sigma){
    1/(sigma*sqrt(2*pi)) * exp(-(data - mu)^2/2*sigma^2)
}

## call the user defined density function
normal_density(data=x, mu=1, sigma=1)
normal_density(data=x, mu=2, sigma=1)
normal_density(data=x, mu=3, sigma=1)
normal_density(data=x, mu=4, sigma=1)
normal_density(data=x, mu=100, sigma=1)

## evaluate the sum of the log-likelihood when we guess that mu is 2
mu_hat <- 2
sum(log(1/(sigma*sqrt(2*pi)) * exp(-(x - mu_hat)^2/2*sigma^2)))

sum(log(normal_density(data=x, mu=mu_hat, sigma=1)))

sum(log(dnorm(x, mean=mu_hat, sd=1)))

## evaluate the sum of the log-likelihood when we guess that mu is 3
mu_hat <- 3
sum(log(1/(sigma*sqrt(2*pi)) * exp(-(x - mu_hat)^2/2*sigma^2)))

### evaluate the sum of the log-likelihood when we guess that mu is 4
mu_hat <- 4
sum(log(1/(sigma*sqrt(2*pi)) * exp(-(x - mu_hat)^2/2*sigma^2)))

## evaluate the sum of the log-likelihood when we guess that mu is 5
mu_hat <- 5
sum(log(1/(sigma*sqrt(2*pi)) * exp(-(x - mu_hat)^2/2*sigma^2)))

mu_hat <- 20
sum(log(1/(sigma*sqrt(2*pi)) * exp(-(x - mu_hat)^2/2*sigma^2)))

## print x to screen
x

## define an empty vector
dens <- c()

## define a sequence of values
MU <- seq(-5,5,.1)
for(i in 1:length(MU)){
    mu_hat <- MU[i]
    dens[i] <- sum(log(1/(sigma*sqrt(2*pi)) * exp(-(x - mu_hat)^2/2*sigma^2)))
}
par(mfrow=c(1,1))
plot(MU, dens)

## what is the best mu_hat? Is this average of the vector of data?

##########################################################################
## apply the density function and the equation for the log-likelihood, which is a loss function
##########################################################################

## make a very simple dataset
simple_data <- 1:5

## print the data
simple_data


##########################################################################
## what is the density for each value in the dataset if we assume mu, the average, is 1?
normal_density(data=simple_data, mu=1, sigma=1)

## what is the density for each value in the dataset if we assume mu, the average, is 2?
normal_density(data=simple_data, mu=2, sigma=1)

## what is the density for each value in the dataset if we assume mu, the average, is 3?
normal_density(data=simple_data, mu=3, sigma=1)

## what is the density for each value in the dataset if we assume mu, the average, is 4?
normal_density(data=simple_data, mu=4, sigma=1)

## what is the density for each value in the dataset if we assume mu, the average, is 4?
normal_density(data=simple_data, mu=4, sigma=1)

##########################################################################
## note: we can summarize these values by using the loss function defined above, which we called the log-likelihood
##########################################################################

## we can multiple all the densities together to get the likihood statistic
prod(normal_density(data=simple_data, mu=1, sigma=1))
prod(normal_density(data=simple_data, mu=2, sigma=1))
prod(normal_density(data=simple_data, mu=3, sigma=1))
prod(normal_density(data=simple_data, mu=4, sigma=1))
prod(normal_density(data=simple_data, mu=5, sigma=1))

## we can take the sum of the all the logged densities to get the log-likihood statistic (we usually multiply by -1 so the we mind the min instead of the max. consistent with the likihood function)
-sum(log(normal_density(data=simple_data, mu=1, sigma=1)))
-sum(log(normal_density(data=simple_data, mu=2, sigma=1)))
-sum(log(normal_density(data=simple_data, mu=3, sigma=1)))
-sum(log(normal_density(data=simple_data, mu=4, sigma=1)))
-sum(log(normal_density(data=simple_data, mu=5, sigma=1)))

## we can transform the log-likelihood into the likelihood statistic by using the exponentiation function exp()
exp(sum(log(normal_density(data=simple_data, mu=1, sigma=1))))
exp(sum(log(normal_density(data=simple_data, mu=2, sigma=1))))
exp(sum(log(normal_density(data=simple_data, mu=3, sigma=1))))
exp(sum(log(normal_density(data=simple_data, mu=4, sigma=1))))
exp(sum(log(normal_density(data=simple_data, mu=5, sigma=1))))

## another commonly used loss statistic is the root-mean-squared-error which we use in later programs because it is a bit easier to think about
sqrt(mean((simple_data-1)^2))
sqrt(mean((simple_data-2)^2))
sqrt(mean((simple_data-3)^2))
sqrt(mean((simple_data-4)^2))
sqrt(mean((simple_data-5)^2))

##########################################################################
## note: that all of these statistics are in agreement that our best guess at the values of the dataset, in terms of the ability of our single guess to reduce the distance between the guess of the single parameter and each data point is the simple average
##########################################################################

##########################################################################
## let's do this via a simulation now
##########################################################################

## specify the number of units
sample_size <- 2000
sample_size

## take a random draw of units given a mean that we as the researcher/programmer/data-scientist
## let's use pi for fun but it can be any number (note that we are not estimating the variance for this exercise)
x <- rnorm(sample_size, mean=pi, sd=1)

truehist(x)


## specify values of mu to check (what happens as we decrease the distance between guesses?)
mu_hat <- seq(0,5,0.001)

## loop through all values of mu_hat and calculate the loglik statistic for each
loglik <- c()
for(i in 1:length(mu_hat)){
    loglik[i] <- -sum(log(normal_density(data=x, mu=mu_hat[i], sigma=1)))
}

plot(mu_hat, loglik, type="l", lwd=2)

## which value is the minimum?
coordinate <- which(loglik==min(loglik))
coordinate

## which coordinate minimizes the loglik for the best mu_hat value?
mu_hat[coordinate]

## what is the calculated average?
mean(x)

# pass function to optim with initial values
loglik_func <- function(par, data){
    -sum(log(normal_density(data=data, mu=par, sigma=1)))
}

optim.out <- optim(par = c(0), fn=loglik_func, data=x, method="BFGS")
optim.out

## estimate mean from the optim function
optim.out$par

## empirical mean
mean(x)

## the difference between the estimated parameter and the empirical mean is vanishingly small
optim.out$par - mean(x)

##########################################################################
## Question: what if we repeated this process over and over again? 
## Would the average of the simulations coverage towards the true value of pi?
##
## Notes: The Central Limit Theorem (CLT) establishes that when independently generated variables (iid: independent and identically distributed random variables) are added together, the sums or averages of these variables (when normalized) converge towards a normal distribution. 
## This property emerges even if the original variables are not individually normally distributed, as with the roll of a die. 
## The probability of any value from the single roll of die is equivalent to any other value for the same-sided die in the limit (when the number of rolls approaches infinity).
##
##########################################################################



