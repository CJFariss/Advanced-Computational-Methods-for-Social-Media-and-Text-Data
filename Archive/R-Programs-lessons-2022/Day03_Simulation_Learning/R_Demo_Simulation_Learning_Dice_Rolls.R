## R_Demo_Simulation_Learning_Estimate_Mean.R
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
##
##########################################################################
## For this R tutorial, we will learn how:
##
## (1) Simulate the roll of a D6 dice
## (2) Repeatedly simulate the roll of a D6 dice and see how close the average value is to the expected value. 
## (3) Learn about the central limit theorem from the simulation.
##
## Notes: The Central Limit Theorem (CLT) establishes that when independently generated variables (iid: independent and identically distributed random variables) are added together, the sums or averages of these variables (when normalized) converge towards a normal distribution. 
##
## This property emerges even if the original variables are not individually normally distributed, as with the roll of a die. 
##
## The probability of any value from the single roll of die is equivalent to any other value for the same-sided die in the limit (when the number of rolls approaches infinity).
## 
##########################################################################


library(MASS)


## simulate 20 randomly generated rolls from a D6 (6-sided-die)
sample(1:6, size=20, replace=TRUE)

## true mean is 3.5
(1 + 2 + 3 + 4 + 5 + 6) / 6

## or 
mean(1:6)

## true variance is approximately 2.916667 or exactly 70/24 
(1 - 3.5)^2 * (1/6) + (2 - 3.5)^2 * (1/6) + (3 - 3.5)^2 * (1/6) + (4 - 3.5)^2 * (1/6) + (5 - 3.5)^2 * (1/6) + (6 - 3.5)^2 * (1/6)

## or
sum((1:6 - mean(1:6))^2 * (1/6))

## repeat the simulation 10,000 times and calculate the average
n_sims <- 2000

## number of samples to roll each iteration
n_samples <- 10

## create two objects to hold the calculated mean and variance from each simulated sample
sim_mean_values <- c()
sim_var_values <- c()

## iterate/repeat the simulation n_sims times
for(i in 1:n_sims){
  
  ## create output
  sample_output <- sample(1:6, size=n_samples, replace=TRUE)
  
  ## save the output in the i_th position of the objects
  sim_mean_values[i] <- mean(sample_output)
  sim_var_values[i] <- var(sample_output)

}

## calculate the mean and variance of the 10,000 sample means
mean(sim_mean_values)
mean(sim_var_values)

## set graphical parameters
par(mfrow=c(1,2), mar=c(4,3,1,1))

## plot histograms 
truehist(sim_mean_values, main="Mean Estimate")
truehist(sim_var_values, main="Variance Estimate")


## calculate and plot the converging average using increasing sample sizes starting at 1 and ending at all the samples
## set graphical parameters
par(mfrow=c(1,2))

##
plot(0,0, ylim=c(2.5,4), xlim=c(0,n_sims), type="n", main="Mean Estimate")
value <- c()
for(i in 1:n_sims){
  value[i] <- mean(sim_mean_values[1:i])
}
lines(value)
abline(h=3.5, col="orange", lwd=2, lty=2)

##
plot(0,0, ylim=c(2.5,4), xlim=c(0,n_sims), type="n", main="Variance Estimate")
value <- c()
for(i in 1:n_sims){
  value[i] <- mean(sim_var_values[1:i])
}
lines(value)
abline(h=2.916667, col="orange", lwd=2, lty=2)


sqrt(2.916667)

## the values converge towards normality 
summary((sim_mean_values - 3.5))
mean((sim_mean_values - 3.5))
var((sim_mean_values - 3.5))



var(sim_mean_values)
var(sim_var_values)



