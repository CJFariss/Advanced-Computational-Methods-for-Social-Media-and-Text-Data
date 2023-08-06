## R_Demo_Into_linear_model_predict.R
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
## (1) Demonstrates how to use the perdict() function after fitting a linear model with the lm() function.
##
##########################################################################


## LOAD DATA INTO AN R SESSION
macro <- read.csv("macro.csv", header=TRUE)

## list the variables in mydata
names(macro)


## dimensions of an object
dim(macro)

## other commands to look at the dimensions of an object
nrow(macro)
ncol(macro)
dim(macro)[1]
dim(macro)[2]

## look at the first 6 rows in the data set
head(macro)

## look at the first 10 rows in the data set
head(macro, n=10)

## look at the last 6 rows in the data set
tail(macro)


## alternative syntax to look at rows in a data set
macro[1:10,]
macro[1:10, 1:2]

## look at the unemployment variable called unem that exists in the macro dataset
macro$unem

## calculate the mean and standard deviation for each variable in the data set
sum(macro$unem)/length(macro$unem)
sqrt(sum((macro$unem-mean(macro$unem))^2)/(length(macro$unem)-1))

mean(macro$unem)
sd(macro$unem)

summary(macro)

## generate plots of the distribution of the trade variable
hist(macro$trade, col=grey(_5), xlab="Trade", ylab="Frequency", main="Distribution of Trade")
plot(density(macro$trade), col=2, lwd=3, xlab="Trade", ylab="Density", main="Distribution of Trade")


## select the numeric variables (columns) from the full macro data set
macro_variables <- subset(macro, select=c(gdp, unem, capmob, trade))
dim(macro_variables)
summary(macro_variables)

## select only the observations (rows) that occured in the year 1990 from the full data set
macro_1990 <- subset(macro, year==1990)
dim(macro_1990)
summary(macro_1990)

## select the numeric variables (columns) AND the observations (rows) that occured in the year 1990 from the full macro data set
macro_variables_1990 <- subset(macro,  year==1990, select=c(gdp, unem, capmob, trade))
dim(macro_variables_1990)
summary(macro_variables_1990)


## calculate correlation coefficients for each pair of variables in the macro_variables dataset
cor(macro)

cor(macro_variables)

cor(macro_variables$unem, macro_variables$trade)

cor_test(macro_variables$unem, macro_variables$trade)




##########################################################################
## ESTIMATE A SIMPLE LINEAR MODEL: BIVARIATE REGRESSION
##########################################################################

## linear algrebra for the linear model
## for more information on the syntax for matrix
##algebra in R see http://www_statmethods_net/advstats/matrix_html

X <- cbind(rep(1, 350), macro$trade)
Y <- macro$unem

model_01 <- solve(t(X)%*%X) %*% (t(X)%*%Y)

model_01

## linear model function lm()
model_01 <- lm(macro$unem ~ macro$trade)

model_01

model_01 <- lm(unem ~ trade, data = macro)

model_01

summary(model_01)

##########################################################################
## ESTIMATE A SIMPLE LINEAR MODEL: MULTIPLE REGRESSION
##########################################################################

model_01 <- lm(unem ~ gdp + capmob + trade, data = macro)

model_01

summary(model_01)



##########################################################################
## ESTIMATE SUBSTANTIVE EFFECTS/QUANTITIES OF INTEREST
## USING THE MODEL ESTIMATES
##########################################################################

## generate a sequence from the lowest value of the trade variable to the highest value
values <- seq(from=min(macro$trade), to=max(macro$trade), by=1)

## visually inspect the sequence values
values

## create a new data set that is the length of the sequence created above that contains the variables gdp, unem, capmob, trade
new_macro <- macro[1:length(values), 3:6]

## loop through the new data set and change the values of the four variables
for(i in 1:length(values)){
    
    ## change unem to NA (i_e_, missing)_  this is the y variable that will be predicted below using the model estimates contained in the model_01 object
    new_macro$unem[i] <- NA
    
    ## change every value of the variable gdp and capmob to the mean value of these variables from the original data
    new_macro$gdp[i] <- mean(macro$gdp)
    new_macro$capmob[i] <- mean(macro$capmob)
    
    ## change each value of trade to the values in the sequence e
    new_macro$trade[i] <- values[i]
    
    
}

##########################################################################
## use the predict function
##########################################################################
## see the syntax to predict
?predict

## predict the value unem in the new_macro data object using the coefficients contained in the model_01 object
predict_model_01 <- predict(model_01, new_macro, se.fit=TRUE)

## view the values contained in the new prediction object
predict_model_01

## view the predicted values only
predict_model_01$fit


##########################################################################
## PLOT THE SUBSTANTIVE EFFECTS/QUANTITIES OF INTEREST
##########################################################################

plot(values, predict_model_01$fit, type="n", ylim=c(0,max(macro$unem)), ylab="Predicted Value of Unemployment", xlab="Amount of Trade", main="SWANKY TITLE HERE")

lines(values, predict_model_01$fit, lty=1, lwd=3, col=1)
lines(values, predict_model_01$fit-1.96*predict_model_01$se.fit, lty=2, lwd=3, col=2)
lines(values, predict_model_01$fit+1.96*predict_model_01$se.fit, lty=2, lwd=3, col=2)








