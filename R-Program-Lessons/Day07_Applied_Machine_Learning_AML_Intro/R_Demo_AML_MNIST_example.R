## R_Demo_AML_MNIST_example.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-13
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##
##########################################################################
## Introduction to tutorial:
##
## Fit and evaluate a neural network model to the MINAS hand written digit dataset.
##
## https://keras.rstudio.com/index.html
## https://keras.rstudio.com/articles/faq.html
## https://keras.io/losses/
## https://cran.r-project.org/web/packages/keras/keras.pdf
## https://cran.rstudio.com/web/packages/keras/vignettes/sequential_model.html
##
##########################################################################

## load package
install.packages("keras")
library(keras)
#mnist <- dataset_mnist()

## OR

## load MNIST data from dslabs package
library(dslabs)
mnist <- read_mnist()

## or just load it
##load("mnist.Rdata")

## describe the structure of the data
names(mnist)
names(mnist$train)
names(mnist$test)


## transform the data
x_train <- mnist$train$images/255
x_test <- mnist$test$images/255
y_test <- mnist$test$labels
y_train <- mnist$train$labels

matrix(x_train[1,], nrow=28, ncol=28)

image(x=1:28, y=1:28, matrix(x_train[1,], nrow=28, ncol=28)[1:28,28:1])

par(mfrow=c(3,3), mar=c(.5,.5,.5,.5))
for(i in 1:9){
  image(x=1:28, y=1:28, matrix(x_train[i,], nrow=28, ncol=28)[1:28,28:1], yaxt="n", xaxt="n", xlab="", ylab="", col=gray.colors(100, start = 0.99, end = 0.01))
  grid(nx=28, ny=28, col="black")
}

par(mfrow=c(10,10), mar=c(.5,.5,.5,.5))
for(i in 1:100) image(x=1:28, y=1:28, matrix(x_train[i,], nrow=28, ncol=28)[1:28,28:1], yaxt="n", xaxt="n", xlab="", ylab="", col=gray.colors(100, start = 0.99, end = 0.01))

## stretch out the matrix into one visual row
par(mfrow=c(1,1), mar=c(.5,.5,.5,.5))
image(x=1:784, y=1, z=matrix(x_train[1,], nrow=784, ncol=1), yaxt="n", xaxt="n", xlab="", ylab="", col=gray.colors(100, start = 0.99, end = 0.01), ylim=c(-5,5))

## stretch out the matrix into nine visual row
par(mfrow=c(1,1), mar=c(.5,.5,.5,.5))
image(x=1:784, y=1:9, z=t(x_train[9:1,1:784]), yaxt="n", xaxt="n", xlab="", ylab="", col=gray.colors(100, start = 0.99, end = 0.01))

## stretch out the matrix into 100 visual row
par(mfrow=c(1,1), mar=c(.5,.5,.5,.5))
image(x=1:784, y=1:100, z=t(x_train[100:1,1:784]), yaxt="n", xaxt="n", xlab="", ylab="", col=gray.colors(100, start = 0.99, end = 0.01))


## calculate sum for a boxplot
x_train_sum <- apply(x_train, 1, sum)

#y_train_value <- apply(y_train, 1, which.max) - 1

## simple prediction
par(mar=c(5,4,1,1))
boxplot(x_train_sum ~ y_train)
