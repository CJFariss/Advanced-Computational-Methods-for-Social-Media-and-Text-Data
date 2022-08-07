## R_Demo_Intro_Program_Challenge_two_sum.R
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
## Instructions:
##
## These challenges are meant to be just that, challenging. 
## They should also be fun. I encourage you to think creatively and collaboratively. 
## Getting stuck or not finishing all the steps is expected and encouraged. 
## This is how learning works!
## Learn to program, program to learn.
##
## Always start with step (1) and then continue to each step as time permits.
## Don't worry about completing each step. Document your code for each step.
## You may wish to come back to some of the harder steps as you progress through the course.
## Note that some of the steps may ask you to use skills we have not yet covered in the course.
## Don't worry about these steps now but definitely think through the programming logic if you are stuck and make plans to come back to try them once you feel ready.
##
##########################################################################
##
## Steps for the Challenge
##
## (1) create a vector of integers numbers and a scalar integer target
## (2) write a program in R that determines (returns) two numbers from the vector that add up to the target scalar
## (3) how many combinations of numbers in the vector of integers add up to the target scalar?
## (4) write a function to complete steps  1-3 (hint: wrap the program from (3) within a function)
## (5) write a simulation that explore the relationship between (a) the vector of integers numbers and (b) scalar integer target sum
## (6) re-write the program or function so that it takes fewer steps to calculate the number of numeric combinations that add up to the scalar target
##
##########################################################################

## step 1: create a vector of integers numbers
int_numbers <- 1:10
int_numbers

## step 1: and a scalar integer target
target_number <- 6
target_number

## step 2: write a program in R that determines (returns) two of numbers that add up to the target scalar
count <- 1
count

value <- c()
value

mat <- matrix(NA, nrow=length(int_numbers), ncol=length(int_numbers))
mat

for(i in 1:length(int_numbers)){
    for(j in 1:length(int_numbers)){
        mat[i,j] <- int_numbers[i] + int_numbers[j]
        value[count] <- ifelse(mat[i,j]==target_number, TRUE, FALSE)
        count <- count + 1
    }
}
count
mat
value

## step 3: how many combinations of numbers in the vector of integers add up to the target scalar?
sum(value)
sum(value==TRUE)


## step 4: write a function to complete steps  1-3

sum_func <- function(int_numbers=1:10, target_number=2){
    count <- 1
    value <- c()
    mat <- matrix(NA, nrow=length(int_numbers), ncol=length(int_numbers))
    for(i in 1:length(int_numbers)){
        for(j in 1:length(int_numbers)){
            mat[i,j] <- int_numbers[i] + int_numbers[j]
            value[count] <- ifelse(mat[i,j]==target_number, TRUE, FALSE)
            count <- count + 1
        }
    }
    mat
    value
    return(sum(value==TRUE))
}

## print function definition to screen
sum_func

## use the function with its default arguments
sum_func()

## use the function with other arguments
sum_func(int_numbers=1:10, target_number=6)

sum_func(int_numbers=1:100, target_number=6)

sum_func(int_numbers=1:10, target_number=60)

sum_func(int_numbers=1:100, target_number=60)


## try to figure out step 5 and step 6 here

