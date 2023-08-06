## R_Demo_Intro_functions.R
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
## Introduction to tutorial:
##
## For this R tutorial, we will learn how:
## (1) learn to write functions using the function called function
## (2) identify the return() value of a function
##
##########################################################################


## define an empty function called function_name
## we use the function called function (which is a little bit confusing)
function_name <- function(){
    return()
}


## print the function definition to the screen
function_name

function_name()

## the function called function makes a new function instead of an object
## this is the first time we have seen something other than an object created on the left hand side of the <- assignment operator


##########################################################################
## let's make some functions that actually return something of use
##########################################################################

## define a function to create an object out of the value given to it as its argument
## this is a one-to-one mapping function
one_to_one_mapping <- function(value){
    return(value)
}
one_to_one_mapping
one_to_one_mapping(1)
one_to_one_mapping(2)
one_to_one_mapping("this may be dumb")
one_to_one_mapping(TRUE)
one_to_one_mapping("Hello world!")


output <- one_to_one_mapping(1)
output

##########################################################################
## Notes:
## This function is probably not that useful but it illustrates an important concept for functions
## Important concept: functions take input and return output.
## In this example the input and the output are the same
##########################################################################


## This simple function can take a more complex object and return it
## This will come in handy later on, see especially the program challenges
one_to_one_mapping(list(2,TRUE,"message in a list, which is kind of like a message in a bottle"))


##########################################################################
## more function examples
##########################################################################

## define a function for addition of two numbers
## the arguments a and b should be numeric scalars or vectors
add_numbers <- function(a,b){
    a+b
}

## this function and the version above will both return a+b
## explictly defining the object to return is good programming practice so we will build on this version from here on out
add_numbers <- function(a,b){
    return(a+b)
}

## print the function definition to the screen
add_numbers

add_numbers(2,2)

add_numbers(a=2,b=2)

add_numbers(2,1:10)


## define a function to add or subtract two numbers
add_numbers <- function(a,b,sign){
    if(sign=="+") return(a+b)
    if(sign=="-") return(a-b)
}

add_numbers(2,2, sign="+")
add_numbers(2,2, sign="-")


## if one of the arguments is missing, an error will occur
add_numbers(2,2)


## define a function to add or subtract two numbers with a warning for the sign argument
add_numbers <- function(a,b,sign){
    if(missing(sign)) return("missing sign argument") ## print a warning
    if(sign=="+") return(a+b)
    if(sign=="-") return(a-b)
}

## call the function
add_numbers(2,2)


## define a function to add or subtract two numbers with a warning for the sign argument
add_numbers <- function(a,b,sign){
    if(missing(sign)){
        print("warning: missing sign argument, set to + by default") ## print a warning
        sign <- "+"
    }
    if(sign=="+") return(a+b)
    if(sign=="-") return(a-b)
}

## call the function
add_numbers(2,2)






