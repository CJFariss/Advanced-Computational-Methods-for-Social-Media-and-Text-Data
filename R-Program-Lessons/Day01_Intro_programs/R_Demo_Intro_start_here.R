## R_Demo_Intro_start_here.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-07
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
##
## For this R tutorial, we will learn how:
##  (1) to find and change the working directory,
##  (2) how to create objects in the workspace and save those objects to the working directory
##  (3) how to load libraries with additional functions
##
##########################################################################
## There are many additional resources available online:
##
## "The Book of R" by Tilman M. Davies
## https://nostarch.com/bookofr
##
## "The Art of R Programming" by Norman Matloff
## http://nostarch.com/artofr.htm
##
## Quick-R for SAS/SPSS/Stata Users
## http://www.statmethods.net
##
## UCLA Statistical Computing (R and Stata)
## http://www.ats.ucla.edu/stat/
##
## R Seek (Search for R code)
## http://www.rseek.org/
##
## Simple R Plots
## http://www.phaget4.org/R/plot.html
##
##########################################################################



##########################################################################
## Getting Started
##########################################################################
##
## Note: see Matloff (Ch.1) for more information on the material below
##
## When you open R, you are greeted by a prompt: >
## This is the character that indicates you can type in your command
## Your output will be prefaced by an index for the line number, such as [1]
## The # character is what we call the "comment" character. 
## When you want to write something in your code that you don't want to run, you need to "comment it out."


##########################################################################
## the work space and working directory
##########################################################################

## The working directory is the location where R will look for outside datasets

## what is the current working directory?
getwd()

## set the working directory to an object using the assignment operator <- or = (more on this later)
wd <- getwd()

## print to screen
wd

## set the working directory to another folder (actually the same folder in this example)

wd <- "R-Program-Lessons/Day01_Intro_programs"
setwd(wd)
wd
## see the full path
getwd()

## a generalized way to move up one folder level
setwd("../.")
getwd()

wd <- "/Users/christopherfariss/Documents/GitHub/"
setwd(wd)
## see the full path
getwd()

wd <- "Advanced-Computational-Methods-for-Social-Media-and-Text-Data"
setwd(wd)
## see the full path
getwd()

## another way to figure out which directory/folder that R is looking into is to use the list.files() function
list.files()

## list objects in the working environment
ls()

## remove all objects in the working environment
rm(list = ls())

## list objects in the working environment again
ls()

## getting help

## if you remember the function name use  a single question mark
?rm

## if you cannot remember the function name but remember something close try the double question mark
??remove

## OR you can use the help.search function with quotes for the words inside
## this function is especially useful if you want to search for more than one word
help.search("remove")

## you can also try google, Rseek or Quick R for help:
## R Seek (Search for R code)
## http://www.rseek.org/
##
## Quick-R for SAS/SPSS/Stata Users
## http://www.statmethods.net



##########################################################################
## object naming conventions
##########################################################################

## Objects in R can be named with any letter or LETTER from the alphabet, number, the dot sign ".", or the underscore sign "_"
## Objects cannot start with a number or "_" but can technically start with "."
## No other special characters or reserved operators can be used (e.g., @ $ & + - * or /)
##
## R uses $ in a manner analogous to the way other languages use "." dot.
## We will look at the use of $ in more detail later
## R uses "." dot in a similar way to the underscore
## The underscore character was once the assignment character in S (R's predecessor)
## You can use the underscore character now but more often dot is used instead
## I go back and forth between both but I try to avoide using "." most of the time now in my own code.


dice.roll <- 2
dice_roll <- 2

##########################################################################
## assignment operator
##########################################################################

## there are special functions called operators, which you are well familiar with like the plus sign: "+"

## the operator + takes two numeric arguments on it's left-hand and right-hand side and returns the sum of those two numbers
2+2

## the assignment operator is two symbols "<" the left chevron and the "-" minus sign or dash sign
## put these two symbols together with no space "<-" this is the assignment operator
## the assignment operator is a special function that takes two arguments
## the left-hand side argument is the object that R holds in memory
## the right-hand side argument is any value or function (which returns a value)

## the usage can look like:

## object <- some_value
object <- 2
object

## object <- some_function()
object <- sum(2,2)
object

## some_other_function <- function() ## note that function() is a function that creates a function() which we will talk about in a few weeks 
some_other_function <- function(){}
some_other_function

some_other_function()

##########################################################################
## more examples using the assignment operator
##########################################################################

## create an object and assign a scalar to it (it's really just a vector with one element)
obj1 <- 3

## display the value of the object
obj1

## redefine obj1
4 -> obj1
obj1

obj1 = 2
obj1

## use an object in a calculation
obj1 + 2
obj1 * 2
obj1 ^ 2

## remove just obj1 from memory
rm(obj1)

## removing objects is often not necessary but memory management is sometimes important if your
## code is making new objects from old ones instead of overwriting old objects.
## I had to learn this the hard way.


##########################################################################
## loading other functions from packages/libraries
##########################################################################

## library(LIBRARY) loads a previously installed library
## install.packages("LIBRARY")

## unless you have previously installed this package, the following line should generate an error
library(MASS)

## install the new package
install.packages("MASS")

## load the new package once it is installed by the line above
library(MASS)

## this function updates packages (we won't use this one too much)
#update.packages()

MASS::truehist()

##########################################################################
## save the workspace as a binary file
##########################################################################

obj1 <- 3

## the save() function allows you to save individual objects that are currently loaded in memory
save(obj1, file="Datasets/Myobject.Rdata")

## the save.image() function allows you to save all of the objects that are currently loaded in memory
save.image(file="Datasets/Myworkspace.Rdata")


## we will use other functions to save datasets later in the course but we often do not need to save dataset files. Instead we will practice saving and updating the .R or R-script file (which is this lesson file is an example of).


