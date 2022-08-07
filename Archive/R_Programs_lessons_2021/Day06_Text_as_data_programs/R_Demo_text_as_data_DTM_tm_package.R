#### R_Demo_text_as_data_DTM_tm_package.R
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
##
## Introduction to tutorial:
##
## This tutorial replicates the processes demonstrated in the R_Demo_text_as_data_DTM.R file.
##
## This code uses functions available in the tm package.
##
## The code is much much faster at processing the text data though some of the steps are difficul to lean from these functions.
##
##########################################################################

## load libraries
library(stm)
library(tm)
library(SnowballC)

## load data
data <- read.csv("SIMpoliticalTweets.txt", header=FALSE)
data
names(data) <- "text"
data

#trumptweets <- fromJSON("trump_json_files_20190707.txt")
#data <- trumptweets

##
##data <- data[1:1000,]

## create character vector for processing
newtext <- as.character(data$text)
length(newtext)


## use gsub to remove special characters that usually cause errors if left for later
newtext <- gsub("[^0-9A-Za-z///' ]", "", newtext)
newtext <-  gsub("[^[:alnum:]///' ]", "", newtext)
newtext <- gsub("[^\x20-\x7F\x0D\x0A]", "", newtext) # remove all non-ascii characters
newtext <- gsub("http.*", "", newtext) # replace all of the urls
newtext <- gsub("www.*", "", newtext) #

## data$newtext
data$newtext <- newtext

## convert to corpus object using additional functions from the tm package
## the tm_map function takes as its first argument the vector of text and a function as its second argument
corpus <-Corpus(VectorSource(newtext))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

## print to screen
inspect(corpus[1:10])


## make document by term matrix
DTM <- DocumentTermMatrix(corpus)
DTM


## print DTM to screen
inspect(DTM)
inspect(DTM[1:10,1:25])

