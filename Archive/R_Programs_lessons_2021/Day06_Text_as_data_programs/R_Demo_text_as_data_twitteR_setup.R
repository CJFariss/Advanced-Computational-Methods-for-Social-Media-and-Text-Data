## R_Demo_text_as_data_twitteR_setup.R
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
## setup to access the twitter API using R
##
##########################################################################

install.packages("twitteR")
library(twitteR)

## go to twitter's developer page: https://developer.twitter.com/
## clink to create an App
## for the desription, you can say something like this "learn to use twitteR API"
## once you have succefully setup you APP, you will be able to get the four strings that you will fill in below

## set keys and tokens to access the twitter API
consumer_key <- "your_consumer_key"
consumer_secret <- "your_consumer_secret"
access_token <- "your_access_token"
access_secret <- "your_access_secret"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

setup_twitter_oauth("API key", "API secret")
