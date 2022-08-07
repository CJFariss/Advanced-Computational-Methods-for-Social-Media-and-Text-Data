## R_Demo_text_as_data_twitteR_get_tweeter_users.R
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
## Create a Twitter application at http://dev.twitter.com. Make sure to give the app read, write and direct message authority.
## Take note of the following values from the Twitter app page: "API key", "API secret", "Access token", and "Access token secret".

##########################################################################

#install.packages("twitteR")
library(twitteR)

## grab the most recent 100 tweets from Barak Obama
Obama_ut <- userTimeline('barackobama', n=100)

## grab the max number of tweets for the President of Senegal Macky Sall
Macky_Sall_ut <- userTimeline('Macky_Sall', n=3200)
