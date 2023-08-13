## R_Demo_text_as_data_twitteR_get_tweeter_users.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2022
##
## Date: 2022-08-08
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


## (1a) Go to twitter's developer page: https://developer.twitter.com/
## (1b) Click to create an App
## (1c) For the desription, you can say something like this "learn to use twitteR API for a college data science course at the University of Michigan."
##
## (2) Once you have succefully setup you APP, you will be able to get the four strings that you will fill in below
##
## Take note of the following values from the Twitter app page: "API key", "API secret", "Access token", and "Access token secret".
##
## set keys and tokens to access the twitter API
consumer_key <- "your_consumer_key"
consumer_secret <- "your_consumer_secret"
access_token <- "your_access_token"
access_secret <- "your_access_secret"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

## (3) Run the following tweet to see if you successfully created your account
amnestyusa_tweets <- userTimeline('amnestyusa', n=100)
##
## (4) Pick a tweeter account from your country of interest (or you can use the amnestyusa account from above)
##
## (5) Make a DTM (Document-by-Term matrix) for the first 100 tweets from the account you have selected or the amnestyusa account
##
## (6a) Which words are most commonly used in the corpus of 100 tweets?
## (6b) Which words are the least commonly used in the corpus of 100 tweets?
## (6c) Create a barplot of the most frequent words


##########################################################################
## additional examples:
##
## grab the most recent 100 tweets from Barak Obama
Obama_ut <- userTimeline('barackobama', n=100)

## grab the max number of tweets for the President of Senegal Macky Sall
Macky_Sall_ut <- userTimeline('Macky_Sall', n=3200)
