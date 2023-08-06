## R_Demo_google_trends_trump_brexit.R
#########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2022
##
## Date: 2022-08-28
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:

## clean up workspace
rm(list = ls(all.names = TRUE))
gc()

## load libraries
library(gtrendsR)
library(countrycode)
library(stm)
library(tm)
library(MASS)
library(colorbrewer)

## country codes
data("countries")
ISO <- as.character(unique(countries$country_code))
COUNTRY <- countrycode(ISO, origin="iso2c", destination="country.name")


COLORS <- c("#fdae61", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")


##
TERMS <- c("Donald Trump", "Brexit")



world <- gtrends(TERMS[c(1,2)])
plot(world)


