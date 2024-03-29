## R_Demo_intro_lists_dataframes_tables.R
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
## For this R tutorial, we will learn how:
## (1) learn to work with lists and dataframes and tables
## (2) learn to generate and manipulate these structures just like vectors and matrices
## (3) generate tables and cross-tabulations and understand the difference between them
## (4) practice with old and new functions (all in base) as we manipulate vectors, matrices, lists, dataframes, and tables
##
##########################################################################

## create a list with a single, scalar value
s <- list(2)

## print the list object s to screen
s

## print the first element (this is a list) ## DON'T DO THIS
s[1]

## print the first element of the list (this a numeric scalar in a list)
s[[1]]

## repeat this process for 2 scalars in the list
s <- list(2:6,4)
s

## print the first element (this is a list)
s[1]

## print the first element of the list (this a numeric scalar in a list)
s[[1]]

s[[2]]


## s is a list with a two elements, one vector and one single scalar values,
## it list of length 2 with a vector of length 1 as the first element of the list and of length 1 at the second element of the list

## length of the object s
length(s)

## length of the first element of the list s which is a vector of length 1
length(s[[1]])

## create a list of length 5 that contains 5 vectors of length 1
v_list <- list(1, 2, 3, 4, 5)
v_list

length(v_list)
unlist(v_list)

## create a list of length 1 that contains 1 vector of length 5
v_list <- list(c(1, 2, 3, 4, 5))
v_list

length(v_list)
unlist(v_list)

## calculate the average of the vector (hint: these won't work)
mean(v_list)
sum(v_list)/5
sum(v_list)/length(v_list)

## these will once we unlist() the lists to make a numeric vector
mean(unlist(v_list))
sum(unlist(v_list))/5
sum(unlist(v_list))/length(unlist(v_list))

## we can take the min or max of the vector too
min(unlist(v_list))
max(unlist(v_list))

## column bind using the cbind() function
## row bind using the rbind() function
## these two functions let us stack vectors together

## let's create a new numeric vector
a <- c(1,2,5.3,6,-2,4)

## stack the colums together as columns and as rows
cbind(a,a)
rbind(a,a)

## make a dataframe out of the two vectors (the object will look visually like the  output from cbind)
data.frame(a,a)


## let's make two vectors of time and
## stop and frisk data is from New York City for 2003-2012
stop_and_frisks <- c(77704, 155033, 196570, 267468, 243766, 275588, 310611, 315611, 350743, 284229)
year <- 2003:2012

stop_and_frisks
year

## make a simple plot
plot(year, stop_and_frisks)

## make the same plot with several additional arguments to make the plot more interesting and informative
plot(year, stop_and_frisks,
    ylim=c(0, 1.1*max(stop_and_frisks)),
    col="navy",
    bg="lightblue",
    pch=21,
    xlab="Year",
    ylab="Total Stop and Frisks",
    main="Stop and Frisks for African Americans Over Time"
)


## make a dataframe object
dat <- data.frame(year, stop_and_frisks)

dat

## lets remove the original vectors before we make the plot
rm(year, stop_and_frisks)

## make a simple plot (this won't work now)
plot(year, stop_and_frisks)

dat$year
dat$stop_and_frisks

## make the same plot with several additional arguments using the new dataframe we created
plot(dat$year, dat$stop_and_frisks,
    ylim=c(0, 1.1*max(dat$stop_and_frisks)),
    col="navy",
    bg="lightblue",
    pch=21,
    xlab="Year",
    ylab="Total Stop and Frisks",
    main="Stop and Frisks for African Americans Over Time"
)

## print the data frame
dat

##
dat[,1]
dat[,2]
dat$year
dat$stop_and_frisks

## hard coded in from ourworldindata.org
life_expectancy <- c(76.812, 76.962, 77.124, 77.299, 77.486, 77.685, 77.892, 78.101, 78.303, 78.49, 78.652, 78.777, 78.862, 78.909, 78.922, 78.91, 78.885, 78.861, 78.851, 78.862)
year <- 2000:2019

## make a simple plot
plot(year, life_expectancy)

## make a dataframe object
dat <- data.frame(year, life_expectancy)
dat

## make a list object
dat_list <- list(year, life_expectancy)
dat_list

## print to screen the dat_list as a dataframe
data.frame(dat_list)

## add names to the elements of the list
dat_list <- list(year=year, life_expectancy=life_expectancy)
dat_list

## print to screen the dat_list as a dataframe
data.frame(dat_list)

## list the variables in mydata
names(dat)

## dimensions of an object
dim(dat)

## other commands to look at the dimensions of an object
nrow(dat)
ncol(dat)
dim(dat)[1]
dim(dat)[2]


## look at the first 6 rows in the data set
head(dat)

## look at the first 10 rows in the data set
head(dat, n=10)

## look at the last 6 rows in the data set
tail(dat)

## alternative syntax to look at rows in a data set
dat[1:10,]

dat[1:10, 1:2]

## return summary statistics from a dataframe
summary(dat)

## return the mean of one of the variable from the dataset
mean(dat$life_expectancy)


##########################################################################
## lots of ways to load different data formats (we will learn more about these later):
##########################################################################
##?load
##?read.csv
##?read.table
##?read.delim
##?read.dta
##?read.spss
##?readline

## read data from the current working directory
social_media_data <- read.csv("users-by-social-media-platform.csv", header=TRUE)

## print the dimension of the dataframe
dim(social_media_data)

## print the names of the columns in the dataframe
names(social_media_data)

## print the first 6 rows of the dataframe to screen
head(social_media_data)

## print the last 6 rows of the dataframe to screen
tail(social_media_data)

unique(social_media_data$Entity)

##########################################################################
## lets creates some cross-tabs of categorical information in the dataframe
## we will use table() and xtabs() to make the cross tabs which are just tables
##########################################################################

## tabulate the number of rows for each category of Entity using two different functions
table(social_media_data$Entity)
xtabs( ~ Entity, data= social_media_data)

## tabulate the number of rows for each category of Year using two different functions
table(social_media_data$Year)
xtabs( ~ Year, data= social_media_data)

## cross-tabulate the number of rows for each combination of category of Entity and Year combination
table(social_media_data$Entity, social_media_data$Year)
xtabs( ~ Entity + Year, data= social_media_data)

## so far we have only counted the number of units for one or two categorical combinations
## use the cross-tabs function to count the number of values contained in each Entity by Year combination
xtabs(monthly_active_users ~ Entity, data= social_media_data)

## save the cross-tabs as a vector for making a nice plot
tabs <- xtabs(monthly_active_users ~ Entity, data= social_media_data)

## tabs is a table
tabs

## we can prove this using the is.table() function
is.table(tabs)

## we can also prove that tabs is numeric or that it contains only numeric information
is.numeric(tabs)

## we can also prove that the object tabs is NOT a matrix
is.matrix(tabs)

## let's use tabs to make and adjust a simple barplot (we will do more of this later in the course)
barplot(tabs)
barplot(tabs, las=2)
barplot(tabs[order(tabs)], las=2)
barplot(tabs[order(tabs)], las=2, main="Number of people using social media platforms, 2018")


##########################################################################
## let's subset the dataframe
## i.e., only use some of the rows and/or (by really OR) some of the columns
##########################################################################

## which() values in the dataset are for Facebook?
which(social_media_data$Entity=="Facebook")

## which() values in the dataset are for Youtube?
which(social_media_data$Entity=="YouTube")

## print only values for facebook
social_media_data[which(social_media_data$Entity=="Facebook"),]

## print only values for facebook using the subset function
subset(social_media_data, Entity=="Facebook", select=c(Year, monthly_active_users))



