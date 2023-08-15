## R_Demo_google_trends_Human_Rights.R
#########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-13
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
##
## Use the google trends API to access google trends data and generate a graph comparing search terms for a specific geographic and temporal context.
##
## Examples taken from:
## Dancy, Geoff and Christopher J. Fariss. “The Global Resonance of Human Rights: What Google Trends Can Tell Us.” American Political Science Review (Forthcoming).
##
##########################################################################

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



## ------------------------------------------------------------ ##
## English
english.world <- gtrends(TERMS[1])
english.world <- subset(english.world$interest_by_country, !is.na(hits))
english.ISO <- countrycode(english.world$location, origin="country.name", destination="iso2c")

english.world$hits[english.world$hits=="<1"] <- .5
english.world$hits <- as.numeric(english.world$hits)
english.world <- subset(english.world, !is.na(hits))


N <- nrow(english.world)

english.state.list <- lapply(1:N, function(i){
    
    temp <- gtrends(TERMS[1], geo=c(english.ISO[i]))$interest_over_time

    temp$hits[temp$hits=="<1"] <- .5
    temp$hits <- as.numeric(temp$hits)

    temp$hits <- temp$hits * (english.world$hits[i]/100)
    
    return(temp)
})


par(mfrow=c(4,3), mar=c(2,2.5,1,.5))
for(i in 1:length(english.state.list)){
    plot(english.state.list[[i]]$hits, main=english.world$location[i], lwd=1, col=grey(.75), ylim=c(0, 100), xaxt="n", yaxt="n", type="n")
    
    id2013 <- which(as.Date(english.state.list[[i]]$date) > as.Date("2012-12-31") & as.Date(english.state.list[[i]]$date) <= as.Date("2013-12-31"))
    id2014 <- which(as.Date(english.state.list[[i]]$date) > as.Date("2013-12-31") & as.Date(english.state.list[[i]]$date) <= as.Date("2014-12-31"))
    id2015 <- which(as.Date(english.state.list[[i]]$date) > as.Date("2014-12-31") & as.Date(english.state.list[[i]]$date) <= as.Date("2015-12-31"))
    id2016 <- which(as.Date(english.state.list[[i]]$date) > as.Date("2015-12-31") & as.Date(english.state.list[[i]]$date) <= as.Date("2016-12-31"))
    id2017 <- which(as.Date(english.state.list[[i]]$date) > as.Date("2016-12-31") & as.Date(english.state.list[[i]]$date) <= as.Date("2017-12-31"))
    id2018 <- which(as.Date(english.state.list[[i]]$date) > as.Date("2017-12-31") & as.Date(english.state.list[[i]]$date) <= as.Date("2018-12-31"))
    
    polygon(x=c(min(id2013), min(id2013), max(id2013), max(id2013)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2015), min(id2015), max(id2015), max(id2015)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2017), min(id2017), max(id2017), max(id2017)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    box()

lines(english.state.list[[i]]$hits, lwd=1, col="#bdd7e7")
    axis(side=2, at=c(0,25,50,75,100), las=2)
    axis(side=1, at=c(median(id2013), median(id2015), median(id2017)), labels=c(2013, 2015, 2017), las=1)
    model <- bcp(y= english.state.list[[i]]$hits)
    lines(model$posterior.mean, lwd=.75, col="#08519c")
}





## ------------------------------------------------------------ ##
## Spanish
spanish.world <- gtrends(TERMS[2])
spanish.world <- subset(spanish.world$interest_by_country, !is.na(hits))
spanish.ISO <- countrycode(spanish.world$location, origin="country.name", destination="iso2c")

spanish.world$hits[spanish.world$hits=="<1"] <- .5
spanish.world$hits <- as.numeric(spanish.world$hits)
spanish.world <- subset(spanish.world, !is.na(hits))


N <- nrow(spanish.world)
spanish.state.list <- lapply(1:N, function(i){
    
    temp <- gtrends(TERMS[2], geo=c(spanish.ISO[i]))$interest_over_time
    temp$hits[temp$hits=="<1"] <- .5
    temp$hits <- as.numeric(temp$hits)

    temp$hits <- temp$hits * (spanish.world$hits[i]/100)
    
    return(temp)
})



par(mfrow=c(4,3), mar=c(2,2.5,1,.5))
for(i in 1:length(spanish.state.list)){
    plot(spanish.state.list[[i]]$hits, main=spanish.world$location[i], lwd=1, col=grey(.75), ylim=c(0, 100), xaxt="n", yaxt="n", type="n")

    id2013 <- which(as.Date(spanish.state.list[[i]]$date) > as.Date("2012-12-31") & as.Date(spanish.state.list[[i]]$date) <= as.Date("2013-12-31"))
    id2014 <- which(as.Date(spanish.state.list[[i]]$date) > as.Date("2013-12-31") & as.Date(spanish.state.list[[i]]$date) <= as.Date("2014-12-31"))
    id2015 <- which(as.Date(spanish.state.list[[i]]$date) > as.Date("2014-12-31") & as.Date(spanish.state.list[[i]]$date) <= as.Date("2015-12-31"))
    id2016 <- which(as.Date(spanish.state.list[[i]]$date) > as.Date("2015-12-31") & as.Date(spanish.state.list[[i]]$date) <= as.Date("2016-12-31"))
    id2017 <- which(as.Date(spanish.state.list[[i]]$date) > as.Date("2016-12-31") & as.Date(spanish.state.list[[i]]$date) <= as.Date("2017-12-31"))
    id2018 <- which(as.Date(spanish.state.list[[i]]$date) > as.Date("2017-12-31") & as.Date(spanish.state.list[[i]]$date) <= as.Date("2018-12-31"))

    polygon(x=c(min(id2013), min(id2013), max(id2013), max(id2013)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2015), min(id2015), max(id2015), max(id2015)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2017), min(id2017), max(id2017), max(id2017)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    box()
  
    lines(spanish.state.list[[i]]$hits, lwd=1, col="#bdd7e7")
    axis(side=2, at=c(0,25,50,75,100), las=2)
    axis(side=1, at=c(median(id2013), median(id2015), median(id2017)), labels=c(2013, 2015, 2017), las=1)
    model <- bcp(y= spanish.state.list[[i]]$hits)
    lines(model$posterior.mean, lwd=.75, col="#08519c")
}





## ------------------------------------------------------------ ##
## Portugese
portugese.world <- gtrends(TERMS[3])
portugese.world <- subset(portugese.world$interest_by_country, !is.na(hits))
portugese.ISO <- countrycode(portugese.world$location, origin="country.name", destination="iso2c")

portugese.world$hits[portugese.world$hits=="<1"] <- .5
portugese.world$hits <- as.numeric(portugese.world$hits)
portugese.world <- subset(portugese.world, !is.na(hits))


N <- nrow(portugese.world)
portugese.state.list <- lapply(1:N, function(i){
    
    temp <- gtrends(TERMS[3], geo=c(portugese.ISO[i]))$interest_over_time
    temp$hits[temp$hits=="<1"] <- .5
    temp$hits <- as.numeric(temp$hits)

    temp$hits <- temp$hits * (portugese.world$hits[i]/100)
    
    return(temp)
})



par(mfrow=c(4,3), mar=c(2,2.5,1,.5))
for(i in 1:length(portugese.state.list)){
    plot(portugese.state.list[[i]]$hits, main=portugese.world$location[i], lwd=1, col=grey(.75), ylim=c(0, 100), xaxt="n", yaxt="n", type="n")
    
    id2013 <- which(as.Date(portugese.state.list[[i]]$date) > as.Date("2012-12-31") & as.Date(portugese.state.list[[i]]$date) <= as.Date("2013-12-31"))
    id2014 <- which(as.Date(portugese.state.list[[i]]$date) > as.Date("2013-12-31") & as.Date(portugese.state.list[[i]]$date) <= as.Date("2014-12-31"))
    id2015 <- which(as.Date(portugese.state.list[[i]]$date) > as.Date("2014-12-31") & as.Date(portugese.state.list[[i]]$date) <= as.Date("2015-12-31"))
    id2016 <- which(as.Date(portugese.state.list[[i]]$date) > as.Date("2015-12-31") & as.Date(portugese.state.list[[i]]$date) <= as.Date("2016-12-31"))
    id2017 <- which(as.Date(portugese.state.list[[i]]$date) > as.Date("2016-12-31") & as.Date(portugese.state.list[[i]]$date) <= as.Date("2017-12-31"))
    id2018 <- which(as.Date(portugese.state.list[[i]]$date) > as.Date("2017-12-31") & as.Date(portugese.state.list[[i]]$date) <= as.Date("2018-12-31"))
    
    polygon(x=c(min(id2013), min(id2013), max(id2013), max(id2013)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2015), min(id2015), max(id2015), max(id2015)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2017), min(id2017), max(id2017), max(id2017)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    box()
    
    
    lines(portugese.state.list[[i]]$hits, lwd=1, col="#bdd7e7")
    axis(side=2, at=c(0,25,50,75,100), las=2)
    axis(side=1, at=c(median(id2013), median(id2015), median(id2017)), labels=c(2013, 2015, 2017), las=1)
    model <- bcp(y= portugese.state.list[[i]]$hits)
    lines(model$posterior.mean, lwd=.75, col="#08519c")
}






## ------------------------------------------------------------ ##
## Arabic
arabic.world <- gtrends(TERMS[4])
arabic.world <- subset(arabic.world$interest_by_country, !is.na(hits))
arabic.ISO <- countrycode(arabic.world$location, origin="country.name", destination="iso2c")

arabic.world$hits[arabic.world$hits=="<1"] <- .5
arabic.world$hits <- as.numeric(arabic.world$hits)
arabic.world <- subset(arabic.world, !is.na(hits))


N <- nrow(arabic.world)
arabic.state.list <- lapply(1:N, function(i){
    
    temp <- gtrends(TERMS[4], geo=c(arabic.ISO[i]))$interest_over_time
    temp$hits[temp$hits=="<1"] <- .5
    temp$hits <- as.numeric(temp$hits)

    temp$hits <- temp$hits * (arabic.world$hits[i]/100)
    
    return(temp)
})



par(mfrow=c(4,3), mar=c(2,2.5,1,.5))
for(i in 1:length(arabic.state.list)){
    plot(arabic.state.list[[i]]$hits, main=arabic.world$location[i], lwd=1, col=grey(.75), ylim=c(0, 100), xaxt="n", yaxt="n", type="n")
    
    id2013 <- which(as.Date(arabic.state.list[[i]]$date) > as.Date("2012-12-31") & as.Date(arabic.state.list[[i]]$date) <= as.Date("2013-12-31"))
    id2014 <- which(as.Date(arabic.state.list[[i]]$date) > as.Date("2013-12-31") & as.Date(arabic.state.list[[i]]$date) <= as.Date("2014-12-31"))
    id2015 <- which(as.Date(arabic.state.list[[i]]$date) > as.Date("2014-12-31") & as.Date(arabic.state.list[[i]]$date) <= as.Date("2015-12-31"))
    id2016 <- which(as.Date(arabic.state.list[[i]]$date) > as.Date("2015-12-31") & as.Date(arabic.state.list[[i]]$date) <= as.Date("2016-12-31"))
    id2017 <- which(as.Date(arabic.state.list[[i]]$date) > as.Date("2016-12-31") & as.Date(arabic.state.list[[i]]$date) <= as.Date("2017-12-31"))
    id2018 <- which(as.Date(arabic.state.list[[i]]$date) > as.Date("2017-12-31") & as.Date(arabic.state.list[[i]]$date) <= as.Date("2018-12-31"))
    
    polygon(x=c(min(id2013), min(id2013), max(id2013), max(id2013)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2015), min(id2015), max(id2015), max(id2015)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2017), min(id2017), max(id2017), max(id2017)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    box()
    
    
    lines(arabic.state.list[[i]]$hits, lwd=1, col="#bdd7e7")
    axis(side=2, at=c(0,25,50,75,100), las=2)
    axis(side=1, at=c(median(id2013), median(id2015), median(id2017)), labels=c(2013, 2015, 2017), las=1)
    model <- bcp(y=arabic.state.list[[i]]$hits)
    lines(model$posterior.mean, lwd=.75, col="#08519c")
}




## ------------------------------------------------------------ ##
## French
french.world <- gtrends(TERMS[8])
french.world <- subset(french.world$interest_by_country, !is.na(hits))
french.ISO <- countrycode(french.world$location, origin="country.name", destination="iso2c")

french.world$hits[french.world$hits=="<1"] <- .5
french.world$hits <- as.numeric(french.world$hits)
french.world <- subset(french.world, !is.na(hits))


N <- nrow(french.world)
french.state.list <- lapply(1:N, function(i){
    
    temp <- gtrends(TERMS[8], geo=c(french.ISO[i]))$interest_over_time
    temp$hits[temp$hits=="<1"] <- .5
    temp$hits <- as.numeric(temp$hits)
    
    temp$hits <- temp$hits * (french.world$hits[i]/100)
    
    return(temp)
})



par(mfrow=c(4,3), mar=c(2,2.5,1,.5))
for(i in 1:length(french.state.list)){
    plot(french.state.list[[i]]$hits, main=french.world$location[i], lwd=1, col=grey(.75), ylim=c(0, 100), xaxt="n", yaxt="n", type="n")
    
    id2013 <- which(as.Date(french.state.list[[i]]$date) > as.Date("2012-12-31") & as.Date(french.state.list[[i]]$date) <= as.Date("2013-12-31"))
    id2014 <- which(as.Date(french.state.list[[i]]$date) > as.Date("2013-12-31") & as.Date(french.state.list[[i]]$date) <= as.Date("2014-12-31"))
    id2015 <- which(as.Date(french.state.list[[i]]$date) > as.Date("2014-12-31") & as.Date(french.state.list[[i]]$date) <= as.Date("2015-12-31"))
    id2016 <- which(as.Date(french.state.list[[i]]$date) > as.Date("2015-12-31") & as.Date(french.state.list[[i]]$date) <= as.Date("2016-12-31"))
    id2017 <- which(as.Date(french.state.list[[i]]$date) > as.Date("2016-12-31") & as.Date(french.state.list[[i]]$date) <= as.Date("2017-12-31"))
    id2018 <- which(as.Date(french.state.list[[i]]$date) > as.Date("2017-12-31") & as.Date(french.state.list[[i]]$date) <= as.Date("2018-12-31"))
    
    polygon(x=c(min(id2013), min(id2013), max(id2013), max(id2013)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2015), min(id2015), max(id2015), max(id2015)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2017), min(id2017), max(id2017), max(id2017)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    box()
    
    
    lines(french.state.list[[i]]$hits, lwd=1, col="#bdd7e7")
    axis(side=2, at=c(0,25,50,75,100), las=2)
    axis(side=1, at=c(median(id2013), median(id2015), median(id2017)), labels=c(2013, 2015, 2017), las=1)
    model <- bcp(y= french.state.list[[i]]$hits)
    lines(model$posterior.mean, lwd=.75, col="#08519c")
}





## ------------------------------------------------------------ ##
## russian
russian.world <- gtrends(TERMS[5])
russian.world <- subset(russian.world$interest_by_country, !is.na(hits))
russian.ISO <- countrycode(russian.world$location, origin="country.name", destination="iso2c")

russian.world$hits[russian.world$hits=="<1"] <- .5
russian.world$hits <- as.numeric(russian.world$hits)
russian.world <- subset(russian.world, !is.na(hits))


N <- nrow(russian.world)
russian.state.list <- lapply(1:N, function(i){
    
    temp <- gtrends(TERMS[5], geo=c(russian.ISO[i]))$interest_over_time
    temp$hits[temp$hits=="<1"] <- .5
    temp$hits <- as.numeric(temp$hits)
    
    temp$hits <- temp$hits * (russian.world$hits[i]/100)
    
    return(temp)
})



par(mfrow=c(4,3), mar=c(2,2.5,1,.5))
for(i in 1:length(russian.state.list)){
    plot(russian.state.list[[i]]$hits, main=russian.world$location[i], lwd=1, col=grey(.75), ylim=c(0, 100), xaxt="n", yaxt="n", type="n")
    
    id2013 <- which(as.Date(russian.state.list[[i]]$date) > as.Date("2012-12-31") & as.Date(russian.state.list[[i]]$date) <= as.Date("2013-12-31"))
    id2014 <- which(as.Date(russian.state.list[[i]]$date) > as.Date("2013-12-31") & as.Date(russian.state.list[[i]]$date) <= as.Date("2014-12-31"))
    id2015 <- which(as.Date(russian.state.list[[i]]$date) > as.Date("2014-12-31") & as.Date(russian.state.list[[i]]$date) <= as.Date("2015-12-31"))
    id2016 <- which(as.Date(russian.state.list[[i]]$date) > as.Date("2015-12-31") & as.Date(russian.state.list[[i]]$date) <= as.Date("2016-12-31"))
    id2017 <- which(as.Date(russian.state.list[[i]]$date) > as.Date("2016-12-31") & as.Date(russian.state.list[[i]]$date) <= as.Date("2017-12-31"))
    id2018 <- which(as.Date(russian.state.list[[i]]$date) > as.Date("2017-12-31") & as.Date(russian.state.list[[i]]$date) <= as.Date("2018-12-31"))
    
    polygon(x=c(min(id2013), min(id2013), max(id2013), max(id2013)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2015), min(id2015), max(id2015), max(id2015)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2017), min(id2017), max(id2017), max(id2017)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    box()
    
    
    lines(russian.state.list[[i]]$hits, lwd=1, col="#bdd7e7")
    axis(side=2, at=c(0,25,50,75,100), las=2)
    axis(side=1, at=c(median(id2013), median(id2015), median(id2017)), labels=c(2013, 2015, 2017), las=1)
    model <- bcp(y= russian.state.list[[i]]$hits)
    lines(model$posterior.mean, lwd=.75, col="#08519c")
}





## ------------------------------------------------------------ ##
## Indonesian
indonesian.world <- gtrends(TERMS[6])
indonesian.world <- subset(indonesian.world$interest_by_country, !is.na(hits))
indonesian.ISO <- countrycode(indonesian.world$location, origin="country.name", destination="iso2c")

indonesian.world$hits[indonesian.world$hits=="<1"] <- .5
indonesian.world$hits <- as.numeric(indonesian.world$hits)
indonesian.world <- subset(indonesian.world, !is.na(hits))


N <- nrow(indonesian.world)
indonesian.state.list <- lapply(1:N, function(i){
    
    temp <- gtrends(TERMS[6], geo=c(indonesian.ISO[i]))$interest_over_time
    temp$hits[temp$hits=="<1"] <- .5
    temp$hits <- as.numeric(temp$hits)
    
    temp$hits <- temp$hits * (indonesian.world$hits[i]/100)
    
    return(temp)
})






## ------------------------------------------------------------ ##
## Chinese
chinese.world <- gtrends(TERMS[7])
chinese.world <- subset(chinese.world$interest_by_country, !is.na(hits))
chinese.ISO <- countrycode(chinese.world$location, origin="country.name", destination="iso2c")

chinese.world$hits[chinese.world$hits=="<1"] <- .5
chinese.world$hits <- as.numeric(chinese.world$hits)
chinese.world <- subset(chinese.world, !is.na(hits))


N <- nrow(chinese.world)
chinese.state.list <- lapply(1:N, function(i){
    
    temp <- gtrends(TERMS[7], geo=c(chinese.ISO[i]))$interest_over_time
    temp$hits[temp$hits=="<1"] <- .5
    temp$hits <- as.numeric(temp$hits)
    
    temp$hits <- temp$hits * (chinese.world$hits[i]/100)
    
    return(temp)
})



par(mfrow=c(4,3), mar=c(2,2.5,1,.5))
for(i in 1:length(chinese.state.list)){
    plot(chinese.state.list[[i]]$hits, main=chinese.world$location[i], lwd=1, col=grey(.75), ylim=c(0, 100), xaxt="n", yaxt="n", type="n")
    
    id2013 <- which(as.Date(chinese.state.list[[i]]$date) > as.Date("2012-12-31") & as.Date(chinese.state.list[[i]]$date) <= as.Date("2013-12-31"))
    id2014 <- which(as.Date(chinese.state.list[[i]]$date) > as.Date("2013-12-31") & as.Date(chinese.state.list[[i]]$date) <= as.Date("2014-12-31"))
    id2015 <- which(as.Date(chinese.state.list[[i]]$date) > as.Date("2014-12-31") & as.Date(chinese.state.list[[i]]$date) <= as.Date("2015-12-31"))
    id2016 <- which(as.Date(chinese.state.list[[i]]$date) > as.Date("2015-12-31") & as.Date(chinese.state.list[[i]]$date) <= as.Date("2016-12-31"))
    id2017 <- which(as.Date(chinese.state.list[[i]]$date) > as.Date("2016-12-31") & as.Date(chinese.state.list[[i]]$date) <= as.Date("2017-12-31"))
    id2018 <- which(as.Date(chinese.state.list[[i]]$date) > as.Date("2017-12-31") & as.Date(chinese.state.list[[i]]$date) <= as.Date("2018-12-31"))
    
    polygon(x=c(min(id2013), min(id2013), max(id2013), max(id2013)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2015), min(id2015), max(id2015), max(id2015)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2017), min(id2017), max(id2017), max(id2017)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    box()
    
    
    lines(chinese.state.list[[i]]$hits, lwd=1, col="#bdd7e7")
    axis(side=2, at=c(0,25,50,75,100), las=2)
    axis(side=1, at=c(median(id2013), median(id2015), median(id2017)), labels=c(2013, 2015, 2017), las=1)
    model <- bcp(y= chinese.state.list[[i]]$hits)
    lines(model$posterior.mean, lwd=.75, col="#08519c")
}






## analyse temporal patterns
X <- 1:length(state.list[[3]]$hits)
coeff <- matrix(NA, ncol=4, nrow=72)

for(i in 1:72){
    coeff[i,] <- as.numeric(summary(lm(state.list[[i]]$hits ~ X))$coefficients[2,])
}



plot(decompose(ts(state.list[[16]]$hits, frequency=52), type="additive"))








