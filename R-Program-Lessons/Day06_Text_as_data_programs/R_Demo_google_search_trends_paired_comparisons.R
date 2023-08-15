## google_search_trends_paired_comparisons.R
##########################################################################
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
## ## Use the google trends API to access google trends data and genearte a graph comparing search terms for a specific geographic and temporal context.
##
## Examples taken from:
## Dancy, Geoff and Christopher J. Fariss. “The Global Resonance of Human Rights: What Google Trends Can Tell Us.” American Political Science Review (Forthcoming).
##
##########################################################################

## load libraries
library(gtrendsR)
library(countrycode)
library(stm)
library(tm)
library(MASS)
#library(colorbrewer)


## load necessary libraries 
## change groundhog to TRUE to install original versions of libraries from April-2022
source("R-Program-Lessons/Day06_Text_as_data_programs/groundhog_library_func.R")
groundhog_library_func(groundhog=FALSE, regular_install=FALSE)

#pdf("Rplots/Google_search_term_pairs.pdf", height=6, width=6)
#pdf("Rplots/Google_search_term_pairs_shortlist.pdf", height=6, width=6)
pdf("Rplots/Google_search_term_pairs_longlist.pdf", height=6, width=6)

#time
#war
#god
#race
#injustice
#human rights
#malaria
#torture
#terrorism
#national security
#social justice
#right to food
#amnesty International
#worker's rights

TERMS_short <- list(c("time", "war"),
              c("war", "god"),
              c("god", "race"),
              c("race", "injustice"),
              c("injustice", "human rights"),
              c("human rights", "malaria"),
              c("human rights", "torture"),
              c("human rights", "terrorism"),
              c("human rights", "national security"),
              c("human rights", "social justice"),
              c("human rights", "right to food"),
              #c("human rights", "Amnesty International"),
              c("human rights", "worker's rights")
)
length(TERMS_short)

TERMS_long <- list(c("facebook", "google"),
            c("google", "time"),
            c("time", "sports"),
            c("sports", "war"),
            c("war", "god"),
            c("god", "bible"),
            c("bible", "race"),
            c("race", "injustice"),
            c("injustice", "human rights"),
            c("human rights", "terrorism"),
            c("human rights", "malaria"),
            c("human rights", "nationalism"),
            c("human rights", "national security"),
            c("human rights", "sovereignty"),
            c("human rights", "will of the people"),
            c("human rights", "social justice"),
            c("human rights", "rule of law"),
            c("human rights", "populism"),
            #c(URLdecode("%2Fm%2F03ll3"), "human rights"), ## can compare topics to topics and terms to terms but this generates an 200 and 400 error
            c("human rights", "climate change"),
            c("human rights", "right to health"),
            c("human rights", "right to food"),
            c("human rights", "torture"),
            c("human rights", "Amnesty International")
            
)
length(TERMS_long)

TERMS <- TERMS_short
TERMS <- TERMS_long

#TIME <- "2013-01-01 2017-12-31"
TIME <- "2015-01-01 2019-12-31"

#par(mfrow=c(3,3), mar=c(2,2.5,1,.5))
par(mfrow=c(1,1), mar=c(2,2.5,1,.5))

for(i in 1:length(TERMS)){

world <- gtrends(TERMS[[i]], time=TIME, low_search_volume=T)$interest_over_time

    world$hits[world$hits=="<1"] <- .5
    world$hits <- as.numeric(world$hits)
    
    plot(world$hits[world$keyword==TERMS[[i]][1]], main=paste("Global:", TERMS[[i]][1], "vs.", TERMS[[i]][2]), lwd=1, col=grey(.75), ylim=c(0, 100), xaxt="n", yaxt="n", type="n")
    #lines(world$hits[world$keyword==TERMS[[i]][2]], type="l", ylim=c(0,100), col=2)
    #plot.window(xlim=c(1,length(world$hits[world$keyword==TERMS[[i]][1]])), ylim=c(0, 100))
    #mtext(side=3, text=paste("Global:", TERMS[[i]][1], "vs.", TERMS[[i]][2]), line=1)
    
    id2013 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2012-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2013-12-31"))
    id2014 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2013-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2014-12-31"))
    id2015 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2014-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2015-12-31"))
    id2016 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2015-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2016-12-31"))
    id2017 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2016-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2017-12-31"))
    id2018 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2017-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2018-12-31"))
    id2019 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2018-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2019-12-31"))
    id2020 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2019-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2020-12-31"))
    id2021 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2020-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2021-12-31"))
    
    polygon(x=c(min(id2013), min(id2013), max(id2013), max(id2013)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2015), min(id2015), max(id2015), max(id2015)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2017), min(id2017), max(id2017), max(id2017)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2019), min(id2019), max(id2019), max(id2019)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    polygon(x=c(min(id2021), min(id2021), max(id2021), max(id2021)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
    box()
    
    lines(world$hits[world$keyword==TERMS[[i]][1]], lwd=1, col="#c2a5cf")
    lines(world$hits[world$keyword==TERMS[[i]][2]], lwd=1, col="#a6dba0")
    axis(side=2, at=c(0,25,50,75,100), las=2)
    axis(side=1, at=c(median(id2013),  median(id2014), median(id2015), median(id2016), median(id2017), median(id2018), median(id2019), median(id2020), median(id2021)), labels=c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021), las=1)
    model <- bcp(y=world$hits[world$keyword==TERMS[[i]][1]])
    lines(model$posterior.mean, lwd=.75, col="#7b3294")
    model <- bcp(y=world$hits[world$keyword==TERMS[[i]][2]])
    lines(model$posterior.mean, lwd=.75, col="#008837")
    
    legend("topleft", legend=c(TERMS[[i]][1]), text.col="#7b3294" ,bty="n")
    legend("bottomleft", legend=c(TERMS[[i]][2]), text.col="#008837", bty="n")

}

dev.off()


