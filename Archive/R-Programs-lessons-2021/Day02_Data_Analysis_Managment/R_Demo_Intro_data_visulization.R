## R_Demo_Introduction_data_visulization.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2021
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
##
## R makes some very nice graphs
## let's make 2 simple ones
##
## Simple R Plots
## http://www.phaget4.org/R/plot.html
##
##########################################################################


## Introduction to tutorial:
##
## For this R tutorial, we will learn how:
##
## (1) make a plot() and barplot()
##
## NOTE that you can google: "Quick R graphical parameters" for a good resource for making graphs
## the URL is here: http://www.statmethods.net/advgraphs/parameters.html
##
## R color names are available here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
##
## https://wtamu.edu/~cbaird/sq/2015/01/22/why-are-red-yellow-and-blue-the-primary-colors-in-painting-but-computer-screens-use-red-green-and-blue/
##
## (2) Consider different color systems.
##
##########################################################################

## read in data using read.csv() function
##data <- read.csv("http://cfariss.com/code/ny_stop_frisk.csv")
data <- read.csv("ny_stop_frisk.csv")

data$total

## see color names here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
COLOR <- "lightblue"


## make a plot using the barplot() function
par(mfrow=c(1,1), mar=c(5,4,4,4))
barplot(data$total)

barplot(data$total,
beside=T,
space=0,
col=COLOR, # I used the named variable defined above instead of naming here
font=2,
font.lab=2,
ylab="Total Stop and Frisks",
main="Stop and Frisks by Race in 2012",
xpd=F,
horiz=F,
names.arg=data$race,
ylim=c(0, 1.1*max(data$total)) # I made the top of the plot a little bit larger than the largest value
)

## additional plot elements
box() # add box around plot region


## read in data using read.csv() function
##data <- read.csv("http://cfariss.com/code/ny_stop_frisk_black.csv")
data <- read.csv("ny_stop_frisk_black.csv")

## make a plot using the plot() function
par(mfrow=c(1,1))
plot(data$total,
type="o",
col="navy",
bg="lightblue",
pch=21,
ylim=c(0, 1.1*max(data$total)), # I made the top of the plot a little bit larger than the largest value
xlab="Year",
ylab="Total Stop and Frisks",
main="Stop and Frisks for African Americans Over Time",
xaxt="n"
)

# additional plot elements
axis(side=1, at=c(1,2,3,4,5,6,7,8,9,10), label=data$year)



## add multiple plots to one image using the par() function and the mfrow argument
par(mfrow=c(1,2))

data <- read.csv("ny_stop_frisk.csv")
COLOR <- "lightblue"

barplot(data$total, beside=T, space=1, col=COLOR, font=2, font.lab=2, ylab="Total Stop and Frisks", main="Stop and Frisks by Race in 2012", xpd=F, horiz=F, names.arg=data$race, ylim=c(0, 1.1*max(data$total)))
box()

data <- read.csv("ny_stop_frisk_black.csv")

## make a plot using the plot() function
plot(data$total, type="o", col="navy", bg="lightblue", pch=21, ylim=c(0, 1.1*max(data$total)), xlab="Year", ylab="Total Stop and Frisks", main="Stop and Frisks for African Americans Over Time", xaxt="n")
axis(side=1, at=c(1,2,3,4,5,6,7,8,9,10), label=data$year)

## go back to just one plot per image
par(mfrow=c(1,1))


## using color
Louise <- c(.25,0.75,2.5,1.0,.05,2.0)
Louise_color <- c("black", "burlywood1", "darkolivegreen3", "burlywood1", "black", "pink")
barplot(Louise)
barplot(matrix(Louise))
barplot(matrix(Louise), col=c(Louise_color), xlim=c(0,5), width=1)

Gene <- c(.15,.15,0.85,1.0,2.0,1.25,1.0)
Gene_color <- c("darkred","white", "burlywood1", "lightblue3", "yellow1", "burlywood1", "black")
barplot(matrix(Gene), col=c(Gene_color), add=T, space=1.5, width=1.25)

Tina <- c(.15,.15,0.85,.75,1.25,1.5,1.0,1.0)
Tina_color <- c("white","black", "white", "burlywood1", "lightblue4", "lightblue2", "burlywood1", "black")
barplot(matrix(Tina), col=c(Tina_color), add=T, space=3.75, width=1)


## triadic colors (programed with RBG colors)
par(mfrow=c(1,2), mar=c(4,4,2,2))
barplot(c(1,1,1), col=c("red", "green", "blue"), yaxt="n", space=0, main="Primary Colors (Additive)", names.arg=c("red", "green", "blue"))
barplot(c(1,1,1), col=c("cyan", "magenta", "yellow"), yaxt="n", space=0, main="Primary Colors (Subtractive)", names.arg=c("cyan", "magenta", "yellow"))
par(mfrow=c(1,1))

## triadic colors (programed with RBG colors) plot with the color number
par(mfrow=c(1,2), mar=c(4,4,2,2))
barplot(c(1,1,1), col=c("red", "green", "blue"), yaxt="n", space=0, main="Primary Colors (Additive)", names.arg=c("2", "3", "4"))
barplot(c(1,1,1), col=c("cyan", "magenta", "yellow"), yaxt="n", space=0, main="Primary Colors (Subtractive)", names.arg=c("5", "6", "7"))
par(mfrow=c(1,1))

## complementary colors (programed with RBG colors)
par(mfrow=c(2,3), mar=c(4,4,2,2))
barplot(c(1,1), col=c("cyan", "red"), yaxt="n", space=0, names.arg=c("cyan", "red"))
barplot(c(1,1), col=c("magenta", "green"), yaxt="n", space=0, names.arg=c("magenta", "green"))
barplot(c(1,1), col=c("yellow", "blue"), yaxt="n", space=0, names.arg=c("yellow", "blue"))
barplot(c(1,1), col=c(5, 2), yaxt="n", space=0, names.arg=c("5", "2"))
barplot(c(1,1), col=c(6, 3), yaxt="n", space=0, names.arg=c("6", "3"))
barplot(c(1,1), col=c(7, 4), yaxt="n", space=0, names.arg=c("7", "4"))
par(mfrow=c(1,1))


## color wheel(programed with RBG colors)
library(grDevices)
par(mfrow=c(2,2), mar=c(1,1,1,1))
pie(rep(1,6), col=rainbow(6), border=NA, labels=c("red", "yellow", "green", "cyan", "blue", "magenta"))
pie(rep(1,12), col=rainbow(12), border=NA)
pie(rep(1,24), col=rainbow(24), labels=NA, border=NA)
pie(rep(1,256), col=rainbow(256), labels=NA, border=NA)
par(mfrow=c(1,1))

## split complementary colors (user selected from Rcolor.pdf)
par(mfrow=c(3,3))
barplot(c(1,2,3), col=c("darkslateblue", "darkolivegreen1", "orange"), yaxt="n")
barplot(c(1,2,3), col=c("darkslateblue", "green", "orange"), yaxt="n")
barplot(c(1,2,3), col=c("darkorchid4", "green", "orange"), yaxt="n")
barplot(c(1,2,3), col=c("darkorchid", "green", "darkorange2"), yaxt="n")
barplot(c(1,2,3), col=c("orange", "lightblue", "darkred"), yaxt="n")
barplot(c(1,2,3), col=c("darkorange", "navy", "yellow"), yaxt="n")
barplot(c(1,2,3), col=c("red", "navy ", "darkolivegreen1"), yaxt="n")
barplot(c(1,2,3), col=c("red","navy ", "seagreen1 "), yaxt="n")
barplot(c(1,2,3), col=c("yellow", "darkorchid4", "turquoise3"), yaxt="n")
par(mfrow=c(1,1))

## Notes on the efficacy of color systems:
## Any color system is measured by the number of different colors that created by combinations of the primary colors of that color system.
## The set of colors is called the "color gamut" of the system.
## We could estimate the probability of any combination of the set of colors in the color gamut using the probability and set theory we developed in week 9 and week 10 of the course.
## Some color systems have more total possible colors than others.
## A color system with a large gamut (more possible combinations from the set) can more effectively represent of different colors, which are all combinations of the primary colors.




## colors in hexidemical codes (taken from colorbrewer2.org)
COLORS <- c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494")

## a simple barplot using the colors from above
barplot(c(1,2,3,4,5), col=COLORS)


## example using up-to-date data on COVID cases and recoveries
## download data from github
recovered_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
confirmed_case_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

## set high point for the graphs (use log based 2 value; see more below)
high_point <- round(max(log2(c(apply(confirmed_case_data[,-c(1:4)],2,sum)))))
high_point

## reset the parameters for the plot space
par(mfrow=c(1,3), mar=c(4,2,1,5))

## plot the log base 2 data. This makes the y-axis units a doubling from 1 to 2 to 4 to 8 to 16 ...
plot(log2(apply(confirmed_case_data[,-c(1:4)],2,sum)), ylim=c(0, high_point), ylab="", xlab="Days since 1/22/2020", yaxt="n", main="Confirmed Cases", pch=21, col="#d95f02", bg="#fc8d62")
for(i in 1:high_point){ abline(h=i, lwd=.5, lty=2) }
axis(side=2, at=1:high_point, las=2)
axis(side=4, at=c(1:high_point), labels=rep("",high_point), las=2)
mtext(side=4, "log-base-2 (1-unit change on the y-axis is a doubling of cases)",line=2)

plot(log2(apply(recovered_data[,-c(1:4)],2,sum)), ylim=c(0,high_point), ylab="", xlab="Days since 1/22/2020", yaxt="n", main="Recovered Cases", pch=21, col="#1b9e77", bg="#66c2a5")
for(i in 1:high_point){ abline(h=i, lwd=.5, lty=2) }
axis(side=2, at=1:high_point, las=2)
axis(side=4, at=c(1:high_point), labels=rep("",high_point), las=2)

plot(log2(apply(confirmed_case_data[,-c(1:4)],2,sum) - apply(recovered_data[,-c(1:4)],2,sum)), ylim=c(0,high_point), ylab="", xlab="Days since 1/22/2020", yaxt="n", main="Confirmed but not yet Recovered Cases", pch=21, col="#7570b3", bg="#8da0cb")
for(i in 1:high_point){ abline(h=i, lwd=.5, lty=2) }
axis(side=4, at=c(1:high_point), labels=2^c(1:high_point), las=2)
#axis(side=4, at=c(5,10,15,20), labels=2^c(5,10,15,20), las=2)
axis(side=2, at=1:high_point, las=2)

## let's combine the three time series plots into one plot with all of the color coded points

## reset the parameters for the plot space
par(mfrow=c(1,1), mar=c(4,4,1,5))

## make plot
plot(log2(apply(confirmed_case_data[,-c(1:4)],2,sum)), ylim=c(0, high_point), ylab="", xlab="Days since 1/22/2020", yaxt="n", main="COVID-19 Cases", pch=21, col="#d95f02", bg="#fc8d62")

## add more points
points(log2(apply(recovered_data[,-c(1:4)],2,sum)), ylim=c(0,high_point), pch=21, col="#1b9e77", bg="#66c2a5")
points(log2(apply(confirmed_case_data[,-c(1:4)],2,sum) - apply(recovered_data[,-c(1:4)],2,sum)), pch=21, col="#7570b3", bg="#8da0cb")
for(i in 1:high_point){ abline(h=i, lwd=.5, lty=2) }
axis(side=4, at=c(1:high_point), labels=2^c(1:high_point), las=2)
axis(side=2, at=1:high_point, las=2)
mtext(side=2, "log-base-2 (1-unit change on the y-axis is a doubling of cases)",line=2)

legend("bottomleft", legend=c("Confirmed Cases", "Recovered Cases", "Confirmed but not yet Recovered Cases"), fill=c("#fc8d62", "#66c2a5", "#8da0cb"), bty="n", bg="white")



## reset the parameters for the plot space
par(mfrow=c(1,1), mar=c(4,4,1,5))

## make plot with the count (not logged based).
plot(apply(confirmed_case_data[,-c(1:4)],2,sum),
ylab="",
xlab="Days since 1/22/2020",
yaxt="n",
main="COVID-19 Cases",
pch=21,
col="#d95f02",
bg="#fc8d62")

## add more points
points(apply(recovered_data[,-c(1:4)],2,sum), ylim=c(0,high_point), pch=21, col="#1b9e77", bg="#66c2a5")
points(apply(confirmed_case_data[,-c(1:4)],2,sum) - apply(recovered_data[,-c(1:4)],2,sum), pch=21, col="#7570b3", bg="#8da0cb")
for(i in 2^(1:high_point)){ abline(h=i, lwd=.5, lty=2) }
axis(side=4, at=2^(1:high_point), labels=2^c(1:high_point), las=2)
#axis(side=2, at=2^(1:high_point), las=2)
#axis(side=2, at=1:high_point, las=2)
mtext(side=2, "log-base-2 (1-unit change on the y-axis is a doubling of cases)",line=2)

legend("topleft", legend=c("Confirmed Cases", "Recovered Cases", "Confirmed but not yet Recovered Cases"), fill=c("#fc8d62", "#66c2a5", "#8da0cb"), bty="n", bg="white")






## make a barplot version with the count data
par(mfrow=c(1,3), mar=c(6,1,1,5))

## print the day before the starting date to screen as a "Date"
as.Date("01/21/2020", "%m/%d/%y")

## add one to the day before the start date
as.Date("01/21/2020", "%m/%d/%y") + 1

## make a sequence of dates
date_range <- as.Date("01/21/2020", "%m/%d/%y") + 1:ncol(confirmed_case_data[,-c(1:4)])
date_range

barplot(apply(confirmed_case_data[,-c(1:4)],2,sum),
ylab="",
xlab="",
yaxt="n",
main="Confirmed Cases",
space=0,
border="#d95f02",
col="#fc8d62",
ylim=c(0,2^high_point),
names.arg=date_range,
las=2)
axis(side=4, at=2^(1:high_point), labels=2^c(1:high_point), las=2)
box()

barplot(apply(recovered_data[,-c(1:4)],2,sum),
yaxt="n",
main="Recovered Cases",
space=0, border="#1b9e77",
col="#66c2a5",
ylim=c(0,2^high_point),
names.arg=date_range,
las=2)
axis(side=4, at=2^(1:high_point), labels=2^c(1:high_point), las=2)
box()

barplot(apply(confirmed_case_data[,-c(1:4)],2,sum) - apply(recovered_data[,-c(1:4)],2,sum),
yaxt="n",
main="Confirmed  but not yet Recovered",
space=0,
border="#7570b3",
col="#8da0cb",
ylim=c(0,2^high_point),
names.arg=date_range,
las=2)
axis(side=4, at=2^(1:high_point), labels=2^c(1:high_point), las=2)
box()

#legend("topleft", legend=c("Confirmed Cases", "Recovered Cases", "Confirmed but not yet Recovered Cases"), fill=c("#fc8d62", "#66c2a5", "#8da0cb"), bty="n", bg="white")


## replicate an ourworldindata graph: https://ourworldindata.org/covid-vaccinations

## obtain the csv data
#vaccine_data <- read.csv("https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations/locations.csv")
vaccine_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
names(vaccine_data)

## print the system date
Sys.Date()

## use the system date to calcuate the dates that are most recent
vaccine_data$most_recent_date <- Sys.Date() - as.Date(vaccine_data$date)
dim(vaccine_data)
table(vaccine_data$most_recent_date)


## subset only the country's with the most recent dates
vaccine_data_last <- subset(vaccine_data, most_recent_date==1 & !is.na(people_fully_vaccinated_per_hundred))
dim(vaccine_data_last)


## pull out the relevant column-vector of data for plotting

##vaccine_data_last$people_fully_vaccinated_per_hundred[is.na(vaccine_data_last$people_fully_vaccinated_per_hundred)] <- 0

values <- vaccine_data_last$people_fully_vaccinated_per_hundred
location_names <- vaccine_data_last$location

## https://colorbrewer2.org/#type=qualitative&scheme=Paired&n=5
COLORS_QUALITATIVE<- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99")

## https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=5
COLORS_SEQUENTIAL <- c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494")

## https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=9
COLORS_SEQUENTIAL_9 <- c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")

## same versions as above with the lightest color replicated for all the rest after the first 9
COLORS_SEQUENTIAL_9_rep <- c(rep("#ffffd9", length(values)-9), COLORS_SEQUENTIAL_9)

## same versions as above with the lightest color replicated for all the rest after the first 9
COLORS_SEQUENTIAL_9_rep_each <- c(rep(COLORS_SEQUENTIAL_9, each=length(values)/9))
COLORS_SEQUENTIAL_9_rep_each

## set some graphical parameters
par(mfrow=c(1,1), mar=c(4,10,3,3))

## make a sweet barplot
barplot(values[order(values, decreasing=FALSE)],
xlim=c(0,100),
xlab="People fully vaccinated per hundred",
space=0,
horiz=TRUE,
main="Share of the population\nfully vaccinated against COVID-19",
names.arg=location_names[order(values, decreasing=FALSE)],
las=2,
xaxt="n",
col=COLORS_SEQUENTIAL_9_rep,
cex.names=0.65)

axis(side=1, at=c(0,10,20,30,40,50,60,70,80,90,100))

axis(side=4, at=1:length(values), labels=values[order(values, decreasing=FALSE)], las=2, cex.axis=0.65)

abline(v=70, lty=2, col=grey(.5), lwd=2)

box()

