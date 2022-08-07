## Introduction to tutorial
############################

## For this R tutorial we will fit a binary IRT model to a Star Trek survey dataset.

par(mfrow=c(1,1), mar=c(4,0.5,2,18), font=2, font.lab=2, cex=1.3)

## load library
library(rstan) # load rstan library
library(MASS) # loaed library with truehist function

## set time start variable
time1 <- Sys.time()
print(Sys.time() - time1)


# -------------------------------------------------- #
# define STAN model
# -------------------------------------------------- #
model <- "
data {
    int<lower=0> n;
    int<lower=0> k;
    int<lower=0, upper=1> y1[n];
    int<lower=0, upper=1> y2[n];
    int<lower=0, upper=1> y3[n];
    int<lower=0, upper=1> y4[n];
    int<lower=0, upper=1> y5[n];
    int<lower=0, upper=1> y6[n];
    int<lower=0, upper=1> y7[n];
    int<lower=0, upper=1> y8[n];
    int<lower=0, upper=1> y9[n];
    int<lower=0, upper=1> y10[n];
    int<lower=0, upper=1> y11[n];
}
parameters {
    vector[k] alpha;
    real<lower=0> beta[k];
    vector[n] x;    // this is theta but its easier to see as x in the code
}
transformed parameters {
}
model {
    x ~ normal(0,1); //priors on latent variable
    
    alpha ~ normal(0,4); //priors (these are variances not precision)
    beta ~ normal(0,1);
    
    // items
    y1 ~ bernoulli_logit(alpha[1] + beta[1] * x);
    y2 ~ bernoulli_logit(alpha[2] + beta[2] * x);
    y3 ~ bernoulli_logit(alpha[3] + beta[3] * x);
    y4 ~ bernoulli_logit(alpha[4] + beta[4] * x);
    y5 ~ bernoulli_logit(alpha[5] + beta[5] * x);
    y6 ~ bernoulli_logit(alpha[6] + beta[6] * x);
    y7 ~ bernoulli_logit(alpha[7] + beta[7] * x);
    y8 ~ bernoulli_logit(alpha[8] + beta[8] * x);
    y9 ~ bernoulli_logit(alpha[9] + beta[9] * x);
    y10 ~ bernoulli_logit(alpha[10] + beta[10] * x);
    y11 ~ bernoulli_logit(alpha[11] + beta[11] * x);
    
}
"
# -------------------------------------------------- #


response <- read.csv("StarTrekQuiz20210730.csv")

## create binary responses
response$y2 <- as.numeric(as.character(response$y2)=="Picard")
#response$y3 <- as.numeric(as.character(response$y3)=="Reginald Barclay")
#response$y3 <- as.numeric(as.character(response$y3)=="Data")
#response$y3 <- as.numeric(as.character(response$y3)=="Reginald Barclay" | as.character(response$y3)=="Geordi Laforge")
response$y3 <- as.numeric(as.character(response$y3)=="Reginald Barclay" | as.character(response$y3)=="Data")
response$y4 <- as.numeric(as.character(response$y4)=="Cloaking Device")
response$y7 <- as.numeric(as.character(response$y7)=="I")
response$y8 <- as.numeric(as.character(response$y8)=="Gates Mcfadden")

## create additive scale of right and wrong answers
response$additive <- apply(response[,3:12],1,sum)

## calculate the number of observations and number of items
n <- nrow(response)
n
k <- 11


## create datalist
data <- list(n=n,
k=k,
y1=response$y1,
y2=response$y2,
y3=response$y3,
y4=response$y4,
y5=response$y5,
y6=response$y6,
y7=response$y7,
y8=response$y8,
y9=response$y9,
y10=response$y10,
y11=rbinom(n=n,size=1,prob=.5)
)


## fit stan model
fit <- stan(model_code = model, data = data, iter = 2000, chains = 4)

## extract draws from stan model object
output <- extract(fit, permuted = TRUE)

## print fit object
fit

## print the latent variable
cbind(apply(output$x,2,mean), apply(output$x,2,sd))




## MAKE SWEET PLOTS!
##  pdf("/Users/cjfariss/Desktop/Crash_Course_Measurement_STAN/StarTrekPlots/StarTrekQuick_Posterior_AdditiveScale.pdf ", height=6, width=9)
par(mar=c(4,4,1,1), font=2, font.lab=2, cex=1.3)
plot(apply(output$x,2,mean), response$additive, ylim=c(0,10), xlim=c(-3.25,3.25), ylab="additive scale", xlab="posterior estimates")
abline(reg=lm(response$additive ~ apply(output$x,2,mean)), col=2, lwd=2)

lb <- apply(output$x,2,quantile,.05)
ub <- apply(output$x,2,quantile,.95)

for(i in 1:n){
    lines(c(lb[i], ub[i]), c(response$additive[i], response$additive[i]), col="#1c9099", lwd=2)
}

points(apply(output$x,2,mean), response$additive, col="#1c9099", bg="#ece2f0", cex=1.2, pch=21)
##  dev.off()

## make an index, print it, re-order it, print it again
INDEX <- 1:n
INDEX
INDEX <- INDEX[order(apply(output$x,2,mean))]
INDEX

## take the means of the posterior distributions, based on the rank order of the means
POSTERIORS <- apply(output$x,2,mean)[INDEX]


## make a plot
par(mar=c(4,1,2,10), font=2, font.lab=2, cex=1.3)
##  pdf("/Users/cjfariss/Desktop/Crash_Course_Measurement_STAN/StarTrekPlots/StarTrekQuick_LatentVariableParameters.pdf ", height=6, width=9)
plot(POSTERIORS, 1:n, xlim=c(-3.5,3.5), ylab="respondents", xlab="Latent Variable Estimates (posterior distribution of theta)", yaxt="n", main="Star Trek Knowledge", type="n")

abline(v=3, col=grey(.85), lwd=2.0)
abline(v=2, col=grey(.85), lwd=2.0)
abline(v=1, col=grey(.85), lwd=2.0)
abline(v=0, col=grey(.85), lwd=2.0)
abline(v=-1, col=grey(.85), lwd=2.0)
abline(v=-2, col=grey(.85), lwd=2.0)
abline(v=-3, col=grey(.85), lwd=2.0)

# upper and lower bounds of the posterior distributions
lb <- apply(output$x,2,quantile,.05)[INDEX]
ub <- apply(output$x,2,quantile,.95)[INDEX]

lb25 <- apply(output$x,2,quantile,.25)[INDEX]
ub75 <- apply(output$x,2,quantile,.75)[INDEX]

# loop through each estimate and plot a line for the 95% credible interval
for(i in 1:n){
    lines(c(lb25[i], ub75[i]), c(i,i), col="#1c9099", lwd=1.75)
    lines(c(lb[i], ub[i]), c(i,i), col="#1c9099", lwd=1.0)
}

# place the points on top of the credible intervals
points(POSTERIORS, 1:n, col="#1c9099", bg="#ece2f0", cex=.5, pch=21)

# add an axis to the right side of the panel with the names of the subjects, based on rank order
axis(side=4, at=1:n, labels=rep("",n) , las=2, cex=.5)
label_coordinates <- which((as.character(response$name)[INDEX]) != "")
label_coordinates
axis(side=4, at=label_coordinates, labels=as.character(response$name)[INDEX][label_coordinates], las=2, cex=.75)

##  dev.off()

## correlation with posterior point estimates and the additive scale
cor.test(response$additive, apply(output$x,2,mean))


## correlation with posterior draws and the additive scale
TEST <- NA
for(i in 1:nrow(output$x)){
    TEST[i] <- cor(response$additive, output$x[i,])
}
summary(TEST)


## make an index, print it, re-order it, print it again
INDEX <- 1:k
INDEX
INDEX  <- INDEX[order(apply(output$alpha,2,mean))]
INDEX

## make the labels for the questions
ALPHA.LABELS <- c("Darmok and Jalad, at Tanagra", "Picard, USS Enterprise 1701-D", "Memories Downloaded", "Pegasus: Cloaking Device", "James Tiberius Kirk", "The Cold War Analogy (VI)", "The Motion Picture (I): V'Ger", "Dr Crusher: Gates Mcfadden", "George and Gracie, the Humpbacks (IV)", "Warp Speed", "RANDOM")

par(mar=c(4,0.5,2,18), font=2, font.lab=2, cex=1.3)
##  pdf("/Users/cjfariss/Desktop/Crash_Course_Measurement_STAN/StarTrekPlots/StarTrekQuick_DifficultyParameters.pdf ", height=6, width=9)

ALPHA <- apply(output$alpha,2,mean)[INDEX]
LB.ALPHA <- apply(output$alpha,2,quantile,.05)[INDEX]
UB.ALPHA <- apply(output$alpha,2,quantile,.95)[INDEX]

barplot(ALPHA, horiz=T, space=0, xlim=c(-6.25,6.25), main="Star Trek Knowledge Questions", xlab="Difficulty Parameters")

for(i in order(ALPHA)){
    lines(c(LB.ALPHA[i], UB.ALPHA[i]), c(i-.5,i-.5), col=grey(.15), lwd=2)
}

axis(side=4, at=seq(.5, 10.5, 1), labels=ALPHA.LABELS[INDEX], las=2)
##  dev.off()


## make an index, print it, re-order it, print it again
INDEX <- 1:k
INDEX
INDEX  <- INDEX[order(apply(output$beta,2,mean))]
INDEX

par(mar=c(4,0.5,2,18), font=2, font.lab=2, cex=1.3)
##  pdf("/Users/cjfariss/Desktop/Crash_Course_Measurement_STAN/StarTrekPlots/StarTrekQuick_DiscriminationParameters.pdf ", height=6, width=9)

BETA <- apply(output$beta,2,mean)[INDEX]
LB.BETA <- apply(output$beta,2,quantile,.05)[INDEX]
UB.BETA <- apply(output$beta,2,quantile,.95)[INDEX]

barplot(BETA, horiz=T, space=0, xlim=c(0,4.25), main="Star Trek Knowledge Questions", xlab="Discrimination Parameters")

for(i in order(BETA)){
    lines(c(LB.BETA[i], UB.BETA[i]), c(i-.5,i-.5), col=grey(.15), lwd=2)
}

axis(side=4, at=seq(.5, 10.5, 1), labels=ALPHA.LABELS[INDEX], las=2)

##  dev.off()


## plot the difference between the least discriminator question and the other 9 questions
BETA.DIFF <- output$beta - output$beta[,2]


## make an index, print it, re-order it, print it again
INDEX <- 1:k
INDEX
INDEX  <- INDEX[order(apply(BETA.DIFF,2,mean))]
INDEX

par(mar=c(4,2,2,18), font=2, font.lab=2, cex=1.3)
##  pdf("/Users/cjfariss/Desktop/Crash_Course_Measurement_STAN/StarTrekPlots/StarTrekQuick_DiscriminationParametersDifference.pdf ", height=6, width=9)

BETA <- apply(BETA.DIFF,2,mean)[INDEX]
LB.BETA <- apply(BETA.DIFF,2,quantile,.05)[INDEX]
UB.BETA <- apply(BETA.DIFF,2,quantile,.95)[INDEX]

barplot(BETA, horiz=T, space=0, xlim=c(-2.25,4.25), main="Star Trek Knowledge Question Comparisons", xlab="Difference in Discrimination Parameter")

for(i in order(BETA)){
    lines(c(LB.BETA[i], UB.BETA[i]), c(i-.5,i-.5), col=grey(.15), lwd=2)
}

axis(side=4, at=seq(.5, 9.5, 1), labels=ALPHA.LABELS[INDEX], las=2)

TEST <- NA
for(i in 1:k){
    TEST[i] <- mean(BETA.DIFF[,i]>0)
}

axis(side=2, at=seq(.5, 10.5, 1), labels=round(TEST, digits=3)[INDEX], las=2, tick=F, line=-3)
mtext("Probability of Difference in Discrimination",side=2, line=1, cex=1.5)
##  dev.off()

# make an index, print it, re-order it, print it again
INDEX <- 1:k
INDEX
INDEX  <- rev(INDEX[order(apply(output$beta,2,mean))])
INDEX

## make the labels for the questions
ALPHA.LABELS <- c("Darmok and Jalad,\nat Tanagra", "Picard\nEnterprise 1701-D", "Memories Downloaded", "Pegasus\nCloaking Device", "James\nTiberius Kirk", "The Cold War\n Analogy (VI)", "The Motion Picture (I)\nV'Ger", "Dr Crusher\nGates Mcfadden", "George and Gracie\nthe Humpbacks (IV)", "Warp Speed\n","RANDOM")


par(mfrow=c(3,4), mar=c(2,0,0,0))
##  pdf("/Users/cjfariss/Desktop/Crash_Course_Measurement_STAN/StarTrekPlots/Startrek_IRT_Curves.pdf ", height=6, width=9)

plot(0,0, xlim=c(0,1), ylim=c(0,1), type="n", yaxt="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
mtext(expression(Pr(correct[j])), cex=1.5, line=-6)


for(j in INDEX){
    x <- seq(from=-3.0, to=3.5, by=.01)
    
    values <- 1 / (1+exp(-(output$alpha[,j] + output$beta[,j] %*% t(x))))
    
    plot(x, values[1,], ylim=c(0,1.275), type="n", lwd=2.0, col="orange", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-4.00,3.25))
    mtext(ALPHA.LABELS[j], side=3, line=-3, cex=1, at=-1.1)
    axis(side=1, at=c(-3,-2,-1,0,1,2,3))
    axis(side=2, at=c(0.01,.2,.4,.6,.8,1,1.2), labels=c("0.0","0.2","0.4","0.6","0.8","1.0","Pr()"), tick=F, pos=-2.9, las=2, cex.axis=1.2)
    #axis(side=2, at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1,1.2), labels=c("0.0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0"," Pr()"), tick=F, pos=-3.25, las=2, cex.axis=.8)
    lines(x, apply(values,2,quantile, .05), ylim=c(0,1), type="l", lwd=2.0, col="darkorange")
    lines(x, apply(values,2,quantile, .95), ylim=c(0,1), type="l", lwd=2.0, col="darkorange")
    lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=2.0, col="darkred")
    
}

##  dev.off()

par(mfrow=c(1,1), mar=c(4,0.5,2,18), font=2, font.lab=2, cex=1.3)

par(mfrow=c(1,1), mar=c(4,4,4,4), font=2, font.lab=2, cex=1.3)


