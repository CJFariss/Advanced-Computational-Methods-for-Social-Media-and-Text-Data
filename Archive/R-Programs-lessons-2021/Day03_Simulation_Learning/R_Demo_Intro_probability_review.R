## R_Demo_Intro_probability_review_01.R
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
## For this R tutorial, we will learn how:
## (1) learn to think about probability in terms of a simple model
## (2) begin estimating discrete and continuous probability distributions
##
##########################################################################

## Probability considers what data will be observed from a given random (stochastic) process.
## Statistics considers what random (stochastic) process gave rise to observed data at hand.

## A probability measure, a probability space, or a probability model requires three elements: (S, B, P(.)).

## S and B are sets and are related to one another.
## S is short for sample space.
## B (or Beta) is short for Borel field (but it's just another set related to the definition of S)
## P(.) is a rule about the probability value that each element in B is assigned. For practical purposes, we are mostly concered about P(.) but we should know about the role S and B play as well.

## For most applications, the set of events in B is too large to consider or even calculate because it is made up of all possible subsets of S. But we can know in principle what the characteristics of B are based on our knowledge of S. This will become more concrete below.

## Once we have developed a clear definition of S and B, we get to argue about the rules associated with P(.). This is where a lot of conceptual debates in the field of Statistics takes place and where a lot of effort in the social sciences takes place.

## We will start with some very simple examples. These examples will give us an intuition about the model of probability (S, B, P(.)).

## The bullet points for this lesson may seems complicated so far but you already have a well-developed understanding of the basics of probability in practice. This knowledge is based on many of the activities from class in which we have used our dice, cards, and the sample() function to make probablistic decisions in our R programs. The information here helps us formalize our understanding of the definition of probability that we have been using all along. You may not know it yet, but you already understand everything presented in this lesson so far.

## Note that in most texts, any set is formally declared using the squiggly braces {}.
## I will use the squiggly braces {} to denote a set in the lesson comments. In the R code itself, we will create an object using the c() or vector() that we define as our set.

## S: the sample space for a single coin flip is S = {Heads, Tails}
single_coin_flip_sample_space <- c("Heads", "Tails")
single_coin_flip_sample_space


## we could also define the sample space for a single coin flip as the follwing: the sample space for a coin flip is S = {Heads, not Heads}
single_coin_flip_sample_space <- c("Heads", "not Heads")
single_coin_flip_sample_space

## a subset of the sample space S, is an event E
## The sample space S, has two possible events E1 = {Heads}; E2 = {Tails}

single_coin_flip_event1 <- single_coin_flip_sample_space[1]
single_coin_flip_event2 <- single_coin_flip_sample_space[2]
single_coin_flip_event1
single_coin_flip_event2

single_coin_flip_event3 <- single_coin_flip_sample_space[1:2]
single_coin_flip_event4 <- single_coin_flip_sample_space[c(-1,-2)]
single_coin_flip_event3
single_coin_flip_event4


## Heads and not Heads are complements: the complement of Heads is not Heads; the complement of not Heads is Heads.
## Importantly the complement of S is the empty set {Ø}.

## The union of the sample space Heads U Tails occurs with probability 1 for the single coin flip.
## So the total probability of the sample space is P(S)=1.

## The sample space for a single coin flip is not equivalent to the sample space for B (the Borel field).
## The sample space contains 4 events. The reasons this is the case in this simple example is because the events for the single coin flip are disjoint or mutually exclusive.

## The set B (the Borel field) contains every possible combination of unions, intersections, and complements of the events in set S. For the sample space for a single coin flip, there are only two events, which are by definition of complements of one another. There are no unions or intersections (which will consider below).

## To see how quickly complicated a set S can get, let's consider a single roll of a 4-sided dice (this is equivalent to two-coin flips).

## S: the sample space for a single roll of a 4-sided die is S = {1, 2, 3, 4}
## note that I am defining this vector in R as a character vector because we are thinking about this collection of events as part of a set but we have not defined them in such as way that we can treat them algebraically
single_roll_4sided_die_sample_space <- c("1", "2", "3", "4")
single_roll_4sided_die_sample_space

## note that S contains 4 events
single_roll_4sided_die_sample_space[1]
single_roll_4sided_die_sample_space[2]
single_roll_4sided_die_sample_space[3]
single_roll_4sided_die_sample_space[4]


## The set B (the Borel field) for single roll of a 4-sided die is S is defined by 16 events: B = { ... }
single_roll_4sided_die_sample_space[c(-1, -2, -3, -4)] ## the empty set {Ø};
single_roll_4sided_die_sample_space[1] ## roll 1;
single_roll_4sided_die_sample_space[2] ## roll 2;
single_roll_4sided_die_sample_space[3] ## roll 3;
single_roll_4sided_die_sample_space[4] ## roll 4;
single_roll_4sided_die_sample_space[c(1, 2)] ## roll 1 or roll 2; the compliment is roll 3 or roll 4;
single_roll_4sided_die_sample_space[c(1, 3)] ## roll 1 or roll 3; the compliment is roll 2 or roll 4;
single_roll_4sided_die_sample_space[c(1, 4)] ## roll 1 or roll 4; the compliment is roll 2 or roll 3;
single_roll_4sided_die_sample_space[c(2, 3)] ## roll 2 or roll 3; the compliment is roll 1 or roll 4;
single_roll_4sided_die_sample_space[c(2, 4)] ## roll 2 or roll 4; the compliment is roll 1 or roll 3;
single_roll_4sided_die_sample_space[c(3, 4)] ## roll 3 or roll 4; the compliment is roll 1 or roll 2;
single_roll_4sided_die_sample_space[c(1, 2, 3)] ## roll 1 or roll 2 or roll 3;
single_roll_4sided_die_sample_space[c(1, 3, 4)] ## roll 1 or roll 3;
single_roll_4sided_die_sample_space[c(1, 2, 4)] ## roll 1 or roll 4;
single_roll_4sided_die_sample_space[c(2, 3, 4)] ## roll 1 or roll 4;
single_roll_4sided_die_sample_space[c(1, 2, 3, 4)] ## roll 1 or roll 2 or roll 3 or roll 4;


## here are the same as the above but as the compliments
single_roll_4sided_die_sample_space[c(1, 2, 3, 4)]
single_roll_4sided_die_sample_space[-1] ## roll not 1; the compliment is roll 2 or 3 or roll 4
single_roll_4sided_die_sample_space[-2] ## roll not 2; the compliment is roll 1 or 3 or 4
single_roll_4sided_die_sample_space[-3] ## roll not 3; the compliment is roll  2 or 3 or 4
single_roll_4sided_die_sample_space[-4] ## roll not 4; the compliment is roll 1 or 2 or 3
single_roll_4sided_die_sample_space[c(-1, -2)] ## roll not 1 or roll not 2; the compliment is roll 1 or roll 2
single_roll_4sided_die_sample_space[c(-1, -3)] ## roll 1 or roll 3; the compliment is roll 2 or roll 4
single_roll_4sided_die_sample_space[c(-1, -4)] ## roll 1 or roll 4; the compliment is roll 2 or roll 3
single_roll_4sided_die_sample_space[c(-2, -3)] ## roll 2 or roll 3; the compliment is roll 1 or roll 4
single_roll_4sided_die_sample_space[c(-2, -4)] ## roll 2 or roll 4; the compliment is roll 1 or roll 3
single_roll_4sided_die_sample_space[c(-3, -4)] ## roll 3 or roll 4; the compliment is roll 1 or roll 2
single_roll_4sided_die_sample_space[c(-1, -2, -3)] ## roll 1 or roll 2 or roll 3
single_roll_4sided_die_sample_space[c(-1, -3, -4)] ## roll 1 or roll 3;
single_roll_4sided_die_sample_space[c(-1, -2, -4)] ## roll 1 or roll 4;
single_roll_4sided_die_sample_space[c(-2, -3, -4)] ## roll 1 or roll 4;
single_roll_4sided_die_sample_space[c(-1, -2, -3, -4)] ## roll 1 or roll 2 or roll 3 or roll 4


## Each of the events E in B have a P(E) >= 0.
## For the four-sided die example, there are 16 conceptually meaningful events.
## P(S)=1 and the compliment of S itself which is the empty set {Ø}.
## The intersection of each event with every other event which is 8^2 all of which are also empty sets {Ø}.

## Because we defined each dice roll as mutually exclusive, the union of one event with any other event are also empty sets {Ø}.
## This should make sense, we can't both roll 1 and not roll 1 and a 4-sided die can't be simultaneously both 1 or 2 or another combination of values.

## But wait, there are even more potentail events because we could consider the intersections and unions of 2 to 8 of the events.
## All of these combinations are potentially meaningful, but because we defined the events for this sample space as mutually exclusive, they are all empty {Ø} but they still exist.

## Once we allow for unions and intersections in S, the set B increases to a size that is easy to fully characterize, but difficult to fully list for all events.
## We won't try to count all of these combinations in any more of the examples we work with, but you should familiarize yourself with the underlying logic of what the Borel field represents. It is every possible combination of events that could occur for a given set S.


##########################################################################
## We will conduct some in-class activities using legos to further develop intuitions about probability distributions
##
##
##########################################################################
## We will also look at some (but not all) of the distributions in R below
##
##
##########################################################################
## discrete random variable distributions
## Bernoulli
## Binomial
## Poisson
## geometric
## negative binomial
## hypergeometric
## multinomial

##########################################################################
## continuous random variable distributions
## uniform
## normal
## Student's t-distribution
## exponential
## chi-squared distribution
## f-distribution
## gamma distribution
## beta distributions

## see other R files

##########################################################################

