##################################################################
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~/// Stat 102C Lab 2 R Code \\\~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~ Levon Demirdjian ~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~ April 06 2016 ~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##################################################################


################
## Problem 1: ##
################

## Let the pdf f(x) be given by f(x) = kx^4 where 0 <= x <= 1, where 
## k is the normalizing constant.

## a) Find the normalizing constant of f(x).

## b) Find the cdf F(t). Plot F(t) as a function of t.

## c) Use the inversion method to draw samples from f(x). Start off 
## by computing the inverse of F(t); plot F(t) using "curve".

## d) Use the curve function in R to plot f(x). Show the plot of 
## f(x) on the same plot as the histogram of points sampled from part c. 
## Hint: make sure the "prob" parameter is set to true in "hist", and 
## use the "add" parameter in the "curve" function to add the curve
## to the existing plot. Use ?? if you're not sure what you're doing.



###############
## Solutions ##
###############

my_pdf <- function(x){5*x^4}
my_cdf <- function(x){x^5}
my_cdf_inverse <- function(x){x^(1/5)}

par(mfrow=c(1,2))
curve(my_pdf, 0, 1, main = "PDF of X", ylab = "Cumulative probability")
curve(my_cdf, 0, 1, main = "CDF of X", ylab = "Cumulative probability")

n <- 100000
U <- runif(n)           ## Step 1: Generate Uniform(0,1) rv's
y <- my_cdf_inverse(U)  ## Step 2: Apply inverse_cdf to Uniform(0,1) rv's

par(mfrow=c(1,1))
hist(y, prob = TRUE, main = "Sampled values with true pdf drawn on top")
curve(my_pdf, 0, 1, add = TRUE)


################
## Problem 2: ##
################


## Suppose we're interested in sampling from the beta(2,5) distribution
## but we don't want to use R's built in function. We proceed using 
## rejection sampling. Recall that the pdf of a beta(2,5) is given by 
## f(x) = 30 * x * (1 - x)^4

## a) Can we use the Unif(0,1) distribution as the enveloping distribution?
## Why or why not? Use plots to answer this question.

## b) Graphically find an M st M * Unif(0, 1) will envelope f(x). Compute
## The rejection probability for this choice of M.

## c) What is the smallest such value M we can find? What is the acceptance 
## probability for this choice of M?

## d) Implement rejection sampling to sample from the beta(2,5) distribution.
## Use two envelope distributions: one using the optimal M you found in part c,
## another using M = 10. 

## e) Plot your histograms in two separate plots with the true distribution 
## overlain on both histograms.


###############
## Solutions ##
###############


## First, look at density graphically. Play with M. Find and use the optimal 
## M (given in the lab session). I'm using a suboptimal M=5 here
M <- 5
my_beta <- function(x){
  30 * x * (1 - x)^4
}
curve(my_beta, 0, 1, ylim = c(0, M))  ## How does M=5 do?
abline(h = M)

## Compute acceptance_prob
acceptance_prob <- function(x){
  (30 * x * (1 - x)^4) / M
}

n <- 10000
U <- runif(n)               ## Drawing from g(x)
y <- acceptance_prob(U)     ## Compute acceptance probability
accepted <- rbinom(n, 1, y) ## Sample from acceptance probabilities
mean(accepted)

## Plot results
hist(U[accepted == 1], prob = TRUE, main = "Histogram of samples from f(x)", xlab = "x")
curve(my_beta, 0, 1, ylim = c(0, 5), add = TRUE)




################
## Problem 3: ##
################


## Suppose we're interested in sampling from the beta(2,5) distribution
## but we don't want to use R's built in function. We proceed using 
## rejection sampling. Recall that the pdf of a beta(2,5) is given by 
## f(x) = 30 * x * (1 - x)^4

## a) Can we use the Normal(0,1) distribution as the enveloping distribution?
## Why or why not? Use plots to answer this question.

## b) Graphically find an M st M * Normal(0,1) will envelope f(x). Compute
## The rejection probability for this choice of M.

## c) Implement rejection sampling to sample from the beta(2,5) distribution.
## What modification should we make to our algorithm so that we don't sample
## points outside of the domain of f(x)

## d) Plot your histogram with the true distribution overlain.


###############
## Solutions ##
###############


## First, look at density graphically. Play with M
my_beta <- function(x){
  30 * x * (1 - x)^4
}

M <- 7
my_normal <- function(x){
  M * dnorm(x)
}

curve(my_beta, 0, 1, ylim = c(0, 5), main = "pdf of f(x)")
curve(my_normal, 0, 1, ylim = c(0, 5), add = TRUE)


## Compute acceptance_prob
acceptance_prob <- function(x){
  (30 * x * (1 - x)^4) / (M * dnorm(x))
}

n <- 1000000
X <- rnorm(n)                ## Drawing from g(x)
X <- X[(0 <= X) & (X <= 1)]  ## Throw away points outside of the domain of f(x)

y <- acceptance_prob(X)     ## Compute acceptance probability
accepted <- rbinom(n, 1, y) ## Sample from acceptance probabilities
mean(accepted)

## Plot results
hist(X[accepted == 1], prob = TRUE, main = "Histogram of samples from f(x)", xlab = "x")
curve(my_beta, 0, 1, ylim = c(0, 5), add = TRUE)



