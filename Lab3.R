##################################################################
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~/// Stat 102C Lab 3 R Code \\\~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~ Levon Demirdjian ~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~ April 13 2016 ~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##################################################################


################
## Problem 1: ##
################

## Let the pdf f(x) be given by f(x) = exp(-x) where 0 <= x <= Infinity.
## Furthermore, let Y = sqrt(2 * X)


## a) What distribution is this?

## b) Find functions g and h such that y = h(x) and x = g(y). Is h 
## monotone increasing?

## c) Find the density of Y using the method of transformations.

## d) Find the cdf of Y

## e) Find the inverse cdf of Y

## f) Generate samples from the pdf of Y using the inversion method.

## g) Plot a histogram of your results from f. Overlay the true distribution
## of Y on top of the histogram. On a separate plot, plot the density of X.


###############
## Solutions ##
###############

## Plot pdf of X
pdf_x       <- function(x){exp(-x)}
curve(pdf_x, 0, 10, main = "PDF of X", ylab = "Probability")

## Generate X and Y
n <- 10000
X <- rexp(n)

## Plot results
par(mfrow=c(1,2))
hist(X)
hist(sqrt(2 * X))

cdf_x <- function(x){1 - exp(-0.5 * x^2)}
curve(cdf_x, 0, 7, main = "CDF of X", ylab = "Cumulative probability")

## Plot pdf and cdf side by side
par(mfrow=c(1,2))
curve(my_pdf, 0, 10, main = "PDF of X", ylab = "Probability")
curve(cdf_x, 0, 7, main = "CDF of X", ylab = "Cumulative probability")

## Find inverse cdf
pdf_y <- function(y){y * exp(-.5 * y^2)}
my_inverse_cdf <- function(x){sqrt(-2*log(x))}
curve(my_inverse_cdf, 0, 1, main = "Inverse cdf of X")

## Generate from fY
U <- runif(n)
Y <- my_inverse_cdf(U)

par(mfrow=c(1,1))
hist(Y, prob = TRUE)
curve(pdf_y, 0, 4, main = "PDF of Y", add = TRUE)

par(mfrow=c(1,2))
curve(pdf_x, 0, 4, main = "PDF of X")
curve(pdf_y, 0, 4, main = "PDF of Y")


################
## Problem 2: ##
################

## Let the pdf f(x) be given by f(x) = k * x^4 where 0 <= x <= 1.
## Furthermore, let Y = ln(x)

## a) What distribution is this? What is k?

## b) Find functions g and h such that y = h(x) and x = g(y). Is h 
## monotone increasing?

## c) Find the density of Y using the method of transformations.

## d) Find the cdf of Y

## e) Find the inverse cdf of Y

## f) Generate samples from the pdf of Y using the inversion method.

## g) Plot a histogram of your results from f. Overlay the true distribution
## of Y on top of the histogram. On a separate plot, plot the density of X.


###############
## Solutions ##
###############

## Density of x
pdf_x <- function(x){5 * x^4}

## Find inverse cdf
pdf_y <- function(y){5 * exp(5 * y)}
cdf_y <- function(y){exp(5 * y)}
my_inverse_cdf <- function(y){0.2 * log(y)}

## Generate from fY
U <- runif(n)
Y <- my_inverse_cdf(U)

par(mfrow=c(1,1))
hist(Y, prob = TRUE)
curve(pdf_y, -5, 0, main = "PDF of Y", add = TRUE)

par(mfrow=c(1,2))
curve(pdf_x, 0, 1, main = "PDF of X")
curve(pdf_y, -5, 0, main = "PDF of Y")

