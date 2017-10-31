##################################################################
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~/// Stat 102C Lab 1 R Code \\\~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~ Levon Demirdjian ~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~ March 30 2016 ~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##################################################################


################
## Problem 1: ##
################


## Let's say we want to find the integral, from 0 to 1, of the following
## nasty function:
nasty_function <- function(x){
  6*pi * (x^4)*exp(-x^2)*(log(x) + 3^x)/((x+6)^(0.5))
}

## Of course, we can use R's built in 1-d integration:
integrate(nasty_function, lower = 0, upper = 1)


## We can use random sampling to approximate this integral to
## arbitrary precision:
n <- 1000
u <- runif(n, min = 0, max = 1)
mean(nasty_function(u))


## What happens if we repeat this process many times?
## Could you predict what will happen?
## What happens as n goes to infinity
results <- c()
for(i in 1:10000){
  u <- runif(n, min = 0, max = 1)
  results[i] <- mean(nasty_function(u))
}

hist(results, prob = TRUE, main = "Histogram of 10000 approximations of nasty function")


## ~~~~~~~~~~~~~~~~ ##  
## ~~ Question 1 ~~ ##
## ~~~~~~~~~~~~~~~~ ##

## Part a:
## Repeat the above integration, except now integrate the function from 0 to 5.
## Verify your results using R's "integrate" function. Plot the distribution of 
## approximations using a histogram

## Part b:
## Repeat the above integration, except now integrate the function from 3 to 7.
## Verify your results using R's "integrate" function. Plot the distribution of 
## approximations using a histogram



## ~~~~~~~~~~~~~~~~ ##  
## ~~ Question 2 ~~ ##
## ~~~~~~~~~~~~~~~~ ##

## What if I asked you now to sample from a normal(0,1) distribution instead 
## of uniform? Can you calculate the following integral??

nasty_function2 <- function(x){
  (1/sqrt(2*pi)) * exp(-0.5 * x^2) * (x^5) 
}

integrate(nasty_function2, lower = -Inf, upper = Inf)



## We can use random sampling to approximate this integral to
## arbitrary precision:
n <- 10000
u <- rnorm(n, mean = 0, sd = 1)
mean(u^5)

results <- c()
for(i in 1:10000){
  u <- rnorm(n, mean = 0, sd = 1)
  results[i] <- mean(u^5)
}

hist(results, prob = TRUE, main = "Histogram of 10000 approximations of nasty function2")




