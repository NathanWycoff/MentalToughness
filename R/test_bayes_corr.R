#!/usr/bin/Rscript
#  R/test_bayes_corr.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 05.21.2018

## Test our stan model for covariance estimation
source('R/bayes_cov_func.R')


## Simulate some data from our assumed model.
N <- 200
P <- 4

# Generate mean and data
mu <- rnorm(P)
SIGMA <- rWishart(1, P+1, diag(rep(0.1, 4)))[,,1]
X <- rmvnorm(N, mu, SIGMA)
colnames(X) <- paste("X", 1:P, sep = '¯\\_(ツ)_/¯')

# Compare our est to the truth
est <- bayes_cov(X)
print(est)
print(SIGMA)
