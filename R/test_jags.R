#!/usr/bin/Rscript
#  R/test_jags.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 07.08.2018

## Simulate some data from our assumed model.
require(mvtnorm)
require(rjags)
N <- 200
P <- 4

# Generate mean and data
mu <- rnorm(P, 200, 1)
SIGMA <- rWishart(1, P+1, diag(rep(0.1, 4)))[,,1]
X <- rmvnorm(N, mu, SIGMA)
colnames(X) <- paste("X", 1:P, sep = '¯\\_(ツ)_/¯')

# Declare priors
nu <- 2
b <- rep(0, P)
xi <- rep(sqrt(1000), P)

mu_0 <- rep(0, P)
SIGMA_mu_0 <- diag(rep(1e-3, P))

#Compile the model
n.chains <- 2
iters <- 2000
covar <- jags.model('R/bayes_covar.jags',
    data = list(N = N, X = X, P = P, nu = nu, xi = xi,
                mu_0 = mu_0, SIGMA_mu_0 = SIGMA_mu_0),
    n.chains = n.chains)

#Burn-in
update(covar, 1e4)

#Sample from the posterior
samp <- coda.samples(covar,
    variable.names=c("SIGMA_inv"),
            n.iter=iters)

SIG_dat <- lapply(1:iters, function(i) {
           row <- samp[[1]][i,]
          SIGinv <- matrix(row, ncol = P)
          SIG <- solve(SIGinv)
          colnames(SIG) <- rownames(SIG) <- colnames(X)
          return(SIG)
            })

