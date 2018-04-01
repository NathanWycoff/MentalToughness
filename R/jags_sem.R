#!/usr/bin/Rscript
#  jags_sem.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.22.2018

### Do a bayesian SEM model in JAGS
require(rjags)
require(lavaan)
require(Matrix)

## Simulate some data
set.seed(123)
n <- 1000 #Number of observations
ipf <- 3#Indicators per factor
r <- 3#Number of Factors

#Generate each factor
betas <- c(-0.5,-0.2,0.5)
sigmas_z <- rep(1, r)#rgamma(r, 1, 1)
Z <- matrix(rnorm(n*r), nrow = n, ncol = r)
Z[,1] <- rnorm(n, sigmas_z[1])
Z[,2] <- rnorm(n, sigmas_z[2])
Z[,3] <- rnorm(n, sigmas_z[3]) + betas[1] * Z[,1] + betas[2] * Z[,2] + betas[3] * Z[,1] * Z[,2]

#Generate the coordinates of the indicators in factor space
LAMBDA <- as.matrix(bdiag(lapply(1:r, function(i) t(c(1, rgamma(ipf-1, 5, 5))))))

#Generate the error matrix
sigmas_x <- rep(1, ipf*r)#rgamma(ipf*r, 1, 1)
E <- t(do.call(rbind, lapply(1:(ipf*r), function(i) rnorm(n, 0, sigmas_x[i]))))

MU <- Z %*% LAMBDA
X <- MU + E

X <- apply(X, 2, function(i) i - mean(i))

#### ANALYZE USING LAVAAN
df <- data.frame(X)

lavaan_model <- "
    #Measurement Model
    Z1 =~ X1 + X2 + X3
    Z2 =~ X4 + X5 + X6
    Z3 =~ X7 + X8 + X9

    #Regressions
    Z3 ~ Z1 + Z2
"

fit <- cfa(lavaan_model, data = df)
summary(fit)

#### ANALYZE USING JAGS
p <- ipf * r
fact_asgn <- rep(1:r, each = ipf)
jags_model <- "model {
    ##likelihood
    #Measurement Model
    for (i in 1:n) {
        for (j in 1:p) {
            X[i,j] ~ dnorm(lambdas[j] * Z[i, fact_asgn[j]], phis_x[j])
        }
    }

    #latent vars
    for (i in 1:n) {
        Z[i,1] ~ dnorm(eta[1], phis_z[1])
        Z[i,2] ~ dnorm(eta[2], phis_z[2])
        mu3[i] <- eta[3] + beta[1] * Z[i,1] + beta[2] * Z[i,2] + beta[3] * Z[i,1] *  Z[i,2] 
        Z[i,3] ~ dnorm(mu3[i], phis_z[3])
    }


    ##Priors
    for (j in 1:p) {
        sigmas_x[j] ~ dnorm(0, 1e-2)T(0,)
    }
    for (j in 1:r) {
        sigmas_z[j] ~ dnorm(0, 1e-2)T(0,)
    }
    for (j in free_lams) {
        lambdas[j] ~ dnorm(0, 1e-2)
    }
    for (j in fixed_lams) {
        lambdas[j] <- 1
    }
    beta[1] ~ dnorm(0, 1e-2)
    beta[2] ~ dnorm(0, 1e-2)
    beta[3] ~ dnorm(0, 1e-2)

    eta[1] ~ dnorm(0, 1e-2)
    eta[2] ~ dnorm(0, 2e-2)
    eta[3] ~ dnorm(0, 3e-2)

    #Transforms
    for (j in 1:p) {
        phis_x[j] <- 1/(sigmas_x[j])^2
    }
    for (j in 1:r) {
        phis_z[j] <- 1/(sigmas_z[j])^2
    }
}
"

fixed_lams <- which(0:(ipf*r-1) %% ipf == 0)
free_lams <- which(0:(ipf*r-1) %% ipf != 0)

#Compile the model
jm <- jags.model(textConnection(jags_model),
    data = list(X = X, n = nrow(X), p = ncol(X), r = r, fact_asgn = fact_asgn,
                free_lams = free_lams, fixed_lams = fixed_lams),
    n.chains = 2)

#Burn-in
update(jm, 1e2)

#Sample from the posterior
samp <- coda.samples(jm,
    variable.names=c("lambdas", "sigmas_x", "sigmas_z", "beta", "eta"), 
            n.iter=4e2)

summary(samp)$statistics
