#!/usr/bin/Rscript
#  R/bayes_cov_func.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 05.23.2018

## The funciton definiton for Bayesian covariance estimation.
require(rjags)
require(mvtnorm)

#' @param X A matrix of observations which a covariance matrix is desired.
#' @param label A string giving a name for the MCMC plot diagnostic.
#' @param tocomp A list of length 2 integer vectors, which will compare the correlation between variables  tocomp[1] and tocomp[2] with the correlation of tocomp[1] with all other variables, and determine the probability that the former correlation is the maximal one. 
bayes_cov <- function(X, label, tocomp = NULL) {
    P <- ncol(X)
    N <- nrow(X)

    # Declare priors
    nu <- 2
    b <- rep(0, P)
    xi <- rep(sqrt(1000), P)

    mu_0 <- rep(0, P)
    SIGMA_mu_0 <- diag(rep(1e-3, P))

    #Compile the model
    n.chains <- 2
    iters <- 5e3
    covar <- jags.model('R/bayes_covar.jags',
        data = list(N = N, X = X, P = P, nu = nu, xi = xi,
                    mu_0 = mu_0, SIGMA_mu_0 = SIGMA_mu_0),
        n.chains = n.chains)

    #Burn-in
    update(covar, 1e3)

    #Sample from the posterior
    samp <- coda.samples(covar,
        variable.names=c("mu", "SIGMA_inv"),
                n.iter=iters)

    SIG_dat <- lapply(1:iters, function(i) {
               row <- samp[[1]][i,]
              SIGinv <- row[grepl("SIGMA", names(row))]
              SIGinv <- matrix(SIGinv, ncol = P)
              SIG <- solve(SIGinv)
              colnames(SIG) <- rownames(SIG) <- colnames(X)
              return(SIG)
                })

    cors <- lapply(1:iters, function(i) cov2cor(SIG_dat[[i]]))

    # Convert to correlations
    ret <- list()
    ret$mean <- matrix(NA, nrow = P, ncol = P)
    ret$lb <- matrix(NA, nrow = P, ncol = P)
    ret$ub <- matrix(NA, nrow = P, ncol = P)
    rownames(ret$mean) <- colnames(ret$mean) <- colnames(X)
    rownames(ret$lb) <- colnames(ret$lb) <- colnames(X)
    rownames(ret$ub) <- colnames(ret$ub) <- colnames(X)
    for (p1 in 1:P) {
        for (p2 in 1:p1) {
            ret$mean[p1,p2] <- ret$mean[p2,p1] <- 
                mean(sapply(1:iters, function(i) cors[[i]][p1, p2]))
            ret$lb[p1,p2] <- ret$lb[p2,p1] <- 
                quantile(sapply(1:iters, function(i) cors[[i]][p1, p2]), 0.025)
            ret$ub[p1,p2] <- ret$ub[p2,p1] <- 
                quantile(sapply(1:iters, function(i) cors[[i]][p1, p2]), 0.975)
        }
    }

    # Do pairwise comparisons if desired
    if (!is.null(tocomp)) {
        # Translate each covar mat to a correlation mat
        cors <- lapply(1:iters, function(i) cov2cor(SIG_dat[[i]]))

        probmax <- rep(NA, length(tocomp))
        i <- 0
        for (tc in tocomp) {
            i <- i + 1
            probmax[i] <- mean(sapply(1:iters, function(i) {
                                         a <- cors[[i]]
                                         diag(a) <- -2
                                         which.max(a[tc[1],]) == tc[2]
                            }))
        }
        ret$probmax <- probmax
    }

    # Save plots of the MCMC to visually assess convergence.
    pdf(paste('images/mcmc_output/', label, "_mcmc_plot.pdf", sep = ''))
    plot(samp)
    dev.off()

    return(ret)
}
