#!/usr/bin/Rscript
#  R/bayes_cov_func.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 05.23.2018

## The funciton definiton for Bayesian covariance estimation.
require(rstan)
require(mvtnorm)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#' @param X A matrix of observations which a covariance matrix is desired.
#' @param tocomp A list of length 2 integer vectors, which will compare the correlation between variables  tocomp[1] and tocomp[2] with the correlation of tocomp[1] with all other variables, and determine the probability that the former correlation is the maximal one. 
bayes_cov <- function(X, tocomp = NULL) {
    P <- ncol(X)
    N <- nrow(X)

    param_names <- colnames(X)

    ## Declare priors
    nu <- P + 1
    LAMBDA <- diag(P)
    b <- rep(0, P)
    xi <- rep(sqrt(1e3), P)

    mu_0 <- rep(0, P)
    SIGMA_mu_0 <- diag(rep(1e3, P))

    # Built in
    stan_dat <- list(
                     N = N,
                     P = P,
                     nu = nu,
                     LAMBDA = LAMBDA,
                     b = b,
                     xi = xi,
                     mu_0 = mu_0,
                     SIGMA_mu_0 = SIGMA_mu_0,
                     X = X)

    iters <- 5e2
    fit <- stan(file = 'R/bayes_covar.stan', data = stan_dat,
                iter = iters, chains = 2, control = list(max_treedepth = 15))

    ret <- list()
    ret$mean <- matrix(summary(fit, pars = "SIGMA", probs = c(0.5))$summary[,'mean'], ncol = P)
    colnames(ret$mean) <- param_names
    rownames(ret$mean) <- param_names
    ret$lb <- matrix(summary(fit, pars = "SIGMA", probs = c(0.05))$summary[,'5%'], ncol = P)
    colnames(ret$lb) <- param_names
    rownames(ret$lb) <- param_names
    ret$ub <- matrix(summary(fit, pars = "SIGMA", probs = c(0.95))$summary[,'95%'], ncol = P)
    colnames(ret$ub) <- param_names
    rownames(ret$ub) <- param_names
    ret$conv <- summary(fit)$summary[,'Rhat']

    # Do pairwise comparisons if desired
    if (!is.null(tocomp)) {
        # Translate each covar mat to a correlation mat
        SIG_dat <- extract(fit, 'SIGMA')[[1]]
        cors <- lapply(1:iters, function(i) cov2cor(SIG_dat[i,,]))

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

    return(ret)
}
