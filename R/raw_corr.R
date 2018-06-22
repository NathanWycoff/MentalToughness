#!/usr/bin/Rscript
#  raw_corr.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.27.2018

## Calculate a correlation matrix between some factors

library(psych)
library(corpcor)
source('R/bayes_cov_func.R')

#' A function which makes some output for covariance matrices.
#'
#'
#'
#' @param X A matrix of which covariance information is desired
#' @param bayes_fit The result of the function bayes_cov
#' @param out_name The name for the files which will be saved.
#' @return Nothing, used for side effect of saving data.
save_covar_info <- function(X, bayes_fit, out_name) {

    # Store the stuff for later processing.
    save(X, bayes_fit, file = paste('./RData/', out_name, '_corr.RData', sep = ''))

    # Classical results
    capture.output(print(cor(X)),
                   file = paste('./output/corr/', out_name, '_point', time, '.txt', sep = ''))

    p <- ncol(X)
    freq_lb <- matrix(NA, nrow = p, ncol = p)
    freq_ub <- matrix(NA, nrow = p, ncol = p)
    freq_pval <- matrix(NA, nrow = p, ncol = p)
    for (i in 1:ncol(X)) {
        for (j in 1:i) {
            ret <- cor.test(X[,i], X[,j])
            freq_lb[i,j] <- freq_lb[j,i] <- ret$conf.int[1]
            freq_ub[i,j] <- freq_ub[j,i] <- ret$conf.int[2]
            freq_pval[i,j] <- freq_pval[j,i] <- ret$p.val
        }
    }

    colnames(freq_lb) <- colnames(freq_ub) <- rownames(freq_lb) <- 
        rownames(freq_ub) <- cor_cols

    capture.output(print(cor(X)),
                   file = paste('./output/corr/', out_name, '_point', time, '.txt', sep = ''))
    capture.output(print(freq_lb),
                   file = paste('./output/corr/', out_name, '_freq_lb', time, '.txt', sep = ''))
    capture.output(print(freq_ub),
                   file = paste('./output/corr/', out_name, '_freq_ub', time, '.txt', sep = ''))
    capture.output(print(freq_pval),
                   file = paste('./output/corr/', out_name, '_freq_pval', time, '.txt', sep = ''))

    # Bayesian results
    capture.output(print(cov2cor(bayes_fit$mean)),
                   file = paste('./output/corr/', out_name, '_mean', time, '.txt', sep = ''))
    capture.output(print(cov2cor(bayes_fit$lb)),
                   file = paste('./output/corr/', out_name, '_bayes_lb', time, '.txt', sep = ''))
    capture.output(print(cov2cor(bayes_fit$ub)),
                   file = paste('./output/corr/', out_name, '_bayes_ub', time, '.txt', sep = ''))
    capture.output(print(bayes_fit$conv),
                   file = paste('./output/corr/', out_name, '_conv', time, '.txt', sep = ''))

    if (!is.null(bayes_fit$probmax)) {
    capture.output(print(bayes_fit$probmax),
                   file = paste('./output/corr/', out_name, '_probmax', time, '.txt', sep = ''))
    }
}



#Load up the full data
files <- c("./data/proc_met_fl_17.csv", "./data/proc_met_sp_17.csv", "./data/proc_met_sp_18.csv")

## Create a correlation matrix for each.
for (file in files) {
    #Load up the indicators
    time <- strsplit(strsplit(strsplit(file, '/')[[1]][3], '\\.')[[1]][1], 'proc_met_')[[1]][2]
    ind <- read.csv(file)

    ## Correlation matrix for leadership scales.
    #Specify the columns the correlations of which are to be analyzed
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili_|lnr$|leadChal$)', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili_|leadChal$)', colnames(ind))]
    }

    #Estimate a correlation matrix
    out_name <- 'lead_corr'
    if (length(grep('18', time)) > 0) {
        tocomp <- list(match(c('lnr', 'leadChal'), cor_cols))
        bayes_fit <- bayes_cov(ind[,cor_cols], tocomp = tocomp)
    } else {
        bayes_fit <- bayes_cov(ind[,cor_cols])
    }

    # Save the output
    save_covar_info(ind[,cor_cols], bayes_fit, out_name)

    ## Compare GPA to some other things, pairwise.
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili_|lnr$|leadChal$)', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili_|leadChal$)', colnames(ind))]
    }

    #
    out_name <- 'gpa'
    pair_cor <- sapply(cor_cols, function(i) cor.test(ind$gpa, ind[,i])$conf.int)
    row.names(pair_cor) <- c('lb', 'ub')
    capture.output(print(pair_cor),
                   file = paste('./output/corr/', out_name, '_point', time, '.txt', sep = ''))

    ## Correlation matrix for discomfort scales
    #Specify the columns the correlations of which are to be analyzed
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(lnr$|sc.hw|uh.vmi|dis|uh|brs|leadChal)$', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(sc.hw|uh.vmi|dis|uh|brs|leadChal)$', colnames(ind))]
    }

    #Estimate a correlation matrix
    out_name <- 'disscal_corr'
    bayes_fit <- bayes_cov(ind[,cor_cols])
    save_covar_info(ind[,cor_cols], bayes_fit, out_name)


    ## Correlation matrix for mental toughness scales
    #Specify the columns the correlations of which are to be analyzed
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(grt|lnr$|bfi_consc|mt|har|leadChal)$', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(grt|bfi_consc|leadChal)$', colnames(ind))]
    }

    #Estimate a correlation matrix
    out_name <- 'mt_corr'
    bayes_fit <- bayes_cov(ind[,cor_cols])
    save_covar_info(ind[,cor_cols], bayes_fit, out_name)

    ## Correlation matrix for personality
    #Specify the columns the correlations of which are to be analyzed
    cor_cols <- colnames(ind)[grep('(bfi_|ffmq$|leadChal$)', colnames(ind))]

    #Estimate a correlation matrix
    out_name <- 'pers_corr'
    bayes_fit <- bayes_cov(ind[,cor_cols])
    save_covar_info(ind[,cor_cols], bayes_fit, out_name)

    ## A second correlation matrix
    #Specify the columns the correlations of which are to be analyzed
    # Do the 18 scales if necessary
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(leadChal|uh|dis|ffmq|lnr|mt)$', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(leadChal|uh|dis|ffmq)$', colnames(ind))]
    }

    #Estimate a correlation matrix
    out_name <- 'corr_2'
    bayes_fit <- bayes_cov(ind[,cor_cols])
    save_covar_info(ind[,cor_cols], bayes_fit, out_name)
}
