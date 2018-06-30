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
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili|lnr$|leadChal$)', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili|leadChal$)', colnames(ind))]
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
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili|lnr$|leadChal$)', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili|leadChal$)', colnames(ind))]
    }

    # Pairwise GPA correlations.
    out_name <- 'gpa'
    pair_cor <- sapply(cor_cols, function(i) {
                       ret <- cor.test(ind$gpa, ind[,i])
                       c(ret$conf.int[1], ret$estimate, ret$conf.int[2], ret$p.value)
                   })
    row.names(pair_cor) <- c('lb', 'point', 'ub', 'pval')
    capture.output(print(pair_cor),
                   file = paste('./output/corr/', out_name, '_point', time, '.txt', sep = ''))

    ## Correlations of the MTLS with scales relevant to everyday discomfort:
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

    ##Correlations of the MTLS with scales relevant to personality:
    #Specify the columns the correlations of which are to be analyzed
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(grt|lnr$|bfi_|ffmq|mt|har|leadChal)$', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(grt|bfi_|ffmq|leadChal)$', colnames(ind))]
    }

    #Estimate a correlation matrix
    out_name <- 'mt_corr'
    bayes_fit <- bayes_cov(ind[,cor_cols])
    save_covar_info(ind[,cor_cols], bayes_fit, out_name)

    ## Correlation matrix for personality
    #Specify the columns the correlations of which are to be analyzed
    cor_cols <- colnames(ind)[grep('(bfi_|ffmq|leadChal$)', colnames(ind))]

    #Estimate a correlation matrix
    out_name <- 'pers_corr'
    bayes_fit <- bayes_cov(ind[,cor_cols])
    save_covar_info(ind[,cor_cols], bayes_fit, out_name)
}
