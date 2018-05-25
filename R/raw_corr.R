#!/usr/bin/Rscript
#  raw_corr.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.27.2018

## Calculate a correlation matrix between some factors

library(psych)
library(corpcor)
source('R/bayes_cov_func.R')

#Load up the full data
files <- c("./data/proc_met_fl_17.csv", "./data/proc_met_sp_17.csv", "./data/proc_met_sp_18.csv")

for (file in files) {
    #Load up the indicators
    time <- strsplit(strsplit(strsplit(file, '/')[[1]][3], '\\.')[[1]][1], 'proc_met_')[[1]][2]
    ind <- read.csv(file)

    ## Correlation matrix for leadership scales.
    #Specify the columns the correlations of which are to be analyzed
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili$|lnr$|leadChal$)', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili$|leadChal$)', colnames(ind))]
    }

    #Estimate a correlation matrix
    out_name <- 'lead_corr'
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/', out_name, '_point', time, '.txt', sep = ''))
    if (length(grep('18', time)) > 0) {
        tocomp <- list(match(c('lnr', 'leadChal'), cor_cols))
        bayes_fit <- bayes_cov(ind[,cor_cols], tocomp = tocomp)
    } else {
        bayes_fit <- bayes_cov(ind[,cor_cols])
    }
    capture.output(print(cov2cor(bayes_fit$mean)),
                   file = paste('./output/corr/', out_name, '_mean', time, '.txt', sep = ''))
    capture.output(print(cov2cor(bayes_fit$lb)),
                   file = paste('./output/corr/', out_name, '_lb', time, '.txt', sep = ''))
    capture.output(print(cov2cor(bayes_fit$ub)),
                   file = paste('./output/corr/', out_name, '_ub', time, '.txt', sep = ''))
    capture.output(print(cov2cor(bayes_fit$conv)),
                   file = paste('./output/corr/', out_name, '_conv', time, '.txt', sep = ''))
    if (length(grep('18', time)) > 0) {
    capture.output(print(cov2cor(bayes_fit$probmax)),
                   file = paste('./output/corr/', out_name, '_probmax', time, '.txt', sep = ''))
    }

    ## Correlation matrix for discomfort scales
    #Specify the columns the correlations of which are to be analyzed
    cor_cols <- colnames(ind)[grep('(sc.hw|uh.vmi|dis|uh|brs|leadChal)$', colnames(ind))]

    #Estimate a correlation matrix
    out_name <- 'disscal_corr'
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/', out_name, '_point', time, '.txt', sep = ''))


    ## Correlation matrix for mental toughness scales
    #Specify the columns the correlations of which are to be analyzed
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(grt|bfi_consc|mt|har|leadChal)$', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(grt|bfi_consc|leadChal)$', colnames(ind))]
    }

    #Estimate a correlation matrix
    out_name <- 'mt_corr'
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/', out_name, '_point', time, '.txt', sep = ''))


    ## Correlation matrix for personality
    #Specify the columns the correlations of which are to be analyzed
    cor_cols <- colnames(ind)[grep('(bfi_|ffmq$|leadChal$)', colnames(ind))]

    #Estimate a correlation matrix
    out_name <- 'pers_corr'
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/', out_name, '_point', time, '.txt', sep = ''))


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
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/, 'out_name, '_point', time, '.txt', sep = ''))
}
