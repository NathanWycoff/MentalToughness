#!/usr/bin/Rscript
#  raw_corr.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.27.2018

## Calculate a correlation matrix between some factors

library(psych)
library(corpcor)

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
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/lead_corr_', time, '.txt', sep = ''))


    ## Correlation matrix for discomfort scales
    #Specify the columns the correlations of which are to be analyzed
    cor_cols <- colnames(ind)[grep('(sc.hw|uh.vmi|dis|uh|brs|leadChal)$', colnames(ind))]

    #Estimate a correlation matrix
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/disscal_corr_', time, '.txt', sep = ''))


    ## Correlation matrix for mental toughness scales
    #Specify the columns the correlations of which are to be analyzed
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(grt|bfi_consc|mt|har|leadChal)$', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(grt|bfi_consc|leadChal)$', colnames(ind))]
    }

    #Estimate a correlation matrix
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/mt_corr_', time, '.txt', sep = ''))


    ## Correlation matrix for personality
    #Specify the columns the correlations of which are to be analyzed
    cor_cols <- colnames(ind)[grep('(bfi_|ffmq$|leadChal$)', colnames(ind))]

    #Estimate a correlation matrix
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/pers_corr_', time, '.txt', sep = ''))


    ## A second correlation matrix
    #Specify the columns the correlations of which are to be analyzed
    # Do the 18 scales if necessary
    if (length(grep('18', time)) > 0) {
        cor_cols <- colnames(ind)[grep('(leadChal|uh|dis|ffmq|lnr|mt)$', colnames(ind))]
    } else {
        cor_cols <- colnames(ind)[grep('(leadChal|uh|dis|ffmq)$', colnames(ind))]
    }

    #Estimate a correlation matrix
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr/corr_2_', time, '.txt', sep = ''))
}
