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

    #Specify the columns the correlations of which are to be analyzed
    cor_cols <- colnames(ind)[grep('(auth_|tfl_|ili|leadChal$)', colnames(ind))]

    #Estimate a correlation matrix
    capture.output(print(cor.shrink(ind[,cor_cols])),
                   file = paste('./output/corr_', time, '.txt', sep = ''))
}
