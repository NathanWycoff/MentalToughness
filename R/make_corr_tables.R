#!/usr/bin/Rscript
#  R/make_corr_tables.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.21.2018

## Make a nice table from our correlations data.
source('R/cor2table.R')

#TODO: One for each semester.

#Read in data
load('./RData/corr_2_corr.RData')

print(cor2table(bayes_fit$mean, bayes_fit$lb, bayes_fit$ub),
      file = './latex_out/corr_2_data.tex')
