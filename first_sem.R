#!/usr/bin/Rscript
#  first_sem.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.24.2018

## Do data analysis for the first semester

require(lavaan)
require(blavaan)

# Load data
load(file = "./data/processed_metrics.RData")

#Write up some of the models
alt_model_1 <- "
    cop_res ~= 
"
