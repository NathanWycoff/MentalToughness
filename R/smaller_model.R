#!/usr/bin/Rscript
#  smaller_model.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.26.2018

## A smaller model, used for 2017 data
require(blavaan)
require(lavaan)

#Load the data
fl17 <- read.csv('./data/processed_metrics_fl_17.csv')
sp17 <- read.csv('./data/processed_metrics_sp_17.csv')

#Get down some interactions that we need
fl17$uh_ffmq <- fl17$uh * fl17$ffmq
sp17$uh_ffmq <- sp17$uh * sp17$ffmq

##Write up the Models
model_desc <- "
    #Relationships
    uh ~ ffmq
    leadChal ~ uh + uh_ffmq + ffmq
"

##Run the models using LAVAAN - a classical-frequentist toolbox
##as well as BLAVAAN, a wrapper for JAGS which allows similar Bayesian modeling.
#Set PRNG seed so bayes results may be reproduced exactly
set.seed(123)
datasets <- list(fl17 = fl17, sp17 = sp17)
for (i in 1:length(datasets)) {
    df <- datasets[[i]]
    d_name <- names(datasets)[i]

    #Do things the Bayesian way, taking full account of uncertainty
    fit_bayes <- bsem(model_desc, data = df, sample = 1e5)
    capture.output(summary(fit_bayes), 
        file = paste('./output/bayes_out_', d_name, '.txt', sep = ''))

    #To contrast to the old way of doing things, fit things using asymptotics
    fit_class <- sem(model_desc, data = df)
    capture.output(summary(fit_class), 
        file = paste('./output/class_out_', d_name, '.txt', sep = ''))
}
