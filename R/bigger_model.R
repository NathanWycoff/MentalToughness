#!/usr/bin/Rscript
#  bigger_model.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.26.2018

## Fit a set of larger models on the 2018 dataset
require(blavaan)
require(lavaan)

#Load the data
sp18 <- read.csv('./data/processed_metrics_sp_18.csv')

#Specify some interactions that we need
sp18$lnr_ffmq <- sp18$lnr * sp18$ffmq
sp18$lnr_har <- sp18$lnr * sp18$har
sp18$lnr_mt <- sp18$lnr * sp18$mt


#Specify some models of interest
hypv1_desc <- "
    #Define latent vars
    coping =~ har + mt
    #Define some relationships
    uh ~ ffmq + coping
    lnr ~ uh
    leadChal ~ lnr + lnr_ffmq#We're missing the coping-lnr interaction here
    coping ~ ffmq
"
hypv2_desc <- "
    #Define some relationships
    uh ~ ffmq + har + mt
    lnr ~ uh
    leadChal ~ lnr + lnr_ffmq + lnr_har + lnr_mt
    #Covariance
    har ~~ mt
"
alt <- "
    #Define latent vars
    coping =~ har + mt
    #Some regressions
    uh ~ ffmq + coping
    lnr ~ uh
    leadChal ~ ffmq + coping
    coping ~ ffmq
"

##Run the models using LAVAAN - a classical-frequentist toolbox
##as well as BLAVAAN, a wrapper for JAGS which allows similar Bayesian modeling.
set.seed(123)
datasets <- list(sp18 = sp18)
models <- list(hypv1_desc = hypv1_desc, hypv2_desc = hypv2_desc, alt = alt)
for (i in 1:length(datasets)) {
    df <- datasets[[i]]
    d_name <- names(datasets)[i]

    for (j in 1:length(models)) {
        model_desc <- models[[j]]
        model_name <- names(models)[i]

        #Do things the Bayesian way, taking full account of uncertainty
        fit_bayes <- bsem(model_desc, data = df)
        capture.output(summary(fit_bayes), 
            file = paste('./output/bayes_out_', d_name, '_', model_name, '.txt', sep = ''))

        #To contrast to the old way of doing things, fit things using asymptotics
        fit_class <- sem(model_desc, data = df)
        capture.output(summary(fit_class), 
            file = paste('./output/class_out_', d_name, '_', model_name, '.txt', sep = ''))
    }
}
