#!/usr/bin/Rscript
#  bigger_model.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.26.2018

## Fit a set of larger models on the 2018 dataset

##Run the models using LAVAAN - a classical-frequentist toolbox
##as well as BLAVAAN, a wrapper for JAGS which allows similar Bayesian modeling.
datasets <- list(fl17 = fl17, sp17 = sp17)
models <- list(hypv1 = hypv1_desc)
for (i in 1:length(datasets)) {
    df <- datasets[[i]]
    d_name <- names(datasets)[i]

    for (j in 1:length(models)) {
        model_desc <- models[[j]]
        model_name <- names(models)[i]

        #Do things the Bayesian way, taking full account of uncertainty
        fit_bayes <- bsem(model_desc, data = df)
        capture.output(summary(fit_bayes), 
            file = paste('bayes_out_', d_name, '_', model_name, '.txt.', sep = ''))

        #To contrast to the old way of doing things, fit things using asymptotics
        fit_class <- sem(model_desc, data = df)
        capture.output(summary(fit_class), 
            file = paste('class_out_', d_name, '_', model_name, '.txt.', sep = ''))
    }
}
