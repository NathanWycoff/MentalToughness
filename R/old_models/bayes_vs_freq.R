#!/usr/bin/Rscript
#  R/bayes_vs_freq.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.27.2018

##Sometimes, the bayesian and frequentist SEM funcs return very different vals.
## Who's right?
require(lavaan)
require(blavaan)

#Generate some data from the generative model
#Define some params
n <- 100
int_ffmq <- 2.3
int_uh <- 1.2
int_leadChal <- 0.3

ffmq_to_uh <- 1.3
uh_to_leadChal <- 0.2
uh_ffmq_to_leadChal <- -0.4
ffmq_to_leadChal <- 2.3

#Gen the actual data
ffmq <- rnorm(n) + int_ffmq
uh <- ffmq * ffmq_to_uh + int_uh + rnorm(n)
leadChal <- uh*uh_to_leadChal + ffmq * ffmq_to_leadChal + 
    ffmq*uh*uh_ffmq_to_leadChal + int_leadChal + rnorm(n)

#The model description
uh_ffmq <- uh * ffmq
model_desc <- "
    #Relationships
    uh ~ ffmq
    leadChal ~ uh + ffmq
"

#Store all the data
df <- data.frame(ffmq, uh, leadChal, uh_ffmq)

#Do things the Bayesian way, taking full account of uncertainty
fit_bayes <- bsem(model_desc, data = df)
summary(fit_bayes)

#To contrast to the old way of doing things, fit things using asymptotics
fit_class <- sem(model_desc, data = df)
summary(fit_class)
