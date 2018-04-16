#!/usr/bin/Rscript
#  R/sem_2_alt_1.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 04.09.2018

## An alternative SEM for the 2018 year.

require(lavaan)
require(blavaan)

#Read in data
file <- "./data/proc_met_sp_18.csv"
df <- read.csv(file)
#Get rid of overall s ales just to be safe.
df <- df[,grep('\\_', colnames(df))]

time <- 'sp_18'

# Define the model
model <- '  #Measurement Model:
    ffmq =~ ffmq_de + ffmq_aa + ffmq_nj + ffmq_nr
    leadChal =~ leadChal_bp1 + leadChal_bp2 + leadChal_bp3 
    dis =~ dis_avd + dis_int
    mt =~ mt_bp1 + mt_bp2 + mt_bp3
    lnr =~ lnr_bp1 + lnr_bp2 + lnr_bp3

    #Latent Structure
    lnr ~ ffmq + dis
    mt ~ lnr
    leadChal ~ mt

    #Correlations
    ffmq ~~ dis
    #ffmq ~~ mt
    dis ~~ mt
    '

#Fit the SEM
fit_class <- sem(model, data = df)
capture.output(summary(fit_class),
               file = paste('./output/sem2a2/class_sem_2a2_', time, '.txt', sep = ''))
capture.output(standardizedSolution(fit_class),
               file = paste('./output/sem2a2/std_class_sem_2a2_', time, '.txt', sep = ''))
capture.output(fitMeasures(fit_class),
              file = paste('./output/sem2a2/fit_class_sem_2a2_', time, '.txt', sep = ''))
fit_bayes <- bsem(model, data = df)
capture.output(summary(fit_bayes),
               file = paste('./output/sem2a2/bayes_sem_2a2_', time, '.txt', sep = ''))
capture.output(standardizedSolution(fit_bayes),
               file = paste('./output/sem2a2/std_bayes_sem_2a2_', time, '.txt', sep = ''))
capture.output(fitMeasures(fit_bayes),
              file = paste('./output/sem2a2/fit_bayes_sem_2a2_', time, '.txt', sep = ''))
