#!/usr/bin/Rscript
#  sem_1.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.16.2018

## Fit the SEM for the Spring and Fall semesters of 2017.

require(lavaan)
require(blavaan)

files <- c("./data/proc_met_fl_17.csv", "./data/proc_met_sp_17.csv")

for (file in files) {
    df <- read.csv(file)

    time <- strsplit(strsplit(strsplit(file, '/')[[1]][3], '\\.')[[1]][1], 'proc_met_')[[1]][2]
    # Define the model
    model <- '  #Measurement Model:
                ffmq =~ ffmq_de + ffmq_aa + ffmq_nj + ffmq_nr
                leadChal =~ leadChal_emb + leadChal_func + leadChal_pers
                uh =~ uh_hi + uh_lo

                #Latent Structure
                leadChal ~ ffmq + uh #+ ffmq:uh

                #Correlation Structure
                uh ~~ ffmq

                #Variance structure
                #ffmq_de ~~ ffmq_de
                #ffmq_aa ~~ ffmq_aa
                #ffmq_nj ~~ ffmq_nj
                #ffmq_nr ~~ ffmq_nr

                #leadChal_emb ~~ leadChal_emb
                #leadChal_func ~~ leadChal_func
                #leadChal_pers ~~ leadChal_pers

                #uh_hi ~~ uh_hi
                #uh_lo ~~ uh_lo

                #leadChal ~~ leadChal
                #uh ~~ uh
                #ffmq ~~ ffmq
                '
    fit <- sem(model, data = df)
    capture.output(summary(fit),
                   file = paste('./output/class_sem_', time, '.txt', sep = ''))
    fit <- bsem(model, data = df)
    capture.output(summary(fit),
                   file = paste('./output/bayes_sem_', time, '.txt', sep = ''))
}
