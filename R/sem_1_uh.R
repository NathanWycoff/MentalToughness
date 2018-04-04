#!/usr/bin/Rscript
#  sem_1.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.16.2018

## Do a SEM for the 2017 school year, originally done with Unconditional happinesss
## instead of DIS

require(lavaan)
require(blavaan)

files <- c("./data/proc_met_fl_17.csv", "./data/proc_met_sp_17.csv")

for (file in files) {
    df <- read.csv(file)

    #Get rid of overall s ales just to be safe.
    df <- df[,grep('\\_', colnames(df))]

    time <- strsplit(strsplit(strsplit(file, '/')[[1]][3], '\\.')[[1]][1], 'proc_met_')[[1]][2]
    # Define the model
    model_noint <- '  #Measurement Model:
                ffmq =~ ffmq_de + ffmq_aa + ffmq_nj + ffmq_nr
                leadChal =~ leadChal_bp1 + leadChal_bp2 + leadChal_bp3 
                uh =~ uh_hi_p1 + uh_hi_p2 + uh_lo_p1 + uh_lo_p2

                #Latent Structure
                leadChal ~ uh 
                leadChal ~ ffmq
                '

    model_int <- '  #Measurement Model:
                ffmq =~ ffmq_de + ffmq_aa + ffmq_nj + ffmq_nr
                leadChal =~ leadChal_bp1 + leadChal_bp2 + leadChal_bp3 
                uh =~ uh_hi_p1 + uh_hi_p2 + uh_lo_p1 + uh_lo_p2

                #Latent Structure
                leadChal ~ uh + ffmq_uh_int
                leadChal ~ ffmq
                '

    ### Estimate the latent interaction using the product of the expectations
    ### of the factors.
    #First pass to get loadings
    fit_class <- sem(model_noint, data = df)
    fit_bayes <- bsem(model_noint, data = df)

    ## First for the classical output
    #Get the expected value of the latent factors
    ffmq_coefs <- c(1,coef(fit_class)[1:3])
    ffmq_coefs <- ffmq_coefs / sum(ffmq_coefs)
    ffmq_mean <- rowMeans(as.matrix(df[,c('ffmq_de', 'ffmq_aa', 'ffmq_nj', 'ffmq_nr')]) %*% 
                    diag(ffmq_coefs))
    uh_coefs <- c(1, coef(fit_class)[6:8])
    uh_coefs <- uh_coefs / uh_coefs 
    uh_mean <- rowMeans(as.matrix(df[,c('uh_hi_p1', 'uh_hi_p2', 'uh_lo_p1', 'uh_lo_p2')]) %*% 
                    diag(uh_coefs))

    #Center the latent variables
    ffmq_mean <- ffmq_mean - mean(ffmq_mean)
    uh_mean <- uh_mean - mean(uh_mean)

    #Get a guess of their interaction
    ffmq_uh_int <- uh_mean * ffmq_mean

    df_class <- df
    df_class$ffmq_uh_int <- ffmq_uh_int

    ## Then for the Bayes output
    #Get the expected value of the latent factors
    ffmq_coefs <- c(1,coef(fit_bayes)[1:3])
    ffmq_coefs <- ffmq_coefs / sum(ffmq_coefs)
    ffmq_mean <- rowMeans(as.matrix(df[,c('ffmq_de', 'ffmq_aa', 'ffmq_nj', 'ffmq_nr')]) %*% 
                    diag(ffmq_coefs))
    uh_coefs <- c(1, coef(fit_bayes)[6:8])
    uh_coefs <- uh_coefs / uh_coefs 
    uh_mean <- rowMeans(as.matrix(df[,c('uh_hi_p1', 'uh_hi_p2', 'uh_lo_p1', 'uh_lo_p2')]) %*% 
                    diag(uh_coefs))

    #Center the latent variables
    ffmq_mean <- ffmq_mean - mean(ffmq_mean)
    uh_mean <- uh_mean - mean(uh_mean)

    #Get a guess of their interaction
    ffmq_uh_int <- uh_mean * ffmq_mean

    df_bayes <- df
    df_bayes$ffmq_uh_int <- ffmq_uh_int

    ## Fit the complete model
    fit <- sem(model_int, data = df_class)
    capture.output(summary(fit),
                   file = paste('./output/uh_class_sem_', time, '.txt', sep = ''))
    fit <- bsem(model_int, data = df_bayes)
    capture.output(summary(fit),
                   file = paste('./output/uh_bayes_sem_', time, '.txt', sep = ''))
}
