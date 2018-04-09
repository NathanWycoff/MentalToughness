#!/usr/bin/Rscript
#  sem_1.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.16.2018

## Do a SEM for the 2017 school year.

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
                dis =~ dis_avd + dis_int

                #Latent Structure
                leadChal ~ dis 
                leadChal ~ ffmq
                '

    model_int <- '  #Measurement Model:
                ffmq =~ ffmq_de + ffmq_aa + ffmq_nj + ffmq_nr
                leadChal =~ leadChal_bp1 + leadChal_bp2 + leadChal_bp3 
                dis =~ dis_avd + dis_int

                #Latent Structure
                leadChal ~ dis + ffmq_dis_int
                leadChal ~ ffmq
                '

    ### Estimate the latent interaction using the product of the expectations
    ### of the factors.
    #First pass to get loadings
    fit_class <- sem(model_noint, data = df)
    fit_bayes <- bsem(model_noint, data = df)

    ## First for the classical output
    #Get the expected value of the latent factors
    #ffmq_coefs <- c(1,coef(fit_class)[1:3])
    ffmq_coefs <- standardizedSolution(fit_class)[1:4, 4]
    ffmq_coefs <- ffmq_coefs / sum(ffmq_coefs)
    ffmq_mean <- rowMeans(as.matrix(df[,c('ffmq_de', 'ffmq_aa', 'ffmq_nj', 'ffmq_nr')]) %*% 
                    diag(ffmq_coefs))
    #dis_coefs <- c(1, coef(fit_class)[6])
    dis_coefs <- standardizedSolution(fit_class)[8:9, 4]
    dis_coefs <- dis_coefs / sum(dis_coefs)
    dis_mean <- rowMeans(as.matrix(df[,c('dis_avd', 'dis_int')]) %*% 
                    diag(dis_coefs))

    #Center the latent variables
    ffmq_mean <- ffmq_mean - mean(ffmq_mean)
    dis_mean <- dis_mean - mean(dis_mean)

    #Get a guess of their interaction
    ffmq_dis_int <- dis_mean * ffmq_mean

    df_class <- df
    df_class$ffmq_dis_int <- ffmq_dis_int

    ## Then for the Bayes output
    #Get the expected value of the latent factors
    #ffmq_coefs <- c(1,coef(fit_bayes)[1:3])
    ffmq_coefs <- standardizedSolution(fit_class)[1:4,4]
    ffmq_coefs <- ffmq_coefs / sum(ffmq_coefs)
    ffmq_mean <- rowMeans(as.matrix(df[,c('ffmq_de', 'ffmq_aa', 'ffmq_nj', 'ffmq_nr')]) %*% 
                    diag(ffmq_coefs))
    #dis_coefs <- c(1, coef(fit_bayes)[6])
    dis_coefs <- standardizedSolution(fit_class)[8:9,4]
    dis_coefs <- dis_coefs / sum(dis_coefs)
    dis_mean <- rowMeans(as.matrix(df[,c('dis_avd', 'dis_int')]) %*% 
                    diag(dis_coefs))

    #Center the latent variables
    ffmq_mean <- ffmq_mean - mean(ffmq_mean)
    dis_mean <- dis_mean - mean(dis_mean)

    #Get a guess of their interaction
    ffmq_dis_int <- dis_mean * ffmq_mean

    df_bayes <- df
    df_bayes$ffmq_dis_int <- ffmq_dis_int

    ## Fit the complete model
    fit <- sem(model_int, data = df_class)
    capture.output(summary(fit),
                   file = paste('./output/sem1/class_sem_1_', time, '.txt', sep = ''))
    capture.output(standardizedSolution(fit),
                   file = paste('./output/sem1/std_class_sem_1_', time, '.txt', sep = ''))
    fit <- bsem(model_int, data = df_bayes)
    capture.output(standardizedSolution(fit),
                   file = paste('./output/sem1/std_bayes_sem_1_', time, '.txt', sep = ''))
    capture.output(summary(fit),
                   file = paste('./output/sem1/bayes_sem_1_', time, '.txt', sep = ''))
}
