#!/usr/bin/Rscript
#  R/sem_2.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 04.06.2018

## Do the primary SEM on the 2018 data.

require(lavaan)
require(blavaan)

#Read in data
file <- "./data/proc_met_sp_18.csv"
df <- read.csv(file)
#Get rid of overall s ales just to be safe.
df <- df[,grep('\\_', colnames(df))]

time <- 'sp_18'

# Define the model
model_noint <- '  #Measurement Model:
    ffmq =~ ffmq_de + ffmq_aa + ffmq_nj + ffmq_nr
    leadChal =~ leadChal_bp1 + leadChal_bp2 + leadChal_bp3 
    dis =~ dis_avd + dis_int
    mt =~ mt_bp1 + mt_bp2 + mt_bp3
    lnr =~ lnr_bp1 + lnr_bp2 + lnr_bp3

    #Latent Structure
    lnr ~ dis
    leadChal ~ lnr + mt + ffmq

    #Correlations
    dis ~~ ffmq
    dis ~~ mt
    ffmq ~~ mt
    ffmq ~~ lnr
    mt ~~ lnr
    '

model_int <- '  #Measurement Model:
    ffmq =~ ffmq_de + ffmq_aa + ffmq_nj + ffmq_nr
    leadChal =~ leadChal_bp1 + leadChal_bp2 + leadChal_bp3 
    dis =~ dis_avd + dis_int
    mt =~ mt_bp1 + mt_bp2 + mt_bp3
    lnr =~ lnr_bp1 + lnr_bp2 + lnr_bp3

    #Latent Structure
    lnr ~ dis
    leadChal ~ lnr + mt + ffmq + lnr_mt_int + lnr_ffmq_int

    #Correlations
    dis ~~ ffmq
    dis ~~ mt
    ffmq ~~ mt
    ffmq ~~ lnr
    mt ~~ lnr
    '

### Estimate the latent interaction using the product of the expectations
### of the factors.
#First pass to get loadings
fit_class <- sem(model_noint, data = df)
fit_bayes <- bsem(model_noint, data = df)

## First for the classical output
#Get the expected value of the latent factors
ffmq_coefs <- standardizedSolution(fit_class)[1:4,4]
ffmq_coefs <- ffmq_coefs / sum(ffmq_coefs)
ffmq_mean <- rowMeans(as.matrix(df[,c('ffmq_de', 'ffmq_aa', 'ffmq_nj', 'ffmq_nr')]) %*% 
                diag(ffmq_coefs))
mt_coefs <- standardizedSolution(fit_class)[10:12,4]
mt_coefs <- mt_coefs / sum(mt_coefs)
mt_mean <- rowMeans(as.matrix(df[,c('mt_bp1', 'mt_bp2','mt_bp3')]) %*% 
                diag(mt_coefs))
lnr_coefs <- standardizedSolution(fit_class)[13:15,4]
lnr_coefs <- lnr_coefs / sum(lnr_coefs)
lnr_mean <- rowMeans(as.matrix(df[,c('lnr_bp1', 'lnr_bp2','lnr_bp3')]) %*% 
                diag(lnr_coefs))

#Center the latent variables
ffmq_mean <- ffmq_mean - mean(ffmq_mean)
mt_mean <- mt_mean - mean(mt_mean)
lnr_mean <- lnr_mean - mean(lnr_mean)

#Get a guess of some interactions
lnr_mt_int <- lnr_mean * mt_mean
lnr_ffmq_int <- lnr_mean * ffmq_mean

df_class <- df
df_class$lnr_mt_int <- lnr_mt_int
df_class$lnr_ffmq_int <- lnr_ffmq_int

## Fit the complete model
fit <- sem(model_int, data = df_class)
capture.output(summary(fit),
               file = paste('./output/class_sem_', time, '.txt', sep = ''))
capture.output(standardizedSolution(fit),
               file = paste('./output/std_class_sem_', time, '.txt', sep = ''))
fit <- bsem(model_int, data = df_bayes)
capture.output(standardizedSolution(fit),
               file = paste('./output/std_bayes_sem_', time, '.txt', sep = ''))
capture.output(summary(fit),
               file = paste('./output/bayes_sem_', time, '.txt', sep = ''))
