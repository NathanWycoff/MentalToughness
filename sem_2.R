#!/usr/bin/Rscript
#  sem_2.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 04.05.2018

## Do a SEM for Spring 2018

require(lavaan)
require(blavaan)

file <- "./data/proc_met_sp_18.csv"

df <- read.csv(file)

#Get rid of overall s ales just to be safe.
df <- df[,grep('\\_', colnames(df))]


# Define the model
model_noint <- '
    #Measurement Model
    
 '


## For reference, the last SEM
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

