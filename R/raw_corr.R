#!/usr/bin/Rscript
#  raw_corr.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.27.2018

## Calculate a raw correlation matrix, and examine some of its entries.

# As outlined in http://www.tqmp.org/RegularArticles/vol09-2/p079/p079.pdf, 
# latent vars with only 2 variables should be interpreted with caution unless,
# they are correlated with r > 0.7, and little correlation with the rest

library(psych)
library(corpcor)

#Load up the full data
sp17 <- read.csv("./data/sp_17.csv")
fl17 <- read.csv("./data/fl_17.csv")

#Load up the indicators
ind <- read.csv('./data/processed_metrics_sp_18.csv')

#Extract just the measurements
sp17_facs <- sp17[,15:238]
fl17_facs <- fl17[,14:248]

#A principle components analysis:
fit <- principal(sp17_facs, nfactors=10, rotate="varimax")
capture.output(print(fit), 
    file = './output/pcasp17.txt')
fit <- principal(fl17_facs, nfactors=10, rotate="varimax")
capture.output(print(fit), 
    file = './output/pcafl17.txt')


cor.shrink(ind)
capture.output(print(fit),
    file = './output/cor_mat_18.txt')
