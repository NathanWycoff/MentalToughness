#!/usr/bin/Rscript
#  raw_corr.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.27.2018

## Calculate a raw correlation matrix, and examine some of its entries.

# As outlined in http://www.tqmp.org/RegularArticles/vol09-2/p079/p079.pdf, 
# latent vars with only 2 variables should be interpreted with caution unless,
# they are correlated with r > 0.7, and little correlation with the rest

library(psych)

#Load up the data
sp17 <- read.csv("./data/sp_17.csv")
fl17 <- read.csv("./data/fl_17.csv")

#Extract just the measurements
sp17_facs <- sp17[,15:238]
fl17_facs <- head(fl17[,14:248])

#A principle components analysis:
# RC1 may correspond to ili
# RC2 may correspond to uh
# RC10 
fit <- principal(sp17_facs, nfactors=10, rotate="varimax")
capture.output(print(fit), 
    file = './output/pca.txt')

fit <- factanal(sp17_facs, 3, rotation="varimax")
summary(fit)
