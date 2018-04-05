#!/usr/bin/Rscript
#  raw_corr.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.27.2018

## Calculate a raw correlation matrix, and examine some of its entries.

# As outlined in http://www.tqmp.org/RegularArticles/vol09-2/p079/p079.pdf, 
# latent vars with only 2 variables should be interpreted with caution unless,
# they are correlated with r > 0.7, and little correlation with the rest

library(bfa)
library(BayesFM)

#Load up the full data
sp17 <- read.csv("./data/sp_17.csv")
fl17 <- read.csv("./data/fl_17.csv")

# Classical EFAs
# Single factor
leadChal_cols <- sp17[,grep('^leadChal', colnames(sp17))]
fit <- factanal(leadChal_cols, factors = 1, rotation = 'promax')
capture.output(print(fit),
               file =  './output/class_efa_sp17_1f.txt')

leadChal_cols <- fl17[,grep('^leadChal', colnames(fl17))]
fit <- factanal(leadChal_cols, factors = 1, rotation = 'promax')
capture.output(print(fit),
               file =  './output/class_efa_fl17_1f.txt')
#Multiple factor
leadChal_cols <- sp17[,grep('^leadChal', colnames(sp17))]
fit <- factanal(leadChal_cols, factors = 4, rotation = 'promax')
capture.output(print(fit),
               file =  './output/class_efa_sp17_mf.txt')

leadChal_cols <- fl17[,grep('^leadChal', colnames(fl17))]
fit <- factanal(leadChal_cols, factors = 4, rotation = 'promax')
capture.output(print(fit),
               file =  './output/class_efa_fl17_mf.txt')

# Bayesian EFAs -- who knows for now
#leadChal_cols <- apply(sp17[,grep('^leadChal', colnames(sp17))], 1, as.numeric)
#fit <- befa(leadChal_cols, iter = 1000)
#fit <- post.sign.switch(post.column.switch(fit))
#capture.ouptut(,
#               file =  './output/class_efa_sp17.txt')
#
#
#leadChal_cols <- apply(sp17[,grep('^leadChal', colnames(sp17))], 1, as.numeric)
#fit <- bfa_gauss(~., data = leadChal_cols, num.factor = 3)
#fit$loadings
