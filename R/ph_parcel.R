#!/usr/bin/Rscript
#  ph_parcel.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.19.2018

## Post-hoc parcelling
#Load up the full data
require(psych)
sp17 <- read.csv("./data/reversed_data_sp_17.csv")
fl17 <- read.csv("./data/reversed_data_fl_17.csv")
sp18 <- read.csv("./data/reversed_data_sp_18.csv")

##Balanced Parcelling for leadChal
lc_cols <- grep('leadChal_', colnames(sp17))
leadChal <- sp17[,lc_cols]
fit <- alpha(leadChal)
cors <- fit$item.stats$r.cor
