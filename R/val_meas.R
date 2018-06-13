#!/usr/bin/Rscript
#  R/val_meas.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.12.2018

## Validate self measurements by comparing how they correlate with what others rate.

load("data/measurement_data.RData")

for (cl in colnames(X_others)) {
    tdf <- as.data.frame(cbind(X_others[,cl], X_self[,cl]))
    colnames(tdf) <- c(paste('others_', cl, sep = ''), paste('self_', cl, sep = ''))
    capture.output(summary(lm(tdf)), 
                   file = paste('./output/meas_val/summ_', cl, '.txt', sep = ''))
    capture.output(confint(lm(tdf)), 
                   file = paste('./output/meas_val/ci_', cl, '.txt', sep = ''))
}
