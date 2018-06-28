#!/usr/bin/Rscript
#  R/make_corr_again.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.27.2018

## A second attempt at making correlation tables.

library(gsubfn)

## Keys for each variable
source('R/cor2table.R')

## Make the correlation tables.
#Read in data
datapath <- './RData'
files <- list.files(datapath)
outnames <- strsplit(files, '_')
file_df <- data.frame(
              name = sapply(outnames, function(x) paste(x[1:2], collapse = '_')),
              semester = sapply(outnames, function(x) paste(x[3:4], collapse = '_')),
              file = files)
file_df$name <- as.character(file_df$name)
file_df$file <- as.character(file_df$file)


## LeadChal corrs
# Collect correlation of each variable with leadChal
leadChal_corrs <- list()
for (file in file_df$file) {
    load(paste(datapath, '/', file, sep = ''))
    vars <- colnames(bayes_fit$mean)
    sem <- strapplyc(file, ".._1.", simplify = TRUE)

    # Get the pertinent rows
    targ_ind <- which(vars=='leadChal')
    corrs <- bayes_fit$mean[targ_ind,-targ_ind]

    # Store them
    it <- 0
    for (var in names(corrs)) {
        it <- it + 1
        if (var %in% names(leadChal_corrs)) {
            leadChal_corrs[[var]][[sem]] <- corrs[[it]]
        } else {
            leadChal_corrs[[var]] <- list()
            leadChal_corrs[[var]][[sem]] <- corrs[[it]]
        }
    }
}

# Turn it into a pretty dataframe.
vars <- names(leadChal_corrs)
sems <- c('sp_17', 'fl_17', 'sp_18')
corr_list <- list()
for (sem in sems) {
    corr_list[[sem]] <- sapply(vars, function(var) {
                       if (sem %in% names(leadChal_corrs[[var]])) {
                           return(leadChal_corrs[[var]][[sem]])
                       } else {
                           return(NA)
                       }
              })
}
corr_df <- data.frame(corr_list)
