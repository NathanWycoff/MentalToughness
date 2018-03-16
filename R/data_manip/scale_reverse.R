#!/usr/bin/Rscript
#  R/scale_reverse.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.11.2018

## Some scales are reversed in this dataset, meaning that a high reading
## on the scale indicates a low measurement of the attribute of interest.
## This script reverses such scales. After the data has been processed
## through this script, high readings on a scale always indicate high measurements.

files <- c("./data/fl_17.csv", "./data/sp_17.csv", "./data/sp_18.csv")
for (file in files) {
    dat <- read.csv(file)
    time <- strsplit(strsplit(file, '/')[[1]][3], '\\.')[[1]][1]

    ## Most reversed scales are labelled, this code block will take care of those.
    #Create Reversed Scales
    rev_col <- grep('R$', colnames(dat))
    dat[,rev_col] <- apply(dat[,rev_col], 2, function(i) max(i) - i)
    #Change the column name to get rid of the R
    colnames(dat)[rev_col] <- gsub('R$', '', colnames(dat)[rev_col])

    ## Manually Correct some that are not labelled correctly
    #Some dis cols have R but don't need it
    dat$dis_2 <- 10-dat$dis_2
    dat$dis_4 <- 10-dat$dis_4

    #Some dis cols dont' have an R but do need it
    dat$dis_3 <- 10-dat$dis_3
    dat$dis_6 <- 10-dat$dis_6

    #Grit recoding; apparently I had NOT labeled the reversed items in this scale with an R
    #So I will replace the items entirely with the reverse-coded items.
    dat$grt_1 <- 6-dat$grt_1
    dat$grt_3 <- 6-dat$grt_3
    dat$grt_5 <- 6-dat$grt_5
    dat$grt_6 <- 6-dat$grt_6

    #Forgot to label with R for 2018 data
    if (length(grep('18', time)) > 0) {
        dat$tfl_13 <- max(dat$tfl_13 + 1) - dat$tfl_13
    }

    ##Save the output
    write.csv(dat, file = paste("./data/reversed_data_", time, ".csv", sep = ''),
              row.names = FALSE)
}
