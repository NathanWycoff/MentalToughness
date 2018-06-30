#!/usr/bin/Rscript
#  R/scale_reverse.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.11.2018

## Some scales are reversed in this dataset, meaning that a high reading
## on the scale indicates a low measurement of the attribute of interest.
## This script reverses such scales. After the data has been processed
## through this script, high readings on a scale always indicate high measurements.

reverser_2017 <- function(scal, val) {
    if (strsplit(scal, '_')[[1]][1] == 'bfi') {
        9 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'ffmq') {
        5 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'grt') {
        7 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'sc.hw') {
        9 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'sc.now') {
        9 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'dis') {
        11 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'tfl') {
        9 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'brs') {
        9 - val
    } else {
        stop(paste("Unrecognized Reverse Value:", scal))
    }
}

reverser_2018 <- function(scal, val) {
    if (strsplit(scal, '_')[[1]][1] == 'bfi') {
        9 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'ffmq') {
        5 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'grt') {
        7 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'sc.hw') {
        9 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'dis') {
        7 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'tfl') {
        9 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'brs') {
        9 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'lnr') {
        7 - val
    } else if (strsplit(scal, '_')[[1]][1] == 'har') {
        6 - val
    } else {
        stop(paste("Unrecognized Reverse Value:", scal))
    }
}

files <- c("./data/fl_16.csv", "./data/fl_17.csv", "./data/sp_17.csv", 
           "./data/sp_18.csv")
for (file in files) {

    dat <- read.csv(file)
    time <- strsplit(strsplit(file, '/')[[1]][3], '\\.')[[1]][1]

    # Remove some things that aren't going to be used
    rem <- grep(('HVS|EI|SM'), colnames(dat))
    if (length(rem) > 0) {
        dat <- dat[, -rem]
    }


    ## Manually Correct some that are not labelled correctly (all years EXCEPT 2016)
    #Code below will need to vary based on semester; SP17 and FL17 used same # of response categories, but SP18 was different in some instances.
    #Some dis cols have R but don't need it
    if (length(grep('17', time)) > 0) {  
        # Reverse marked scales
        rev_col <- grep('R$', colnames(dat))
        for (col in rev_col) {
            dat[,col] <- sapply(dat[,col], function(i) reverser_2017(colnames(dat)[col], i))
        }
        #Change the column name to get rid of the R
        colnames(dat)[rev_col] <- gsub('R$', '', colnames(dat)[rev_col])

        dat$dis_2 <- 11-dat$dis_2 #shouldn't the dis_2 on the right side be dis_2R?  Aren't you recoding it to say that it isn't reversed?
        dat$dis_4 <- 11-dat$dis_4 #same as above

        #Some dis cols dont' have an R but do need it
        dat$dis_3 <- 11-dat$dis_3
        dat$dis_6 <- 11-dat$dis_6
    }

   ##DIS recoding: ONLY SP18
    if (length(grep('18', time)) > 0) {  
        # Reverse marked scales
        rev_col <- grep('R$', colnames(dat))
        for (col in rev_col) {
            dat[,col] <- sapply(dat[,col], function(i) reverser_2018(colnames(dat)[col], i))
        }
        #Change the column name to get rid of the R
        colnames(dat)[rev_col] <- gsub('R$', '', colnames(dat)[rev_col])

         #Same as above, but should be 6-... since it was a 5-point scale in SP18.
        dat$dis_2 <- 7-dat$dis_2 #shouldn't the dis_2 on the right side be dis_2R?  Aren't you recoding it to say that it isn't reversed?
        dat$dis_4 <- 7-dat$dis_4 #same as above

        #Some dis cols dont' have an R but do need it
        dat$dis_3 <- 7-dat$dis_3
        dat$dis_6 <- 7-dat$dis_6

        # Correct BRS issue 2, 4, and 6; 7-point scale; this works for all 3 semesters.
        dat$brs_2 <- 9 - dat$brs_2
        dat$brs_4 <- 9 - dat$brs_4
        dat$brs_6 <- 9 - dat$brs_6

        #Grit recoding; apparently I had NOT labeled the reversed items in this scale with an R
        #So I will replace the items entirely with the reverse-coded items.  This works for all 3 semesters.
        dat$grt_1 <- 7-dat$grt_1
        dat$grt_3 <- 7-dat$grt_3
        dat$grt_5 <- 7-dat$grt_5
        dat$grt_6 <- 7-dat$grt_6

        #dat$tfl_13 <- max(dat$tfl_13 + 1) - dat$tfl_13
        dat$tfl_13 <- 9 - dat$tfl_13 #I don't trust the max calc for this, so I did it manually
    }

    ##Save the output
    write.csv(dat, file = paste("./data/reversed_data_", time, ".csv", sep = ''),
              row.names = FALSE)
}
