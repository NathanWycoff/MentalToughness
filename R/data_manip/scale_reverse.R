#!/usr/bin/Rscript
#  R/scale_reverse.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.11.2018

## Some scales are reversed in this dataset, meaning that a high reading
## on the scale indicates a low measurement of the attribute of interest.
## This script reverses such scales. After the data has been processed
## through this script, high readings on a scale always indicate high measurements.

reverser_2017 <- function(scal, val) {
    if (strsplit(scal, '_')[[1]][1] == 'bfi') {
        8 - val #1-7 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'ffmq') {
        4 - val #since scale is 0-4, starting with 0, need to subtract from 4.
    } else if (strsplit(scal, '_')[[1]][1] == 'grt') {
        6 - val #1-5 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'sc.hw') {
        8 - val #since scale is 0-8, starting with 0, need to subtract from 8.
    } else if (strsplit(scal, '_')[[1]][1] == 'sc.now') { ##You don't need to include sc.now data in any results!
        9 - val #can remove this - not using this variable
    } else if (strsplit(scal, '_')[[1]][1] == 'dis') {
        10 - val #1-9 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'tfl') {
        8 - val #1-7 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'brs') {
        8 - val #1-7 scale
    } else {
        stop(paste("Unrecognized Reverse Value:", scal))
    }
}

reverser_2018 <- function(scal, val) {
    if (strsplit(scal, '_')[[1]][1] == 'bfi') {
        8 - val #1-7 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'ffmq') {
        6 - val #1-5 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'grt') {
        6 - val #1-5 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'sc.hw') {
        6 - val #1-5 scale in SP18
    } else if (strsplit(scal, '_')[[1]][1] == 'dis') {
        6 - val #1-5 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'tfl') {
        8 - val #1-7 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'brs') {
        8 - val #1-7 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'lnr') {
        6 - val #1-5 scale
    } else if (strsplit(scal, '_')[[1]][1] == 'har') {
        5 - val #1-4 scale
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
  
        dat$dis_2 <- 10-dat$dis_2 
        dat$dis_4 <- 10-dat$dis_4 

        #Some dis cols dont' have an R but do need it
        dat$dis_3 <- 10-dat$dis_3
        dat$dis_6 <- 10-dat$dis_6
        #dis_1R will be properly reverse coded automatically because it did have the R in the name, right?
    }

   ##DIS & TFL recoding: ONLY SP18
    if (length(grep('18', time)) > 0) {  
        # Reverse marked scales
        rev_col <- grep('R$', colnames(dat))
        for (col in rev_col) {
            dat[,col] <- sapply(dat[,col], function(i) reverser_2018(colnames(dat)[col], i))
        }
        #Change the column name to get rid of the R
        colnames(dat)[rev_col] <- gsub('R$', '', colnames(dat)[rev_col])

         #Same as above, but should be 6-... since it was a 5-point scale in SP18.
        dat$dis_2 <- 6-dat$dis_2 
        dat$dis_4 <- 6-dat$dis_4 

       #**You had this in the code block below, but I think it belongs here.
        dat$dis_3 <- 6-dat$dis_3 #1-5 scale
        dat$dis_6 <- 6-dat$dis_6
                                
      #I believe this tfl code is only needed for the SP18 dataset - the other datasets had the proper labeling for this variable.
      #I (MJ) moved this up to this code block.
        #dat$tfl_13 <- max(dat$tfl_13 + 1) - dat$tfl_13
        dat$tfl_13 <- 8 - dat$tfl_13 #1-7 scale
    }

    # Do the following whenever the year is NOT 16
    if (length(grep('16', time)) == 0) {

        # Correct BRS issue 2, 4, and 6; 7-point scale; this works for all 3 semesters.
        dat$brs_2 <- 8 - dat$brs_2 #1-7 scale
        dat$brs_4 <- 8 - dat$brs_4
        dat$brs_6 <- 8 - dat$brs_6

        #Grit recoding; apparently I had NOT labeled the reversed items in this scale with an R
        #So I will replace the items entirely with the reverse-coded items.  This works for all 3 semesters.
        dat$grt_1 <- 6-dat$grt_1 #1-5 scale
        dat$grt_3 <- 6-dat$grt_3
        dat$grt_5 <- 6-dat$grt_5
        dat$grt_6 <- 6-dat$grt_6

        
    }

    ##Save the output
    write.csv(dat, file = paste("./data/reversed_data_", time, ".csv", sep = ''),
              row.names = FALSE)
}
