#!/usr/bin/Rscript
#  R/scale_reverse.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.11.2018

## Some scales are reversed in this dataset, meaning that a high reading
## on the scale indicates a low measurement of the attribute of interest.
## This script reverses such scales. After the data has been processed
## through this script, high readings on a scale always indicate high measurements.

files <- c("./data/fl_16.csv", "./data/fl_17.csv", "./data/sp_17.csv", 
           "./data/sp_18.csv")
for (file in files) {
    dat <- read.csv(file)
    time <- strsplit(strsplit(file, '/')[[1]][3], '\\.')[[1]][1]

    ## Most reversed scales are labelled, this code block will take care of those.
    #Create Reversed Scales
      #####****** I'm concerned that this general command may not always work.  If it's a 9-point scale, for example, 
           #but the highest response anyone uses is 7, then wouldn't this command treat 7 as the possible max, which would result in
           #an incorrect reverse coding?  It seems to me that we need to specify what number should be subtracted from based on what
           #we know of the possible scale response range.  We should at the very least look at what the max is for all the scales and
           #compare this with the data explanation document to make sure they match up. 
    rev_col <- grep('R$', colnames(dat))
    dat[,rev_col] <- apply(dat[,rev_col], 2, function(i) max(i, na.rm = TRUE) - i)
    #Change the column name to get rid of the R
    colnames(dat)[rev_col] <- gsub('R$', '', colnames(dat)[rev_col])

    ## Manually Correct some that are not labelled correctly (all years EXCEPT 2016)
    if (!length(grep('16', time)) > 0) {  
        #Code below will need to vary based on semester; SP17 and FL17 used same # of response categories, but SP18 was different in some instances.
        #Some dis cols have R but don't need it
        dat$dis_2 <- 10-dat$dis_2 #shouldn't the dis_2 on the right side be dis_2R?  Aren't you recoding it to say that it isn't reversed?
        dat$dis_4 <- 10-dat$dis_4 #same as above

        #Some dis cols dont' have an R but do need it
        dat$dis_3 <- 10-dat$dis_3
        dat$dis_6 <- 10-dat$dis_6
               
       ##DIS recoding: ONLY SP18
         #Same as above, but should be 6-... since it was a 5-point scale in SP18.
        dat$dis_2 <- 6-dat$dis_2 #shouldn't the dis_2 on the right side be dis_2R?  Aren't you recoding it to say that it isn't reversed?
        dat$dis_4 <- 6-dat$dis_4 #same as above

        #Some dis cols dont' have an R but do need it
        dat$dis_3 <- 6-dat$dis_3
        dat$dis_6 <- 6-dat$dis_6

        # Correct BRS issue 2, 4, and 6; 7-point scale; this works for all 3 semesters.
        dat$brs_2 <- 8 - dat$brs_2
        dat$brs_4 <- 8 - dat$brs_4
        dat$brs_6 <- 8 - dat$brs_6

        #Grit recoding; apparently I had NOT labeled the reversed items in this scale with an R
        #So I will replace the items entirely with the reverse-coded items.  This works for all 3 semesters.
        dat$grt_1 <- 6-dat$grt_1
        dat$grt_3 <- 6-dat$grt_3
        dat$grt_5 <- 6-dat$grt_5
        dat$grt_6 <- 6-dat$grt_6
    }


    #Forgot to label with R for 2018 data ONLY 2018
    if (length(grep('18', time)) > 0) {
        #dat$tfl_13 <- max(dat$tfl_13 + 1) - dat$tfl_13
        dat$tfl_13 <- 8 - dat$tfl_13 #I don't trust the max calc for this, so I did it manually
        
    }

    ##Save the output
    write.csv(dat, file = paste("./data/reversed_data_", time, ".csv", sep = ''),
              row.names = FALSE)
}
