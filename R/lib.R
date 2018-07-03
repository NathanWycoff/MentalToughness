#!/usr/bin/Rscript
#  R/lib.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.30.2018

## Some handy functions

#' Make a decimal number in [-1,1]  into an APA compliant string
#' A number in [-1,1]
num2presentable <- function(num, round_to) {
    if (is.na(num)) {
        return('--')
    }
    if (abs(num) > 1) {
        return(round(num, round_to))
    }
    if (num >= 0)
        return(substr(paste(as.character(round(num ,round_to)), '0', sep  =''), 2, 2 + round_to))
    else {
        num <- abs(num)
        ret <- substr(paste(as.character(round(num ,round_to)), '0', sep  =''), 2, 2 + round_to)
        ret <- paste('-', ret, sep = '')
        return(ret)
    }
}

