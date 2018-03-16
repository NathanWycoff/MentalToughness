#!/usr/bin/Rscript
#  stat_parcelling.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.16.2018

## Do parcelling techniques where a prior parcells are not available.

files <- c("./data/reversed_data_fl_17.csv", "./data/reversed_data_sp_17.csv", "./data/reversed_data_sp_18.csv")

file <- files[1]
dat <- read.csv(file)

leadChal_scal <- dat[,grep('leadChal_\\d+', colnames(dat))]

#' Do balanced Parcelling
#'
#' 
bal_parcel <- function(target) {
    fit <- factanal(target, 1)
    load <- as.numeric(fit$loadings)
}
