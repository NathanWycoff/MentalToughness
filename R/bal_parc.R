#!/usr/bin/Rscript
#  bal_parc.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.19.2018

#' Create balanced parcels
#' 
#' @param cors The correlations of each item with a scale.
#' @param parcels The number of parcels to create
#' @return An integer vector of the same length as cors, giving assignemnts.
bal_parc <- function(cors, parcels = 3) {

    ranks <- rank(cors)
    taken <- c()
    asgn <- rep(NA, length(cors))

    max_filter <- rep(1, length(ranks))
    min_filter <- rep(1, length(ranks))

    for (r in 1:(min(floor(length(ranks)/2), parcels))) {
        max_filter[!is.na(asgn)] <- -Inf
        min_filter[!is.na(asgn)] <- Inf

        ma <- which.max(ranks * max_filter)
        mi <- which.min(ranks * min_filter)

        asgn[ma] <- r
        asgn[mi] <- r
    }

    if (length(cors) > 2*parcels) {
        base <- c(parcels:1, 1:parcels)
        for (i in (2*parcels+1):length(cors)) {
            max_filter <- rep(1, length(ranks))
            max_filter[!is.na(asgn)] <- -Inf
            cur <- which.max(ranks * max_filter)
            asgn[cur] <- base[(i-(2*parcels+1)) %% parcels + 1]
        }
    }

    return(asgn)
}

## Test
#require(psych)
#file <- "./data/reversed_data_fl_17.csv"
#dat <- read.csv(file)
#lc_cols <- grep('leadChal_', colnames(dat))
#leadChal_scal <- dat[,lc_cols]
#fit <- alpha(leadChal_scal)
#cors <- fit$item.stats$r.cor
#asgn <- bal_parc(cors)
