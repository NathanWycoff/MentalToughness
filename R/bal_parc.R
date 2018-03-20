#!/usr/bin/Rscript
#  bal_parc.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.19.2018

#' Create balanced parcels
#' 
#' @param cors The correlations of each item with a scale.
#' @return A list of length [ceil(length(cors)/2)] with each item a numeric vector, representing the indices of each paried items.
bal_parc <- function(cors) {

    ranks <- rank(cors)
    taken <- c()
    it <- list()
    asgn <- rep(NA, length(cors))

    max_filter <- rep(1, length(ranks))
    min_filter <- rep(1, length(ranks))

    for (r in 1:(min(floor(length(ranks)/2), 3))) {
        max_filter[!is.na(asgn)] <- -Inf
        min_filter[!is.na(asgn)] <- Inf

        ma <- which.max(ranks * max_filter)
        mi <- which.min(ranks * min_filter)

        asgn[ma] <- r
        asgn[mi] <- r
    }

    if (length(cors) > 6) {
        base <- c(3:1, 1:3)
        for (i in 7:length(cors)) {
            max_filter <- rep(1, length(ranks))
            max_filter[!is.na(asgn)] <- -Inf
            cur <- which.max(ranks * max_filter)

            print(cur)

            asgn[cur] <- base[(i-7) %% 3 + 1]
        }
    }

    return(it)
}
