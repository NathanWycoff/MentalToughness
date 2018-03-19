#!/usr/bin/Rscript
#  bal_parc.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.19.2018

#' Create balanced parcels
#' 
#' Create balanced parcels (averages over several survey items which purport to measure the same thing) by pairing the item with the most correlation to the overall scale with the item with the least correlation, the item with the second most correlation with the item with the second least, and so on.
#' @param cors The correlations of each item with a scale.
#' @return A list of length [ceil(length(cors)/2)] with each item a numeric vector, representing the indices of each paried items.
bal_parc <- function(cors) {
    ranks <- rank(cors)
    taken <- c()
    it <- list()
    for (r in 1:(floor(length(ranks)/2))) {
        max_filter <- rep(1, length(ranks))
        max_filter[taken] <- -Inf
        min_filter <- rep(1, length(ranks))
        min_filter[taken] <- Inf

        ma <- which.max(ranks * max_filter)
        mi <- which.min(ranks * min_filter)

        it[[r]] <- c(ma, mi)
        taken <- c(taken, ma, mi)
    }
    #Add on a straggler if there's an odd number of items
    if (length(ranks) %% 2 == 1) {
        it[[length(it)+1]] <- ranks[ceiling(length(ranks)/2)]
    }

    return(it)
}
