#!/usr/bin/Rscript
#  R/corr2table.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.14.2018

## Code that creates a beautiful LaTeX table from 
##      a CI and point estimate for a covariance matrix.
require(xtable)

## Test data
n <- 100
p <- 5
X <- matrix(rnorm(n*p), ncol = p)

lb <- matrix(NA, nrow = p, ncol = p)
ub <- matrix(NA, nrow = p, ncol = p)
for (i in 1:ncol(X)) {
    for (j in 1:i) {
        ret <- cor.test(X[,i], X[,j])$conf.int
        lb[i,j] <- lb[j,i] <- ret[1]
        ub[i,j] <- ub[j,i] <- ret[2]
    }
}
point <- cor(X)

colnames(point) <- paste('V', 1:p, sep = '')

#' Make a Table for a Correlation Matrix
#'
#'
#'
#' @param point A matrix giving a point estimate.
#' @param lb A matrix giving elementwise upper bounds.
#' @param ub A matrix giving elementwise lower bounds.
#' @param round_to How many digits to round to in the output.
#' @return An xtable object.
cor2table <- function(point, lb, ub, round_to = 2) {
    p <- nrow(point)

    cn <- colnames(point)

    point <- round(point, round_to)
    lb <- round(lb, round_to)
    ub <- round(ub, round_to)

    mat <- matrix('', nrow = p, ncol = p)
    for (i in 1:p) {
        for (j in 1:i) {
            mat[i,j] <- mat[j,i] <- paste('[', lb[i,j], 
                                          ', ', point[i,j], 
                                          ', ', ub[i,j], ']', sep = '')
        }
    }

    colnames(mat) <- rownames(mat) <- cn
    return(xtable(mat))
}
