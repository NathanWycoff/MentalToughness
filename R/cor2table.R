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

#' Make a Table for a Correlation/Covariance Matrix
#'
#' Makes a table of covariances measured for certain variables over different groups between one variable in particular and all the others, one row per variable, one column per group, displaying both point and interval estimatis for correlations/covariances.
#'
#' @param means A list of matrices giving point estimates.
#' @param lbs A list of matrices giving elementwise upper bounds.
#' @param ubs A list of matrices giving elementwise lower bounds.
#' @param points A list of matrices giving elementwise frequentist estimates.
#' @param pvals A list of matrices giving elementwise frequentist p values.
#' @param title The name of each group.
#' @param target The index of the variable to compare to.
#' @param vars The names of each variable.
#' @param round_to How many digits to round to in the output.
#' @return An xtable object.
cor2table <- function(label, means, lbs, ubs, points, pvals, titles, target, 
                      vars, round_to = 2) {
    means <- lapply(means, function(mean) round(cov2cor(mean), round_to))
    lbs <- lapply(lbs, function(lb) round(cov2cor(lb), round_to))
    ubs <- lapply(ubs, function(ub) round(cov2cor(ub), round_to))
    points <- lapply(points, function(point) round(point, round_to))
    pvals <- lapply(pvals, function(pval) round(pval, round_to))

    S <- length(means)
    sems <- lapply(1:S, function(s) {
            sapply(vars[vars!=target], function(i) {
                       if (i %in% colnames(means[[s]])) {
                           paste(means[[s]][target, i],
                                 ' (', points[[s]][target,i], ')\n',
                                 '[', lbs[[s]][target, i],
                                 ', ',
                                ubs[[s]][target, i], ']',
                                sep = '')
                       } else {
                           '--'
                       }
            })
    })

    uvars <- setdiff(vars, target)
    disp_df <- matrix(NA, nrow = 2*length(uvars), ncol = length(titles))
    for (i in 1:nrow(disp_df)) {
        for (j in 1:ncol(disp_df)) {
            ci <- ceiling(i / 2)
            if (i %% 2 == 1) {
                disp_df[i,j] <- strsplit(sems[[j]][uvars[ci]], '\n')[[1]][1]
            } else {
                disp_df[i,j] <- strsplit(sems[[j]][uvars[ci]], '\n')[[1]][2]
            }
        }
    }

    colnames(disp_df) <- titles
    rownames(disp_df) <- sapply(1:(2*length(uvars)), function(i) {
                                    if (i %% 2 == 1) {
                                        uvars[ceiling(i/2)]
                                    } else {
                                        paste(rep('', ceiling(i/2)), collapse = '  ')
                                    }})

    return(xtable(disp_df, label = label, caption = 
                  paste(c(gsub('_', ' ', label), "Table displays posterior mean first, classical frequentist estimate in paranthesis, and a Bayesian credible interval on the next line"), collapse = ': ')))
}
