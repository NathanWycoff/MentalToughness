#!/usr/bin/Rscript
#  R/make_corr_tables.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.21.2018

## Make a nice table from our correlations data.
source('R/cor2table.R')

#TODO: One for each semester.

## Make the correlation tables.
#Read in data
datapath <- './RData'
files <- list.files(datapath)
outnames <- strsplit(files, '_')
file_df <- data.frame(
              name = sapply(outnames, function(x) paste(x[1:2], collapse = '_')),
              semester = sapply(outnames, function(x) paste(x[3:4], collapse = '_')),
              file = files)
file_df$name <- as.character(file_df$name)

#Make each table.
unames <- as.character(unique(file_df$name))
# This is a gross way of doing it, but it doesn't matter.
#' @param n The name of the file
#' @param titles The name of each group to be displayed.
#' @param target The variable with which to display correlations, use its display name
make_table <- function(n, titles, target) {
    means <- lapply(file_df$file[file_df$name==n], function(f) {
                        load(paste(datapath, f, sep = '/'))
                        bayes_fit$mean
              })
    lbs <- lapply(file_df$file[file_df$name==n], function(f) {
                        load(paste(datapath, f, sep = '/'))
                        bayes_fit$lb
              })
    ubs <- lapply(file_df$file[file_df$name==n], function(f) {
                        load(paste(datapath, f, sep = '/'))
                        bayes_fit$ub
              })
    vars <- Reduce(union, lapply(file_df$file[file_df$name==n], function(f) {
                        load(paste(datapath, f, sep = '/'))
                        colnames(bayes_fit$ub)
              }))

    pvals <- list()
    points <- list()
    fi <- 0
    for (f in file_df$file[file_df$name==n]) {
        fi <- fi + 1
        load(paste(datapath, f, sep = '/'))
        p <- ncol(X)
        freq_pval <- matrix(NA, nrow = p, ncol = p)
        freq_est <- matrix(NA, nrow = p, ncol = p)
        for (i in 1:ncol(X)) {
            for (j in 1:i) {
                ret <- cor.test(X[,i], X[,j])
                freq_pval[i,j] <- freq_pval[j,i] <- ret$p.val
                freq_est[i,j] <- freq_est[j,i] <- ret$estimate
            }
        }
        colnames(freq_pval) <- rownames(freq_pval) <- colnames(freq_est) <- 
            rownames(freq_est) <- colnames(bayes_fit$mean)
        pvals[[fi]] <- freq_pval
        points[[fi]] <- freq_est
    }

    vars <- colnames(X)
    cor2table(n, means, lbs, ubs, points, pvals, titles, target, vars)
}

# Make tables for all 3 semesters vs leadChal
for (na in unames) {
    print(make_table(na, titles = c('Semester1', 'Semester2', 'Semester3'), 'leadChal'), 
          file = paste('latex_out/', na, '.tex', sep = ''))
}

# Make tables for all 3 semesters vs leadChal
for (na in unames) {
    print(make_table(na, titles = c('Semester1', 'Semester2', 'Semester3'), 'leadChal')) 
}
