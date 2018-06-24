#!/usr/bin/Rscript
#  R/cor_tables.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.23.2018

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

unames <- as.character(unique(file_df$name))

#Make lead
for (nam in unames) {
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
        pvals[[fi]] <- freq_pval
        points[[fi]] <- freq_est
    }

    target <- 'leadChal'
}
