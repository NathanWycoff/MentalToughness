#!/usr/bin/Rscript
#  R/freq_corr_table.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.25.2018

require(xtable)

## Make the correlation tables.

get_sig <- function(est, p) {
    est <- round(est, 2)
    if (p < 0.001) {
        return(paste('$', est, '^{***}', '$', sep = ''))
    } 
    if (p < 0.1) {
        return(paste('$', est, '^{**}', '$', sep = ''))
    }
    if (p < 0.05) {
        return(paste('$', est, '^*', '$', sep = ''))
    }
    return(paste('$', est, '$', sep = ''))
}
#Read in data
datapath <- './RData'
files <- list.files(datapath)
outnames <- strsplit(files, '_')
file_df <- data.frame(
              name = sapply(outnames, function(x) paste(x[1:2], collapse = '_')),
              semester = sapply(outnames, function(x) paste(x[3:4], collapse = '_')),
              file = files)
file_df$name <- as.character(file_df$name)

pvals <- list()
points <- list()
fi <- 0
for (f in file_df$file) {
    fi <- fi + 1
    load(paste(datapath, f, sep = '/'))
    p <- ncol(X)
    pval <- matrix(NA, nrow = p, ncol = p)
    point <- matrix(NA, nrow = p, ncol = p)
    for (i in 1:ncol(X)) {
        for (j in 1:i) {
            ret <- cor.test(X[,i], X[,j])
            pval[i,j] <- pval[j,i] <- ret$p.val
            point[i,j] <- point[j,i] <- ret$estimate
        }
    }
    colnames(pval) <- rownames(pval) <- colnames(point) <- 
        rownames(point) <- colnames(bayes_fit$mean)

    # Make a table.
    repr <- matrix("", nrow = nrow(point), ncol = ncol(pval))
    for (i in 1:nrow(point)) {
        for (j in 1:i) {
            repr[i,j] <- get_sig(point[i,j], pval[i,j])
            repr[j,i] <- "--"
        }
    }
    colnames(repr) <- rownames(repr) <- 
        gsub('_', '.', colnames(bayes_fit$mean))

    outname <- strsplit(f, '_')[[1]]
    name <- gsub('_', '.', paste(outname[1:2], collapse = '_'))
    semester <- gsub('_', '.', paste(outname[3:4], collapse = '_'))

    print(xtable(repr, caption = 
                 paste(name, semester, '$* p < 0.05; ** p < 0.01; *** p < 0.001$'),
             label = paste('freq_corr', name, semester, sep  ='.')), 
          sanitize.text.function=identity, 
          file = paste('latex_out/freq_tables/freq_', strsplit(f, '\\.')[[1]][1], '.tex', sep = ''))
}
