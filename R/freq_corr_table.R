#!/usr/bin/Rscript 
#  R/freq_corr_table.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.25.2018

require(xtable)
source('R/label_keys.R')
source('R/lib.R')

## Make the correlation tables.

get_sig <- function(est, p) {
    est <- num2presentable(est, 2)
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
# See what files we have.
datapath <- './RData/bayes_output/'
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
    # Get rid of tfl_id
    good_cols <- colnames(X) != "tfl_id"
    X <- X[,good_cols]

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
        rownames(point) <- setdiff(colnames(bayes_fit$mean), 'tfl_id')

    # Make a table.
    repr <- matrix("", nrow = nrow(point), ncol = ncol(pval))
    for (i in 1:nrow(point)) {
        for (j in 1:i) {
            repr[i,j] <- get_sig(point[i,j], pval[i,j])
            repr[j,i] <- ""
        }
    }

    # Name its rows and columns nicely
    nice_names <- setdiff(colnames(bayes_fit$mean), 'tfl_id')
    nice_names <- sapply(nice_names, function(v) {
                                  v <- gsub('_scal', '', v)

                                  if (v %in% names(id_to_pretty)) {
                                      id_to_pretty[[v]]
                                  } else {
                                      v
                                  }})
    nice_names <- paste(1:nrow(repr), '. ', nice_names, sep = '')

    diag(repr) <- '--'
    rownames(repr) <- nice_names
        
    colnames(repr) <- 1:ncol(repr)

    outname <- strsplit(f, '_')[[1]]
    name <- gsub('_', '.', paste(outname[1:2], collapse = '_'))
    semester <- gsub('_', '.', paste(outname[3:4], collapse = '_'))

    align <- rep('l', ncol(repr) + 1)

    print(xtable(repr, caption = 
                 paste(name, semester, '$* p < 0.05; ** p < 0.01; *** p < 0.001$'),
             label = paste('freq_corr', name, semester, sep  ='.'),
             align = align), 
          sanitize.text.function=identity, 
          file = paste('latex_out/freq_tables/freq_', strsplit(f, '\\.')[[1]][1], '.tex', sep = ''))
}
