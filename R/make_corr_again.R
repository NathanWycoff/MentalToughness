#!/usr/bin/Rscript
#  R/make_corr_again.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.27.2018

## A second attempt at making correlation tables.

library(gsubfn)
library(xtable)
source('R/label_keys.R')
source('R/lib.R')

## Make the correlation tables.
#Read in data
datapath <- './RData/corr_data/'
files <- list.files(datapath)
outnames <- strsplit(files, '_')
file_df <- data.frame(
              name = sapply(outnames, function(x) paste(x[1:2], collapse = '_')),
              semester = sapply(outnames, function(x) paste(x[3:4], collapse = '_')),
              file = files)
file_df$name <- as.character(file_df$name)
file_df$file <- as.character(file_df$file)

## LeadChal corrs
# Collect correlation of each variable with leadChal
leadChal_corrs <- list()
for (file in file_df$file) {
    load(paste(datapath, '/', file, sep = ''))
    vars <- colnames(bayes_fit$mean)
    sem <- strapplyc(file, ".._1.", simplify = TRUE)

    # Rescale to correlations
    bayes_fit$mean <- cov2cor(bayes_fit$mean)
    bayes_fit$lb <- cov2cor(bayes_fit$lb)
    bayes_fit$ub <- cov2cor(bayes_fit$ub)

    # Get the pertinent rows
    targ_ind <- which(vars=='leadChal')
    corrs <- cbind(bayes_fit$mean[targ_ind,-targ_ind], var(X)[targ_ind, -targ_ind],
                   bayes_fit$lb[targ_ind,-targ_ind], bayes_fit$ub[targ_ind,-targ_ind])
    colnames(corrs) <- c('mean',  'freq', 'lb', 'ub')
    corrs <- apply(corrs, 2, function(i) sapply(i, function(j) num2presentable(j, 2)))

    # Store them
    it <- 0
    for (var in rownames(corrs)) {
        it <- it + 1
        if (!var %in% names(leadChal_corrs)) {
            leadChal_corrs[[var]] <- list()
        }
        leadChal_corrs[[var]][[sem]] <- 
            paste(corrs[var, 1], '/', corrs[var, 2], 
                  ' (', corrs[var, 3], ', ', corrs[var, 4], ')', sep = '')
    }
}

# Turn it into a pretty dataframe.
vars <- names(leadChal_corrs)
sems <- c('sp_17', 'fl_17', 'sp_18')
corr_list <- list()
for (sem in sems) {
    corr_list[[sem]] <- sapply(vars, function(var) {
                       if (sem %in% names(leadChal_corrs[[var]])) {
                           val <- leadChal_corrs[[var]][[sem]]
                           # Remove leading 0 to vibe with APA
                           return(gsub('0\\.', '.', val))
                       } else {
                           return('--')
                       }
              })
}
corr_df <- data.frame(corr_list)
colnames(corr_df) <- c("Semester 1", "Semester 2", "Semester 3")

# Make a nice table for a combination of variables.
make_table <- function(vars, label) {
    our_df <- corr_df[vars,]
    rownames(our_df) <- sapply(vars, function(v) id_to_pretty[[v]])
    align <- rep('l', ncol(our_df) + 1)
    print(xtable(our_df, label = paste('tab:', label, sep = ''), 
                 caption = "\\textbf{Correlation of Various Scales to Leader Toughness:} Table displays posterior mean first, classical frequentist, and a Bayesian credible interval on the next line.",
                 align = align),
          file = paste("./latex_out/bayes_tables/", label, ".tex", sep = ''))
}

#Make the desired tables
edc_vars <- c("dis", "uh", "uh.vmi", "brs", "sc.hw", "lnr")
make_table(edc_vars, 'edc_corr')

pers_vars <- c("bfi_extrav", "bfi_agree", "bfi_neur", "bfi_open", "bfi_consc", "grt", "mt", 
               "har", "ffmq", "lnr")
make_table(pers_vars, 'pers_corr')

lead_vars <- c("auth_sa", "auth_rt", "auth_bp", "auth_imp", "tfl_vis", "tfl_insp", "tfl_int", "tfl_sup",
               "tfl_pers", "ili_pro", "ili_champ", "ili_ent", "ili_emb", "lnr")
make_table(lead_vars, 'lead_corr')


## LNR corrs
# Collect correlation of each variable with leadChal
lnr_corrs <- list()
for (file in file_df$file) {
    load(paste(datapath, '/', file, sep = ''))
    vars <- colnames(bayes_fit$mean)
    sem <- strapplyc(file, ".._1.", simplify = TRUE)

    if (!'lnr' %in% vars) {
        next
    }

    # Rescale to correlations
    bayes_fit$mean <- cov2cor(bayes_fit$mean)
    bayes_fit$lb <- cov2cor(bayes_fit$lb)
    bayes_fit$ub <- cov2cor(bayes_fit$ub)

    # Get the pertinent rows
    targ_ind <- which(vars=='lnr')
    corrs <- cbind(bayes_fit$mean[targ_ind,-targ_ind], var(X)[targ_ind, -targ_ind],
                   bayes_fit$lb[targ_ind,-targ_ind], bayes_fit$ub[targ_ind,-targ_ind])
    colnames(corrs) <- c('mean',  'freq', 'lb', 'ub')
    corrs <- apply(corrs, 2, function(i) sapply(i, function(j) num2presentable(j, 2)))

    # Recover Frequentist estimates
    var(X)

    # Store them
    it <- 0
    for (var in rownames(corrs)) {
        it <- it + 1
        if (!var %in% names(lnr_corrs)) {
            lnr_corrs[[var]] <- list()
        }
        lnr_corrs[[var]][[sem]] <- 
            paste(corrs[var, 1], '/', corrs[var, 2], 
                  ' (', corrs[var, 3], ', ', corrs[var, 4], ')', sep = '')
    }
}

# Turn it into a pretty dataframe.
vars <- names(lnr_corrs)
sems <- c('sp_18')
corr_list <- list()
for (sem in sems) {
    corr_list[[sem]] <- sapply(vars, function(var) {
                       if (sem %in% names(lnr_corrs[[var]])) {
                           val <- lnr_corrs[[var]][[sem]]
                           # Remove leading 0 to vibe with APA
                           return(gsub('0\\.', '.', val))
                       } else {
                           return('--')
                       }
              })
}
corr_df <- data.frame(corr_list)
colnames(corr_df) <- c("Semester 3")

# Make a nice table for a combination of variables.
make_table <- function(vars, label) {
    our_df <- data.frame(corr_df[vars,])
    rownames(our_df) <- sapply(vars, function(v) id_to_pretty[[v]])
    colnames(our_df) <- "Semester 3"
    align <- rep('l', ncol(our_df) + 1)
    print(xtable(our_df, label = paste('tab:', label, sep = ''), 
                 caption = "\\textbf{Correlation of Various Scales to Leadership Nonresistance:} Table displays posterior mean first, classical frequentist, and a Bayesian credible interval on the next line.",
                 align = align),
          file = paste("./latex_out/bayes_tables/", label, ".tex", sep = ''))
}

#Make the desired tables
lnr_vars <- c("leadChal", "dis", "uh", "uh.vmi", "brs", "sc.hw", "ffmq", "mt", "har", "auth_sa", "auth_rt", 
              "auth_bp", "auth_imp", "tfl_vis", "tfl_insp", "tfl_int", "tfl_sup", "tfl_pers",
              "ili_pro", "ili_champ", "ili_ent", "ili_emb")
make_table(lnr_vars, 'lnr_corr')
