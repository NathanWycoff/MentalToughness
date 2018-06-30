#!/usr/bin/Rscript
#  R/basic_summary_table.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.29.2018

## Make a table reporting mean and standard deviation 

#### Copy some code from R/make_alpha_tables.R to get variable names that we actually ended up using.
source('R/label_keys.R')
require(xtable)

## Collect alphas for each scale.
path <- "./RData/alphas/"
files <- c("alphas_fl_17.RData", "alphas_sp_17.RData", "alphas_sp_18.RData")
files <- paste(path, files, sep = '')

tab <- lapply(files, function(file) {
    load(file)
    return(atable)
})

all_table <- Reduce(function(i, j) merge(i, j, by = 'scale', all = TRUE), tab)
#### End copied code

root <- './data/proc_met_'
branch <- '.csv'
sem <- c('sp_17', 'fl_17', 'sp_18')
files <- paste(root, sem, branch, sep = '')

var_names <- gsub('_scal', '', as.character(all_table$scale))
table <- c()
res <- lapply(files, function(file) {
    df <- read.csv(file)

    vars <- intersect(var_names, colnames(df))
    good_vars <- df[,vars]
    cm <- apply(good_vars, 2, mean)
    cs <- apply(good_vars, 2, sd)
    df <- as.data.frame(cbind(cm, cs))
    df$scale <- vars
    return(df)
})

all_table <- Reduce(function(i, j) merge(i, j, by = 'scale', all = TRUE), res)
# Make row names nice.
rownames(all_table) <- sapply(all_table$scale, function(v) {
                              v <- gsub('_scal', '', v)

                              if (v %in% names(id_to_pretty)) {
                                  id_to_pretty[[v]]
                              } else {
                                  v
                              }})
all_table$scale <- NULL
colnames(all_table) <- c("Mean S1", "SD S1", "Mean S2", "SD S2", "Mean S3", "SD S3")

print(xtable(all_table, label = "tab:summarystats", 
             caption = "Means and Standard Deviation by Scale for Each Semester"),
      file = "./latex_out/summary_table.tex")
