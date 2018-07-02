#!/usr/bin/Rscript
#  R/make_alpha_basic.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 07.02.2018

# Make a table with both ALPHA and mean/sd by combining two existing scripts.
# As a result, code is significantly more convoluted than necessary. Sorry

######### Make a table reporting mean and standard deviation 

#### Copy some code from R/make_alpha_tables.R to get variable names that we actually ended up using.
source('R/label_keys.R')
source('R/lib.R')
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
# Parcels are identified as having two underscores (or three, in the case of uh) in their names. 
# We wish to remove these from the table.
all_table$scale <- as.character(all_table$scale) 
not_parcels <- sapply(strsplit(all_table$scale, '_'), length) == 2
all_table <- all_table[not_parcels,]
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

# Round etc
all_table <- apply(all_table, 2, function(i) 
                   sapply(i, function(j) num2presentable(j, 2)))


##### Collect alphas for each scale.
path <- "./RData/alphas/"
files <- c("alphas_sp_17.RData", "alphas_fl_17.RData", "alphas_sp_18.RData")
files <- paste(path, files, sep = '')

tab <- lapply(files, function(file) {
    load(file)
    return(atable)
})

alpha_table <- Reduce(function(i, j) merge(i, j, by = 'scale', all = TRUE), tab)

# Parcels are identified as having two underscores (or three, in the case of uh) in their names. 
# We wish to remove these from the table.
alpha_table$scale <- as.character(alpha_table$scale) 
not_parcels <- sapply(strsplit(alpha_table$scale, '_'), length) == 2
alpha_table <- alpha_table[not_parcels,]

# Make row names nice.
rownames(alpha_table) <- sapply(alpha_table$scale, function(v) {
                              v <- gsub('_scal', '', v)

                              if (v %in% names(id_to_pretty)) {
                                  id_to_pretty[[v]]
                              } else {
                                  v
                              }})
alpha_table$scale <- NULL
colnames(alpha_table) <- c("Semester 1", "Semester 2", "Semester 2")

# Prettify output
alpha_table <- apply(alpha_table, 2, function(i) sapply(i, function(j) ifelse(is.na(j), '--', round(as.numeric(j), 2))) )

### Merge the two in a gross way
all_table <- all_table[order(rownames(all_table)),]
alpha_table <- alpha_table[order(rownames(alpha_table)),]
both_table <- lapply(1:3, function(i) {
                     ret <- cbind(alpha_table[,i], all_table[,(2*(i)-1):(2*i)])
                     colnames(ret) <- paste(c('Mean', 'SD', 'Alpha'), ' S', i, sep='') 
                     return(ret)
                              })
both_table <- Reduce(cbind, both_table)
align <- 'llll|lll|lll'

print(xtable(both_table, label = "tab:summarystats", 
             caption = "Means and Standard Deviation by Scale for Each Semester",
             align = align),
      file = "./latex_out/summary_table.tex")
