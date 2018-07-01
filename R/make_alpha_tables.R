#!/usr/bin/Rscript
#  R/make_alpha_tables.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.28.2018

source('R/label_keys.R')
require(xtable)

## Collect alphas for each scale.
path <- "./RData/alphas/"
files <- c("alphas_sp_17.RData", "alphas_fl_17.RData", "alphas_sp_18.RData")
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

# Make row names nice.
rownames(all_table) <- sapply(all_table$scale, function(v) {
                              v <- gsub('_scal', '', v)

                              if (v %in% names(id_to_pretty)) {
                                  id_to_pretty[[v]]
                              } else {
                                  v
                              }})
all_table$scale <- NULL
colnames(all_table) <- c("Semester 1", "Semester 2", "Semester 2")

# Prettify output
all_table <- apply(all_table, 2, function(i) sapply(i, function(j) ifelse(is.na(j), '--', round(as.numeric(j), 2))) )

align <- rep('l', ncol(all_table)+1)
print(xtable(all_table, label = "tab:alphas", 
             caption = "The alphas for all scales for each semester.",
             align = align),
      file = "./latex_out/alpha_table.tex")
