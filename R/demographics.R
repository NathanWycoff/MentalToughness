#!/usr/bin/Rscript
#  R/demographics.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.29.2018

## Record basic demographic info.

files <- c("./data/sp_17.csv", "./data/fl_17.csv",
           "./data/sp_18.csv")

# Sample sizes
ns <- sapply(files, function(file) {
       nrow(read.csv(file))
           })

# Genders 
genders <- sapply(files, function(file) {
       dat <- read.csv(file)
       ret <- gsub('1', 'M', as.character(dat$gender))
       table(gsub('2', 'F', ret))
           })

# grad year
gradYrs <- sapply(files, function(file) {
                  dat <- read.csv(file)
                  unname(sapply(as.character(dat$gradYr), function(year) {
                                year <- gsub('!', '', year)
                                year <- gsub('18\\+3', '2021', year)
                                if (nchar(year) == 2) {
                                    year <- paste('20', year, sep = '')
                                } 
                                if (!year %in% c("2016", "2017", "2018", "2019", "2020", "2021")) {
                                    print(paste("Removing this response:", year))
                                    return(NA)
                                } else {
                                    return(year)
                                }
}))
           })
lapply(gradYrs, table)

#### Formal Experience
formalExps <- sapply(files, function(file) {
       dat <- read.csv(file)
       dat$formalExp
           })
# Subtract from last year
formalExps[[3]] <- formalExps[[3]] - 1

df <- data.frame(Mean = sapply(formalExps, mean), SD = sapply(formalExps, sd))
rownames(df) <- c("Semester 1", "Semester 2", "Semester 3")
print(df)
lapply(formalExps, table)



#### Informal Experience
informalExps <- sapply(files, function(file) {
       dat <- read.csv(file)
       dat$informalExp
           })
# Subtract from last year
informalExps[[3]] <- informalExps[[3]] - 1

df <- data.frame(Mean = sapply(informalExps, mean), SD = sapply(informalExps, sd))
rownames(df) <- c("Semester 1", "Semester 2", "Semester 3")
print(df)
lapply(informalExps, table)



#### Chalfreq 
chalFreq <- sapply(files, function(file) {
       dat <- read.csv(file)
       dat$chalFreq
           })
# Subtract from last year
chalFreq[[3]] <- chalFreq[[3]] - 1

df <- data.frame(Mean = sapply(chalFreq, mean), SD = sapply(chalFreq, sd))
rownames(df) <- c("Semester 1", "Semester 2", "Semester 3")
print(df)
lapply(chalFreq, table)
