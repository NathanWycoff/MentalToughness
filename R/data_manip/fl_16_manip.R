#!/usr/bin/Rscript
#  R/data_manip/fl_16_manip.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 06.12.2018

## Fall 2016 is different enough to merit its own script.
library(psych)
library(dplyr)
source('./R/bal_parc.R')

file <- "./data/reversed_data_fl_16.csv"
dat <- read.csv(file)

time <- strsplit(strsplit(strsplit(file, '/')[[1]][3], '\\.')[[1]][1], 'reversed_data_')[[1]][2]

ffmq_ob_scal <- dat[,grep('ffmq_(1|6|11)$', colnames(dat))]#Observing
ffmq_de_scal <- dat[,grep('ffmq_(2|7|12)$', colnames(dat))]#Describing
ffmq_aa_scal <- dat[,grep('ffmq_(3|8|13)$', colnames(dat))]#Acting with Awareness
ffmq_nj_scal <- dat[,grep('ffmq_(4|9|14)$', colnames(dat))]#Nonjuding
ffmq_nr_scal <- dat[,grep('ffmq_(5|10|15)$', colnames(dat))]#Nonreactivity

## Unconditional Happiness Subscale
uh_hi_scal <- dat[,grep('uh_(1|2|3|6|7|8|9|11|14|17|20)$', colnames(dat))]#Low Arousal Situation
uh_lo_scal <- dat[,grep('uh_(4|5|10|12|13|15|16|18|19)$', colnames(dat))]#High Arousal Situation
# Create two parcels for each scale using balanced parcelling
#Get correlations, and assignments
hi_cors <- psych::alpha(uh_hi_scal[complete.cases(uh_hi_scal),])$item.stats$r.cor
hi_asgn <- bal_parc(hi_cors, parcels = 2)
lo_cors <- psych::alpha(uh_lo_scal[complete.cases(uh_hi_scal),])$item.stats$r.cor
lo_asgn <- bal_parc(lo_cors, parcels = 2)
#Subset
uh_hi_p1_scal <- uh_hi_scal[,hi_asgn==1]
uh_hi_p2_scal <- uh_hi_scal[,hi_asgn==2]
uh_lo_p1_scal <- uh_lo_scal[,lo_asgn==1]
uh_lo_p2_scal <- uh_lo_scal[,lo_asgn==2]

##Identity Leadership
ili_pro_scal <- dat[,grep('ili_(1|5|9|13)$', colnames(dat))]#Perseverance
ili_champ_scal <- dat[,grep('ili_(2|6|10|14)$', colnames(dat))]#Perseverance
ili_ent_scal <- dat[,grep('ili_(3|7|11|15)$', colnames(dat))]#Perseverance
ili_emb_scal <- dat[,grep('ili_(4|8|12)$', colnames(dat))]#Perseverance

##Transformational Leadership Subscales
tfl_vis_scal <- dat[,grep('tfl_(1|7|13)$', colnames(dat))]#Perseverance
tfl_insp_scal <- dat[,grep('tfl_(2|8|14)$', colnames(dat))]#Inspirational Motivation
tfl_int_scal <- dat[,grep('tfl_(3|9|15)$', colnames(dat))]#Inspirational Motivation
tfl_sup_scal <- dat[,grep('tfl_(4|10|16)$', colnames(dat))]#Supportive Leadership
tfl_pers_scal <- dat[,grep('tfl_(5|11|17)$', colnames(dat))]#Personal Recognition
tfl_id_scal <- dat[,grep('tfl_(6|12|18)$', colnames(dat))]#Personal Recognition

#Record all of the names for scales.
scales <- ls()[grep('scal', ls())]

# Calculate alpha for each scale, separately for self and other reported.
self_rows <- dat$surVer == 1 & !is.na(dat$surVer)
other_rows <- dat$surVer == 2 & !is.na(dat$surVer)
for (scale in scales) {
    capture.output(psych::alpha(get(scale)[self_rows,], check.keys = TRUE),
        file = paste('./output/alphas/self_alpha_', scale, '_', time, '.txt', sep = ''))
    capture.output(psych::alpha(get(scale)[other_rows,], check.keys = TRUE),
        file = paste('./output/alphas/other_alpha_', scale, '_', time, '.txt', sep = ''))
}

#' Center data not based on mean, but based on (min + max) / 2
center_func <- function(x) {
    x - ((max(x, na.rm = TRUE) + min(x, na.rm = TRUE)) / 2)
}

# Create a dataframe for use in comparing self and other reported things.
df <- c()
met_names <- c()
for (scale in scales) {
    met_name <- sub('_scal', '', scale)
    current_sc <- as.matrix(get(scale))
    current_sc <- apply(current_sc, 2, center_func)
    df <- as.data.frame(cbind(df, rowMeans(current_sc)))
    met_names <- c(met_names, met_name)
}
colnames(df) <- met_names

# Add important meta info.
df$surVer <- dat$surVer
df$anonID <- dat$anonID

# Drop anyone who hasn't rated themselves AND been rated at least once.
rated_selves <- unique(df$anonID[self_rows])
rated_others <- unique(df$anonID[other_rows])
okayed_ids <- intersect(rated_selves, rated_others)

keep_rows <- df$anonID %in% okayed_ids & !is.na(df$anonID)
df <- df[keep_rows,]

# Average the scores of everyone reviewing the same person.
head(df[df$surVer == 2,])

# Create regression DF
X <- df[, !names(df) %in% c("surVer", "anonID")]
X_self <- c()
X_others <- c()
ids <- c()

for (id in unique(df$anonID)) {
    X_self <- rbind(X_self, X[df$anonID==id & df$surVer==1,])
    X_others <- rbind(X_others, colMeans(X[df$anonID==id & df$surVer==2,]))
    ids <- c(ids, id)
}

#Save the data
save(X_self, X_others, ids, file = "data/measurement_data.RData")
