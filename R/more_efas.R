#!/usr/bin/Rscript
#  more_efas.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.05.2018

## Run some EFA's, as described by the Analysis Overview document written
## on March 4th, 2018.

#Load up the eigenvalue adjustement package (Horn Analysis)
require(paran)

#Read in data
sp17 <- read.csv("./data/sp_17.csv")

#### For the Spring 2017 Semester:
### An EFA on leader toughness (leadChal) items:
leadChal_cols <- sp17[,grep('^leadChal', colnames(sp17))]

#Plot the adjusted scree plot:
png("./images/leadChal_scree.png")
h_anal <- paran(leadChal_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(leadChal_cols, factors = 1, rotation = 'promax')
capture.output(print(fit),
               file =  './output/class_leadChal_efa_sp17.txt')


### An EFA on Unconditional Happiness (uh) items and some FFMQ items:
uh_cols <- sp17[,grep('^uh\\_', colnames(sp17))]
ffmq_cols <- sp17[,grep('^ffmq', colnames(sp17))]
#But remove columns 1, 6, and 11.
ffmq_cols <- ffmq_cols[, -grep('(1$|6$)', colnames(ffmq_cols))]
uhffmq_cols <- cbind(uh_cols, ffmq_cols)

#Plot the adjusted scree plot:
png("./images/uhffmq_scree.png")
h_anal <- paran(uhffmq_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(uhffmq_cols, factors = 4, rotation = 'promax')
capture.output(print(fit),
               file =  './output/class_uhffmq_efa_sp17.txt')


### An EFA on leadChal, auth, tfl, ili
third_cols <- sp17[,grep('(^leadChal|^auth|^tfl|^ili)', colnames(sp17))]

#Plot the adjusted scree plot:
png("./images/third_scree.png")
h_anal <- paran(third_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(third_cols, factors = 4, rotation = 'promax')
capture.output(print(fit),
               file =  './output/class_third_efa_sp17.txt')
