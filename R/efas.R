#!/usr/bin/Rscript
#  more_efas.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.05.2018

## Run some EFA's, as described by the Analysis Overview document written
## on March 4th, 2018.

#Load up the eigenvalue adjustement package (Horn Analysis)
require(paran)
require(random.polychor.pa)
require(psych)
require(xtable)

#Read in data
sp17 <- read.csv("./data/reversed_data_sp_17.csv")

#### For the Spring 2017 Semester:
############################ An EFA on leader toughness (leadChal) items:
leadChal_cols <- sp17[,grep('^leadChal', colnames(sp17))]

#Plot the adjusted scree plot:
png("./images/leadChal_scree.png")
h_anal <- paran(leadChal_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(leadChal_cols, factors = 1, rotation = 'promax')
capture.output(print(fit),
               file =  './output/efas/class_leadChal_efa_sp17.txt')

# Make a pretty table.
means <- apply(leadChal_cols, 2, mean)
sds <- apply(leadChal_cols, 2, sd)
loadings <- as.numeric(fit$loadings)
total <- as.matrix(leadChal_cols) %*% as.numeric(fit$loadings)
cors <- apply(leadChal_cols, 2, function(i) cor(i, total))
df <- data.frame(means, sds, loadings, cors)
colnames(df) <- c('Mean', 'SD', "Loading", "IT Corr")

quest_text <- 
    c("I perform well in leading groups through challenging situations. (F)",
      "The thought of a challenging leadership situation excites me. (E)",
      "I persevere in the face of challenging leadership tasks. (P)",
      "I am able to stay calm when I run into unexpected leadership challenges. (F)",
      "I am quick to accept difficult leadership challenges. (E)",
      "I continue to put a lot of effort into leading my group even when progress is slow. (P)",
      "I am able to think clearly when leading people through difficult situations. (F)",
      "I like leading groups through challenges that might take a long time. (E)",
      "I continue working hard to lead even when success is looking less likely. (P)",
      "I perform well in leadership situations where my chances of success are slim. (F)",
      "I volunteer for challenging leadership assignments. (E)",
      "I never give up on a leadership challenge. (P)",
      "I thrive in difficult leadership situations. (F)")
rownames(df) <- quest_text

print(xtable(df), 
               file = './latex_out/efa_table.tex')


############### ### An EFA on Unconditional Happiness (uh) items and some FFMQ items:
uh_cols <- sp17[,grep('^uh\\_', colnames(sp17))] 
ffmq_cols <- sp17[,grep('^ffmq', colnames(sp17))]
#But remove columns 1, 6, and 11.
ffmq_cols <- ffmq_cols[, -grep('(1$|6$)', colnames(ffmq_cols))]
uhffmq_cols <- cbind(uh_cols, ffmq_cols)

#Make a histogram for each column
i <- 0
for (col in colnames(uhffmq_cols)) {
    i <- i + 1
    png(paste('./images/hists/ffmquh_hist_', i , '.png', sep = ''))
    hist(uhffmq_cols[,col], main = col)
    dev.off()
}

## Using Gaussian assumptions:
#Plot the adjusted scree plot:
png("./images/uhffmq_scree.png")
h_anal <- paran(uhffmq_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(uhffmq_cols, factors = 4, rotation = 'promax')
capture.output(print(fit),
               file =  './output/efas/class_uhffmq_efa_sp17.txt')

## Using Polychoric correlations (takes ordinal nature of data into account).
## Can't seem to do the polychoric parallel analysis.
#random.polychor.pa(nrep = 100, data.matrix = ffmq_cols, q.eigen = 0.99)

cor <- mixedCor(uhffmq_cols, c = 1:ncol(uh_cols), 
                 p = (ncol(uh_cols) + 1):(ncol(ffmq_cols) + ncol(uh_cols)))
fit <- fa(r=cor$rho, nfactors=4)
capture.output(print(fit),
               file =  './output/efas/class_polychor_uhffmq_efa_sp17.txt')


###################################### An EFA on leadChal, auth, tfl, ili
third_cols <- sp17[,grep('(^leadChal|^auth|^tfl|^ili)', colnames(sp17))]

#Plot the adjusted scree plot:
png("./images/third_scree.png")
h_anal <- paran(third_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(third_cols, factors = 1, rotation = 'promax')
capture.output(print(fit),
               file =  './output/efas/class_third_efa_sp17.txt')

###################################### An EFA on Leadership Nonresistance
sp18 <- read.csv("./data/reversed_data_sp_18.csv")

lnr_cols <- sp18[,grep('(^lnr_)', colnames(sp18))]

png("./images/lnr_scree.png")
lnr_anal <- paran(lnr_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(lnr_cols, factors = 1, rotation = 'promax')
capture.output(print(fit),
               file =  './output/efas/class_lnr_efa_sp18.txt')

###################################### An EFA on Leadership Nonresistance and Leader Toughness
lnrChal_cols <- sp18[,grep('(^lnr_|^leadChal_)', colnames(sp18))]

png("./images/lnrChal_scree.png")
lnrChal_anal <- paran(lnrChal_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(lnrChal_cols, factors = 2, rotation = 'promax')
capture.output(print(fit),
               file =  './output/efas/class_lnrChal_efa_sp18.txt')


###################################### An EFA on LNR, UH, VMI-UH, DIS and sc-hw
lnrother_cols <- sp18[,grep('(^lnr_|^leadChal|^uh_|^uh.vmi_|^dis_|^sc.hw_)', colnames(sp18))]

png("./images/lnrother_scree.png")
lnrother_anal <- paran(lnrother_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(lnrother_cols, factors = 6, rotation = 'promax')
capture.output(print(fit),
               file =  './output/efas/class_lnrother_efa_sp18.txt')

###################################### An EFA on UH again.
uh_cols <- sp18[,grep('^uh_', colnames(sp18))]

png("./images/uh_scree.png")
uh_anal <- paran(uh_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(uh_cols, factors = 2, rotation = 'promax')
capture.output(print(fit),
               file =  './output/efas/class_uh_efa_sp18.txt')

###################################### An EFA on LeadChal
leadChal_cols <- sp18[,grep('(^leadChal_)', colnames(sp18))]

png("./images/lnr_scree.png")
leadChal_anal <- paran(leadChal_cols, graph = TRUE)
dev.off()

#Fit a factor analysis with the appropriate number of cols
fit <- factanal(leadChal_cols, factors = 1, rotation = 'promax')
capture.output(print(fit),
               file =  './output/efas/class_leadChal_efa_sp18.txt')
