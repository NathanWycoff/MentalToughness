#!/usr/bin/Rscript
#  R/data_processing_18.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.24.2018

##Process the raw 2018 data
library(psych)
library(dplyr)

dat <- read.csv('./data/leadershipData_rawSP18.csv')
colnames(dat)

#bfi subscales; these calcs use the recoded reversed items 
bfi_extrav <- dat %>% select(bfi_1, bfi_6, bfi_11, bfi_16, bfi_21, bfi_26, bfi_31, bfi_36)
bfi_agree <- dat %>% select(bfi_2, bfi_7, bfi_12, bfi_17, bfi_22, bfi_27, bfi_32, bfi_37, bfi_42)
bfi_consc <- dat %>% select(bfi_3, bfi_8, bfi_13, bfi_18, bfi_23, bfi_28, bfi_33, bfi_38, bfi_43)
bfi_neur <- dat %>% select(bfi_4, bfi_9, bfi_14, bfi_19, bfi_24, bfi_29, bfi_34, bfi_39)
bfi_open <- dat %>% select(bfi_5, bfi_10, bfi_15, bfi_20, bfi_25, bfi_30, bfi_35, bfi_40, bfi_41, bfi_44)
#Grit recoding; apparently I had NOT labeled the reversed items in this scale with an R

##Decided not to use this scale in analyses after all.
#BRS reverse recoding (my explanation page had previously incorrectly stated that this scale had no reversed items); I didn't indicate reversed items with an R so I just replaced the items below.
#dat$brs_2 <- 8-dat$brs_2
#dat$brs_4 <- 8-dat$brs_4
#dat$brs_6 <- 8-dat$brs_6

#Scales - The starts_with code won't work for all scales because it would also bring in the reverse-coded items in some cases.  To get around this as needed, I just listed out all of the items where necessary.
uh_scal   <-  dat %>% select(starts_with("uh_")) #need the underscore there to not select "uh-vmi" items
ili_scal  <-  dat %>% select(starts_with("ili"))
ffmq_scal <-  dat %>% select(ffmq_2,	ffmq_3,	ffmq_4,	ffmq_5,	ffmq_7,	ffmq_8,	ffmq_9,	ffmq_10,	ffmq_12,	ffmq_13,	ffmq_14,	ffmq_15) #observing items not included in total ffmq calculation.
grt_scal  <-  dat %>% select(starts_with("grt"))
auth_scal <-  dat %>% select(starts_with("auth"))
tfl_scal  <-  dat %>% select(tfl_1, tfl_7, tfl_13, tfl_2, tfl_8, tfl_14, tfl_3, tfl_9, tfl_15, tfl_4, tfl_10, tfl_16, tfl_5, tfl_11, tfl_17)
uh.vmi_scal  <-  dat %>% select(starts_with("uh.vmi")) 
dis_scal  <-  dat %>% select(dis_1, dis_2R, dis_3, dis_4R, dis_5, dis_6)
leadChal_scal <- dat %>% select(starts_with("leadChal"))
sc.hw_scal <- dat %>% select(starts_with("sc.hw"))#This also selects reversed items, so let's fix that:
sc.hw_scal <- sc.hw_scal[,9:16]
mt_scal <- dat %>% select(starts_with("mt"))
lnr_scal <- dat %>% select(starts_with("lnr"))
har_scal <- dat %>% select(starts_with("har"))
#brs_scal <- dat %>% select(starts_with("brs"))

#See explanation document for differences in UH scale between semesters.  There was an error in survey for it in first semester.

#Note that the UH-VMI variables were poorly named with a hyphen in between "uh-vmi_" which R understands as subtraction...  Be careful when using these in other analyses.

#These alphas are from the SP17 dataset.  
alpha(uh_scal, check.keys = TRUE)
alpha(ili_scal, check.keys = TRUE)
alpha(ffmq_scal, check.keys = TRUE)
alpha(grt_scal, check.keys = TRUE)
alpha(auth_scal, check.keys = TRUE)
alpha(tfl_scal, check.keys = TRUE)
alpha(uh.vmi_scal, check.keys = TRUE)
alpha(dis_scal, check.keys = TRUE)
alpha(leadChal_scal, check.keys = TRUE)
alpha(sc.hw_scal, check.keys = TRUE)
alpha(mt_scal, check.keys = TRUE)
alpha(lnr_scal, check.keys = TRUE)
alpha(har_scal, check.keys = TRUE)
#alpha(brs_scal, check.keys = TRUE)


#This is how the person I was working with calculated variable scores - feel free to do it differently if you'd like.  
uh <- rowMeans(as.matrix(uh_scal))
ili <- rowMeans(as.matrix(ili_scal))
ffmq <- rowMeans(as.matrix(ffmq_scal))
grt <- rowMeans(as.matrix(grt_scal))
auth <- rowMeans(as.matrix(auth_scal))
tfl <- rowMeans(as.matrix(tfl_scal))
dis <- rowMeans(as.matrix(dis_scal[,-3])) #drop item 3
leadChal <- rowMeans(as.matrix(leadChal_scal))
sc.hw <- rowMeans(as.matrix(sc.hw_scal))
uh.vmi <- rowMeans(as.matrix(uh.vmi_scal))
mt <- rowMeans(as.matrix(mt_scal))
lnr <- rowMeans(as.matrix(lnr_scal))
har <- rowMeans(as.matrix(har_scal))
#brs <- rowMeans(as.matrix(brs_scal))

#Put those bad boys into a data frame
df <- as.data.frame(cbind(uh, ili, ffmq, grt, auth, tfl, dis, leadChal, 
                          sc.hw, uh.vmi, mt, lnr, har))

#Save all the results in a convenient file
write.csv(df, file = "./data/processed_metrics_sp_18.csv", row.names = FALSE)
