library(psych)
library(dplyr)

dat <- read.csv("./data/sp_17.csv")
colnames(dat)

#bfi recodings
dat$bfi_2 <- 8-dat$bfi_2R
dat$bfi_6 <- 8-dat$bfi_6R
dat$bfi_8 <- 8-dat$bfi_8R
dat$bfi_9 <- 8-dat$bfi_9R
dat$bfi_12 <- 8-dat$bfi_12R
dat$bfi_18 <- 8-dat$bfi_18R
dat$bfi_21 <- 8-dat$bfi_21R
dat$bfi_23 <- 8-dat$bfi_23R
dat$bfi_24 <- 8-dat$bfi_24R
dat$bfi_27 <- 8-dat$bfi_27R
dat$bfi_31 <- 8-dat$bfi_31R
dat$bfi_34 <- 8-dat$bfi_34R
dat$bfi_35 <- 8-dat$bfi_35R
dat$bfi_37 <- 8-dat$bfi_37R
dat$bfi_41 <- 8-dat$bfi_41R
dat$bfi_43 <- 8-dat$bfi_43R

#bfi subscales; these calcs use the recoded reversed items 
bfi_extrav <- dat %>% select(bfi_1, bfi_6, bfi_11, bfi_16, bfi_21, bfi_26, bfi_31, bfi_36)
bfi_agree <- dat %>% select(bfi_2, bfi_7, bfi_12, bfi_17, bfi_22, bfi_27, bfi_32, bfi_37, bfi_42)
bfi_consc <- dat %>% select(bfi_3, bfi_8, bfi_13, bfi_18, bfi_23, bfi_28, bfi_33, bfi_38, bfi_43)
bfi_neur <- dat %>% select(bfi_4, bfi_9, bfi_14, bfi_19, bfi_24, bfi_29, bfi_34, bfi_39)
bfi_open <- dat %>% select(bfi_5, bfi_10, bfi_15, bfi_20, bfi_25, bfi_30, bfi_35, bfi_40, bfi_41, bfi_44)

#FFMQ (Mindfulness) recoding
dat$ffmq_3 <- 4-dat$ffmq_3R
dat$ffmq_4 <- 4-dat$ffmq_4R
dat$ffmq_7 <- 4-dat$ffmq_7R
dat$ffmq_8 <- 4-dat$ffmq_8R
dat$ffmq_9 <- 4-dat$ffmq_9R
dat$ffmq_13 <- 4-dat$ffmq_13R
dat$ffmq_14 <- 4-dat$ffmq_14R

#Grit recoding; apparently I had NOT labeled the reversed items in this scale with an R
#So I will replace the items entirely with the reverse-coded items.
dat$grt_1 <- 6-dat$grt_1
dat$grt_3 <- 6-dat$grt_3
dat$grt_5 <- 6-dat$grt_5
dat$grt_6 <- 6-dat$grt_6

#"Self-control on Homework" reverse recoding
dat$sc.hw_1 <- 8-dat$"sc.hw_1R" 	
dat$sc.hw_2 <- 8-dat$"sc.hw_2R"	
dat$sc.hw_3 <- 8-dat$"sc.hw_3R"
dat$sc.hw_4 <- dat$"sc-hw_4" #not reverse.coded, but renamed with period, not -
dat$sc.hw_5 <- 8-dat$"sc.hw_5R"	
dat$sc.hw_6 <- 8-dat$"sc.hw_6R"
dat$sc.hw_7 <- 8-dat$"sc.hw_7R"	
dat$sc.hw_8 <- dat$"sc-hw_8" # " " for rename.
dat$sc.hw_9 <- 8-dat$"sc.hw_9R"	
dat$sc.hw_10 <- 8-dat$"sc.hw_10R"

#DIS reverse recoding; Note that the variable names are coded incorrectly.  Items 2 & 4 should not be reverse coded and should just be used as they are.
dat$dis_1 <- 10-dat$dis_1R
dat$dis_3 <- 10-dat$dis_3
dat$dis_6 <- 10-dat$dis_6

##Decided not to use this scale in analyses after all.
#BRS reverse recoding (my explanation page had previously incorrectly stated that this scale had no reversed items); I didn't indicate reversed items with an R so I just replaced the items below.
#dat$brs_2 <- 8-dat$brs_2
#dat$brs_4 <- 8-dat$brs_4
#dat$brs_6 <- 8-dat$brs_6

#TFL reverse coding
dat$tfl_13 <- 8-dat$tfl_13R

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
sc.hw_scal <- dat %>% select(starts_with("sc.hw"))
#brs_scal <- dat %>% select(starts_with("brs"))

#See explanation document for differences in UH scale between semesters.  There was an error in survey for it in first semester.

#Note that the UH-VMI variables were poorly named with a hyphen in between "uh-vmi_" which R understands as subtraction...  Be careful when using these in other analyses.


#These alphas are from the SP17 dataset.  
alpha(uh_scal, check.keys = TRUE) # all good (.92)
alpha(ili_scal, check.keys = TRUE) # all good (.92) 
alpha(ffmq_scal, check.keys = TRUE) # close enough (.67)
alpha(grt_scal, check.keys = TRUE)  # all good 0.7
alpha(auth_scal, check.keys = TRUE) # good 0.91
alpha(tfl_scal, check.keys = TRUE) # good 0.92
alpha(uh.vmi_scal, check.keys = TRUE) # good 0.88
alpha(dis_scal, check.keys = TRUE) #A little low at .62. Remove item 3 for .68. Perhaps item 3 is inappropriate (asks about aspirin) for this cadet sample?
alpha(leadChal_scal, check.keys = TRUE)   # (0.93)
alpha(sc.hw_scal, check.keys = TRUE) # 0.85
#alpha(brs_scal, check.keys = TRUE) # 0.79


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
#brs <- rowMeans(as.matrix(brs_scal))

#Put those bad boys into a data frame
df <- as.data.frame(cbind(uh, ili, ffmq, grt, auth, tfl, dis, leadChal, 
                          sc.hw, uh.vmi))

#Save all the results in a convenient file
save(df, file = "./data/processed_metrics.RData")
