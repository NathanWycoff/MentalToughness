#!/usr/bin/Rscript
#  data_manip/data_processing.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.25.2018
library(psych)
library(dplyr)

#TODO: Maybe rename file as parcelling.R?

#Load the balanced parcelling function
source('./R/bal_parc.R')

files <- c("./data/reversed_data_fl_17.csv", 
           "./data/reversed_data_sp_17.csv", "./data/reversed_data_sp_18.csv")
for (file in files) {
    dat <- read.csv(file)

    # Get the semester these data are from
    time <- strsplit(strsplit(strsplit(file, '/')[[1]][3], '\\.')[[1]][1], 'reversed_data_')[[1]][2]

    # Filter out individuals with no leadership experience
    keep <- dat$chalFreq > 0 & (dat$formalExp > 0 | dat$informalExp > 0)
    dat <- dat[keep,]

    ###### Create Subscales
    ## Mindfulness Subscales
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
    hi_cors <- alpha(uh_hi_scal)$item.stats$r.cor
    hi_asgn <- bal_parc(hi_cors, parcels = 2)
    lo_cors <- alpha(uh_lo_scal)$item.stats$r.cor
    lo_asgn <- bal_parc(lo_cors, parcels = 2)
    #Subset
    uh_hi_p1_scal <- uh_hi_scal[,hi_asgn==1]
    uh_hi_p2_scal <- uh_hi_scal[,hi_asgn==2]
    uh_lo_p1_scal <- uh_lo_scal[,lo_asgn==1]
    uh_lo_p2_scal <- uh_lo_scal[,lo_asgn==2]

    ## Auth subscales
    auth_sa_scal <- dat[,grep('auth_(4|7|11)$', colnames(dat))]#Self Awareness
    auth_rt_scal <- dat[,grep('auth_(1|8|12)$', colnames(dat))]#Relational Transparency
    auth_imp_scal <- dat[,grep('auth_(2|5|9|13)$', colnames(dat))]#Internalized Moral Perspective
    auth_bp_scal <- dat[,grep('auth_(3|6|10|14)$', colnames(dat))]#Balanced Processing

    ## Leadchal Subscales
    # The original idea for doing this:
    #leadChal_emb_scal <- dat[,grep('leadChal_(2|5|8|11)$', colnames(dat))]#Embracing
    #leadChal_func_scal <- dat[,grep('leadChal_(1|4|7|10|13)$', colnames(dat))]#Functioning
    #leadChal_pers_scal <- dat[,grep('leadChal_(3|6|9|12)$', colnames(dat))]#Perseverance
    ##Balanced Parcelling for leadChal, as supported by an EFA
    lc_cols <- grep('leadChal_', colnames(dat))
    leadChal_scal <- dat[,lc_cols]
    fit <- alpha(leadChal_scal)
    cors <- fit$item.stats$r.cor
    asgn <- bal_parc(cors)
    leadChal_bp1_scal <- leadChal_scal[,asgn==1]
    leadChal_bp2_scal <- leadChal_scal[,asgn==2]
    leadChal_bp3_scal <- leadChal_scal[,asgn==3]

    ##Transformational Leadership Subscales
    tfl_vis_scal <- dat[,grep('tfl_(1|7|13)$', colnames(dat))]#Perseverance
    tfl_insp_scal <- dat[,grep('tfl_(2|8|14)$', colnames(dat))]#Inspirational Motivation
    tfl_int_scal <- dat[,grep('tfl_(3|9|15)$', colnames(dat))]#Inspirational Motivation
    tfl_sup_scal <- dat[,grep('tfl_(4|10|16)$', colnames(dat))]#Supportive Leadership
    tfl_pers_scal <- dat[,grep('tfl_(5|11|17)$', colnames(dat))]#Personal Recognition
    tfl_id_scal <- dat[,grep('tfl_(6|12|18)$', colnames(dat))]#Personal Recognition

    ##Identity Leadership
    ili_pro_scal <- dat[,grep('ili_(1|5|9|13)$', colnames(dat))]#Perseverance
    ili_champ_scal <- dat[,grep('ili_(2|6|10|14)$', colnames(dat))]#Perseverance
    ili_ent_scal <- dat[,grep('ili_(3|7|11|15)$', colnames(dat))]#Perseverance
    ili_emb_scal <- dat[,grep('ili_(4|8|12)$', colnames(dat))]#Perseverance

    ## bfi subscales; these calcs use the recoded reversed items 
    bfi_extrav_scal <- dat %>% select(bfi_1, bfi_6, bfi_11, bfi_16, bfi_21, bfi_26, bfi_31, bfi_36)
    bfi_agree_scal <- dat %>% select(bfi_2, bfi_7, bfi_12, bfi_17, bfi_22, bfi_27, bfi_32, bfi_37, bfi_42)
    bfi_consc_scal <- dat %>% select(bfi_3, bfi_8, bfi_13, bfi_18, bfi_23, bfi_28, bfi_33, bfi_38, bfi_43)
    bfi_neur_scal <- dat %>% select(bfi_4, bfi_9, bfi_14, bfi_19, bfi_24, bfi_29, bfi_34, bfi_39)
    bfi_open_scal <- dat %>% select(bfi_5, bfi_10, bfi_15, bfi_20, bfi_25, bfi_30, bfi_35, bfi_40, bfi_41, bfi_44)

    #Discomfort Intolerance Scale (DIS)
    dis_int_scal <- dat[,grep('dis_(1|2)$', colnames(dat))]#Intolerance
    dis_avd_scal <- dat[,grep('dis_(3|5|6)$', colnames(dat))]#Avoidance

    #Scales - The starts_with code won't work for all scales because it would also bring in the reverse-coded items in some cases.  To get around this as needed, I just listed out all of the items where necessary.
    uh_scal   <-  dat %>% select(starts_with("uh_")) #need the underscore there to not select "uh-vmi" items
    ili_scal  <-  dat %>% select(starts_with("ili"))
    ffmq_scal <-  dat %>% select(ffmq_2,	ffmq_3,	ffmq_4,	ffmq_5,	ffmq_7,	ffmq_8,	ffmq_9,	ffmq_10,	ffmq_12,	ffmq_13,	ffmq_14,	ffmq_15) #observing items not included in total ffmq calculation.
    grt_scal  <-  dat %>% select(starts_with("grt"))
    auth_scal <-  dat %>% select(starts_with("auth"))
    tfl_scal  <-  dat %>% select(tfl_1, tfl_7, tfl_13, tfl_2, tfl_8, tfl_14, tfl_3, tfl_9, tfl_15, tfl_4, tfl_10, tfl_16, tfl_5, tfl_11, tfl_17)
    uh.vmi_scal  <-  dat %>% select(starts_with("uh.vmi")) 
    #Note: Item 3 is dropped from dis
    dis_scal  <-  dat %>% select(dis_1, dis_2, dis_3, dis_4, dis_5, dis_6)
    leadChal_scal <- dat %>% select(starts_with("leadChal"))
    sc.hw_scal <- dat %>% select(starts_with("sc.hw"))#This also selects reversed items, so let's fix that:
    brs_scal <- dat %>% select(starts_with("brs"))

    # Do the 18 scales if necessary
    if (length(grep('18', time)) > 0) {
        har_scal <- dat %>% select(starts_with("har"))

        ##Create Mental Toughness subscales
        mt_cols <- grep('mt_', colnames(dat))
        mt_scal <- dat[,mt_cols]
        fit <- alpha(mt_scal)
        cors <- fit$item.stats$r.cor
        asgn <- bal_parc(cors)
        mt_bp1_scal <- mt_scal[,asgn==1]
        mt_bp2_scal <- mt_scal[,asgn==2]
        mt_bp3_scal <- mt_scal[,asgn==3]

        ##Create Leadership Nonresistance
        lnr_cols <- grep('lnr_', colnames(dat))
        lnr_scal <- dat[,lnr_cols]
        fit <- alpha(lnr_scal)
        cors <- fit$item.stats$r.cor
        asgn <- bal_parc(cors)
        lnr_bp1_scal <- lnr_scal[,asgn==1]
        lnr_bp2_scal <- lnr_scal[,asgn==2]
        lnr_bp3_scal <- lnr_scal[,asgn==3]
    }

    #See explanation document for differences in UH scale between semesters.  There was an error in survey for it in first semester.

    #Note that the UH-VMI variables were poorly named with a hyphen in between "uh-vmi_" which R understands as subtraction...  Be careful when using these in other analyses.

    #Record all of the names for scales.
    scales <- ls()[grep('scal', ls())]

    # Calculate alpha for each scale
    alphas <- c()
    for (scale in scales) {
        a <- alpha(get(scale), check.keys = TRUE)
        capture.output(a,
            file = paste('./output/alphas/alpha_', scale, '_', time, '.txt', sep = ''))
        alphas <- c(alphas, summary(a)$raw_alpha)
    }
    atable <- cbind(alphas, scales)
    colnames(atable) <- c("alpha", "scale")
    save(atable, file = paste('./RData/alphas/alphas_', time, '.RData', sep = ''))

    # We decide to drop item 3 for dis after examining the alphas
    dis_scal$dis_3 <- NULL

    # Calculate row means for each scale
    df <- c()
    met_names <- c()
    for (scale in scales) {
        met_name <- sub('_scal', '', scale)
        df <- as.data.frame(cbind(df, rowMeans(as.matrix(get(scale)))))
        met_names <- c(met_names, met_name)
    }
    colnames(df) <- met_names

    rm(scale)
    rm(scales)

    # Throw GPA in there as well.
    df$gpa <- dat$gpa

    #Save all the results in a convenient file
    write.csv(df, file = paste("./data/proc_met_", time, ".csv", sep = ''),
              row.names = FALSE)
}
