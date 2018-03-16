library(psych)
library(dplyr)

#TODO: Maybe rename file as parcelling.R?

files <- c("./data/reversed_data_fl_17.csv", "./data/reversed_data_sp_17.csv", "./data/reversed_data_sp_18.csv")
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

    ## Auth subscales
    auth_sa_scal <- dat[,grep('auth_(4|7|11)$', colnames(dat))]#Self Awareness
    auth_rt_scal <- dat[,grep('auth_(1|8|12)$', colnames(dat))]#Relational Transparency
    auth_imp_scal <- dat[,grep('auth_(2|5|9|13)$', colnames(dat))]#Internalized Moral Perspective
    auth_bp_scal <- dat[,grep('auth_(3|6|10|14)$', colnames(dat))]#Balanced Processing

    ## Leadchal Subscales
    leadChal_emb_scal <- dat[,grep('leadChal_(2|5|8|11)$', colnames(dat))]#Embracing
    leadChal_func_scal <- dat[,grep('leadChal_(1|4|7|10|13)$', colnames(dat))]#Functioning
    leadChal_pers_scal <- dat[,grep('leadChal_(3|6|9|12)$', colnames(dat))]#Perseverance

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
    bfi_extrav <- dat %>% select(bfi_1, bfi_6, bfi_11, bfi_16, bfi_21, bfi_26, bfi_31, bfi_36)
    bfi_agree <- dat %>% select(bfi_2, bfi_7, bfi_12, bfi_17, bfi_22, bfi_27, bfi_32, bfi_37, bfi_42)
    bfi_consc <- dat %>% select(bfi_3, bfi_8, bfi_13, bfi_18, bfi_23, bfi_28, bfi_33, bfi_38, bfi_43)
    bfi_neur <- dat %>% select(bfi_4, bfi_9, bfi_14, bfi_19, bfi_24, bfi_29, bfi_34, bfi_39)
    bfi_open <- dat %>% select(bfi_5, bfi_10, bfi_15, bfi_20, bfi_25, bfi_30, bfi_35, bfi_40, bfi_41, bfi_44)

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

    # Do the 18 scales if necessary
    if (length(grep('18', time)) > 0) {
        mt_scal <- dat %>% select(starts_with("mt"))
        lnr_scal <- dat %>% select(starts_with("lnr"))
        har_scal <- dat %>% select(starts_with("har"))
    }
    #brs_scal <- dat %>% select(starts_with("brs"))

    #See explanation document for differences in UH scale between semesters.  There was an error in survey for it in first semester.

    #Note that the UH-VMI variables were poorly named with a hyphen in between "uh-vmi_" which R understands as subtraction...  Be careful when using these in other analyses.

    #Record all of the names for scales.
    scales <- ls()[grep('scal', ls())]

    # Calculate alpha for each scale
    for (scale in scales) {
        capture.output(alpha(get(scale), check.keys = TRUE),
            file = paste('./output/alphas/alpha_', scale, '_', time, '.txt', sep = ''))
    }

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


    #Save all the results in a convenient file
    write.csv(df, file = paste("./data/proc_met_", time, ".csv", sep = ''),
              row.names = FALSE)
}
