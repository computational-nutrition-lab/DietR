# ===============================================================================================================
# Add physical exercise component to ANCOVA model.
# But too few (n=65) SEQNs have reliable exercise data...  
# Version 1
# Created on 04/04/2023 by Rie Sadohara
# ===============================================================================================================

  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  library(SASxport)
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
# source("lib/data_overview.R") 

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist/")  

# ===============================================================================================================
# Load the prepared data with LegGroup variable and DivGroup variable.
# ===============================================================================================================

# Load the data saved in . 
  totals_c_wa <- read.delim("../Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup_waistBMI.txt")

  dim(totals_c_wa)
# should be 4038 rows, after removing rows containing missing data.

# Ensure there is no  missing data.
  naniar::vis_miss(totals_c_wa[, c("SEQN","BMXWAIST","BMXBMI","FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL", 
                                   "Gender", "RIDAGEYR")])

# Make the DivGroup as a factor.
  totals_c_wa$DivGroup <- factor(totals_c_wa$DivGroup, 
                                 levels = c('DivNA', 'Div0', 'Div1', 'Div2'))
# Make Gender as a factor.
  table(totals_c_wa$Gender)
# totals_c_wa$Gender <- factor(totals_c_wa$Gender, 
# levels = c('F', 'M'))

# Let's look at the Groups
  table(totals_c_wa$DivGroup, useNA = "ifany")

# DivNA  Div0  Div1  Div2 
# 2012  1246   387   393 

# Distribution of each group
  df <- totals_c_wa

# ---------------------------------------------------------------------------------------------------------------
# Download questionnair  
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PAQ_I.XPT", 
                destfile = "../../Raw_data/Physical_PAQ_I.XPT", mode="wb")
  
  qq <- read.xport("../../Raw_data/Physical_PAQ_I.XPT")
  
  head(qq)
  colnames(qq)
  
  https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PAQ_I.htm#PAQ710
  PAQ605 - Vigorous work activity
  PAQ610 - Number of days vigorous work
  PAD615 - Minutes vigorous-intensity work
  
  PAQ620 - Moderate work activity
  PAQ625 - Number of days moderate work
  PAD630 - Minutes moderate-intensity work
  
  PAQ635 - Walk or bicycle
  PAQ640 - Number of days walk or bicycle
  PAD645 - Minutes walk/bicycle for transportation
  
  PAQ650 - Vigorous recreational activities
  PAQ655 - Days vigorous recreational activities
  PAD660 - Minutes vigorous recreational activities
  
  PAQ665 - Moderate recreational activities
  PAQ670 - Days moderate recreational activities
  PAD675 - Minutes moderate recreational activities
  
  PAD680 - Minutes sedentary activity: 7777 refused, 9999 don't know.' 
  PAQ706 - Days physically active at least 60 min.
  PAQ710 - Hours watch TV or videos past 30 days
  PAQ715 - Hours use computer past 30
  PAQ722 - Any physical activities past 7 days
  
# ---------------------------------------------------------------------------------------------------------------
# only vis_miss of adults
  qqq <- qq[, 1:21]
  naniar::vis_miss(qqq)
  
  library(dplyr)
  
  plot(qqq$PAQ710)
  # 99 is dont know, 8 is 'don't watch TV/videos', so combine 0 (less than an hour) and 8 (zero).
  
# PAD680 sedantary (includes TV&games)   
  plot(qqq$PAD680)
  qqq$PAD680[1:100]
  qqq %>% filter(PAD680 == "." ) # no dots.
  qqq %>% filter(is.na(PAD680)) %>% count() # 2304 missing rows.
  
# Select only those with no missing data in sedentary activity hours. 
  is.numeric(qqq$PAD680)
  qqq2 <- qqq %>% filter(!is.na(PAD680)) %>% filter(PAD680 != 7777 & PAD680 != 9999)
  plot(qqq2$PAD680)
  # qqq2 has n=6895 SEQN.
  
# convert sedantary to hours.
  qqq2$sed_hr <- qqq2$PAD680 / 60
  plot(qqq2$sed_hr)
  
# Combine activity hours. 
  PAD615 - Minutes vigorous-intensity work
  PAD630 - Minutes moderate-intensity work
  PAD645 - Minutes walk/bicycle for transportation
  PAD660 - Minutes vigorous recreational activities
  PAD675 - Minutes moderate recreational activities
  
  # qqq3 <- qqq2 %>% filter(!is.na(PAD615)) %>% filter(PAD615 != 7777 & PAD615 != 9999) %>% 
  #                  filter(!is.na(PAD630)) %>% filter(PAD630 != 7777 & PAD630 != 9999) %>% 
  #                  filter(!is.na(PAD645)) %>% filter(PAD645 != 7777 & PAD645 != 9999) %>% 
  #                  filter(!is.na(PAD660)) %>% filter(PAD660 != 7777 & PAD660 != 9999) %>% 
  #                  filter(!is.na(PAD675)) %>% filter(PAD675 != 7777 & PAD675 != 9999)  
  # 
  # qqq3 <- qqq2 %>% filter(PAD615 != 7777 & PAD615 != 9999) %>%  
  #                  filter(PAD630 != 7777 & PAD630 != 9999) %>% 
  #                  filter(PAD645 != 7777 & PAD645 != 9999) %>% 
  #                  filter(PAD660 != 7777 & PAD660 != 9999) %>% 
  #                  filter(PAD675 != 7777 & PAD675 != 9999)  
  # 
  # nrow(qqq3) # only 139!
  
# Better change 7777 and 9999 to NA, so that I can add valid numbers of minutes.
  
  qqq2$vig_work_hr <- NA # [PAD615 vig. work, hr]
  qqq2$mod_work_hr <- NA # [PAD630 mod. work, hr]
  qqq2$transp_hr <- NA   # [PAD645 transportation, hr]
  qqq2$vig_rec_hr <- NA  # [PAD660 vig. recr, hr]
  qqq2$mod_rec_hr <- NA  # [PAD675 mod. recr, hr]
  qqq2$sed_hr <- NA      # [PAD680 sedentary, hr]
  
  
  for(i in 1:nrow(qqq2)){
    
  # [vig. work, hr] if PAD615 == 7777 or 9999, put NA.
    if(    is.na(qqq2$PAD615[i])){
      qqq2$vig_work_hr[i] <- NA
    }else if(qqq2$PAD615[i] == 7777 | qqq2$PAD615[i] == 9999){
      qqq2$vig_work_hr[i] <- NA
    }else{
      qqq2$vig_work_hr[i] <- qqq2$PAD615[i] /60
    }
    
  # [mod. work, hr] if PAD630 == 7777 or 9999, put 0.
    if(    is.na(qqq2$PAD630[i])){
      qqq2$mod_work_hr[i] <- NA
    }else if(    qqq2$PAD630[i] == 7777 | qqq2$PAD630[i] == 9999){
      qqq2$mod_work_hr[i] <- NA
    }else{
      qqq2$mod_work_hr[i] <- qqq2$PAD630[i] /60
    }
  
  # [transportation, hr] if PAD645 == 7777 or 9999, put 0.
    if(    is.na(qqq2$PAD645[i])){
      qqq2$transp_hr[i] <- NA
    }else if(    qqq2$PAD645[i] == 7777 | qqq2$PAD645[i] == 9999){
      qqq2$transp_hr[i] <- NA
    }else{
      qqq2$transp_hr[i] <- qqq2$PAD645[i] /60
    }

  # [vig. rec, hr] if PAD660 == 7777 or 9999, put 0.
    if(    is.na(qqq2$PAD660[i])){
      qqq2$vig_rec_hr[i] <- NA
    }else if(    qqq2$PAD660[i] == 7777 | qqq2$PAD660[i] == 9999){
      qqq2$vig_rec_hr[i] <- NA
    }else{
      qqq2$vig_rec_hr[i] <- qqq2$PAD660[i] /60
    }
    
  # [mod. rec, hr] if PAD660 == 7777 or 9999, put 0.
    if(    is.na(qqq2$PAD675[i])){
      qqq2$mod_rec_hr[i] <- NA
    }else if(    qqq2$PAD675[i] == 7777 | qqq2$PAD675[i] == 9999){
      qqq2$mod_rec_hr[i] <- NA
    }else{
      qqq2$mod_rec_hr[i] <- qqq2$PAD675[i] /60
    }
    
  # [sedentary, hr] if PAD680 == 7777 or 9999, put 0.
    if(    is.na(qqq2$PAD680[i])){
      qqq2$sed_hr[i] <- NA
    }else if(    qqq2$PAD680[i] == 7777 | qqq2$PAD680[i] == 9999){
      qqq2$sed_hr[i] <- NA
    }else{
      qqq2$sed_hr[i] <- qqq2$PAD680[i] /60
    }
  }
  table(qqq2$vig_work_hr, useNA = "always")
  table(qqq2$mod_work_hr, useNA = "always")
  table(qqq2$vig_rec_hr, useNA = "always")
  table(qqq2$mod_rec_hr, useNA = "always")
  table(qqq2$transp_hr, useNA = "always")
  table(qqq2$sed_hr, useNA = "always")

# ---------------------------------------------------------------------------------------------------------------
# Add all the physical exercise hours...
  qqq2$act_hr <- qqq2$vig_work_hr + qqq2$mod_work_hr + qqq2$transp_hr + 
                 qqq2$vig_rec_hr + qqq2$mod_rec_hr 
  table(qqq2$act_hr, useNA = "always")
  summary(qqq2$act_hr)
  
  qqq2 %>% filter(!is.na(act_hr)) %>% nrow() # n = 139 only!
  qqq2 %>% filter(act_hr > 12) %>% nrow() # 35 has more than 12 hr active hours.
                         
  qqq2 %>% filter(PAD630 == 9999) %>% select(PAD615, PAD630, PAD645, PAD660, PAD675)
  qqq2 %>% filter(PAD630 == 7777) %>% select(PAD615, PAD630, PAD645, PAD660, PAD675)
  
# ---------------------------------------------------------------------------------------------------------------
# Add all the wake activity hours...
  qqq2$wake_hr <- qqq2$act_hr + qqq2$sed_hr
  table(qqq2$wake_hr, useNA = "always")
  summary(qqq2$wake_hr) # some have > 24hr...
  
# Add sleep hours
# ---------------------------------------------------------------------------------------------------------------
# Sleep hours
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SLQ_I.XPT", 
                destfile = "../../Raw_data/Sleep_SLQ_I.XPT", mode="wb")
  
  ss <- read.xport("../../Raw_data/Sleep_SLQ_I.XPT")
  
  # SLD012 - Sleep hours
  table(ss$SLD012)
  sss2 <- ss %>% filter(!is.na(SLD012)) %>% filter(SLD012 != 7777 & SLD012 != 9999)

# sss2 has no missing data in sleep hours.
  table(sss2$SLD012, useNA = "always")  
  
  qqq2sss2 <- left_join(qqq2, sss2, by="SEQN") 

  table(qqq2sss2$SLD012, useNA = "always")  
  
# Add wake hours and sleep hours
  qqq2sss2$total_hr <- qqq2sss2$wake_hr + qqq2sss2$SLD012

# total = wake + sleep, so > 24hr should be a mistake.
  plot(qqq2sss2$total_hr)

# how many valid datapoints??? 
  qqq2sss2 %>% filter(total_hr <= 24) %>% nrow() # n=84 have <=24 hr in total. 
  qqq2sss2 %>% filter(total_hr > 24) %>% nrow()  # n=39 have > 24 hr in total.

  str(df)
  remove_val_label()
  str(qqq2sss2)
  qqq2sss2df <-  as.data.frame(qqq2sss2)
  str(qqq2sss2df)

  qqq2sss2$SEQN <-  qqq2sss2$SEQN 
  df2 <- merge(df, qqq2sss2, by="SEQN", all.x = T)

# total hours  
  havetotalhr <- df2[ !is.na(df2$total_hr), ]
  nrow(havetotalhr) # n=65
  table(havetotalhr$DivGroup)
  # DivNA  Div0  Div1  Div2 
  # 39     14    5     7 
  
# activity hours
  haveactivehr <- df2[ !is.na(df2$act_hr), ]
  nrow(haveactivehr) # n=65
  table(haveactivehr$DivGroup)
  # DivNA  Div0  Div1  Div2 
  # 39    14     5     7 
# It's the same!

  # Too few... only 65 has valid exercise data.      

# ===============================================================================================================
# PAQ706 Days physically active at least 60 min.
# ===============================================================================================================
  table(qqq2$PAQ706)
  qqq2$days_act <- NA
    
  for(i in 1:nrow(qqq2)){
    # [days active] if PAQ706 == 77 or 99, put NA.
    if(    is.na(qqq2$PAQ706[i])){
      qqq2$days_act[i] <- NA
    }else if(    qqq2$PAQ706[i] == 77 | qqq2$PAQ706[i] == 99){
      qqq2$days_act[i] <- NA
    }else{
      qqq2$days_act[i] <- qqq2$PAQ706[i] 
    }
  }
  
  table(qqq2$days_act, useNA = 'always')
  nrow( qqq2[!is.na(qqq2$days_act), ] )
  
# --------------------------------------------------------------------------------------------  
# Merge
  df2 <- merge(df, qqq2, by="SEQN", all.x = T)
  head(qqq2)
  table(df2$DivGroup)
  table(df2$days_act)
  table(df2$days_act, useNA = 'always')
  # There is no SEQN common between df and qqq2!! So, cannot use physically active days
  # as an exercise measure.
  
  write.table(df2[, c('DivGroup', 'days_act')], "out.txt", sep="\t", quote=F, row.names = F)
  write.table(df[, c('SEQN', 'DivGroup')], "Divgroup.txt", sep="\t", quote=F, row.names = F)
  write.table(qqq2[, c('SEQN', 'days_act')], "days_act.txt", sep="\t", quote=F, row.names = F)
  
  
  
  