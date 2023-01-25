# ===============================================================================================================
# Quartiles of pulse consumers 
# Evaluate the diversity of pulse consumption patterns.
# Version 1
# Created on 01/25/2023 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Divide NHANES participants into four groups depending on their pulse intake: Q1 - Q4
# ===============================================================================================================
 
# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
  source("lib/add_gender_and_age.R") # to use AddGenderAgeGroups function.  
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  
  
# ---------------------------------------------------------------------------------------------------------------
#   
# Load averaged totals data, nutrition & food categories with demographic, gender-age, body measurements, and 
# metadata. From line 118 in 04_add_meta_GLU_index_NHANES.R.
  
  totals <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta.txt")
  dim(totals)
  head(totals, 2)
  
  # Age
  hist(totals$RIDAGEYR)
  # Gender
  table(totals$Gender, useNA="ifany")
  # BMI
  hist(totals$BMXBMI)
  # Diets 
  table(totals$DRQSDIET, useNA="ifany")
  # 1    2 
  # 674 3533 
  
# ---------------------------------------------------------------------------------------------------------------
# Take a look at the columns that starts with "PF" (protein foods)

  # df[, grepl(x=names(df), pattern="^x")]
  PF <- totals[, grepl(x=names(totals), pattern="^PF_")]
  dim(PF)  
  colnames(PF)  
  write.table(head(PF, 10), "clipboard", sep="\t", row.names=F, quote=F)  
# PF_TOTAL is the sum of all the PF_XXX foods EXCEPT PF_LEGUMES!!!
# Dunno why....  

# 'PF_MPS_TOTAL' is the sum of Meat, Poultry, Seafood.. 
  diet_mps <- totals[, c("DRQSDIET", "PF_MPS_TOTAL")]
  boxplot(x=diet_mps$DRQSDIET, diet_mps$PF_MPS_TOTAL)
  # Those folloing special diets have very low 'PF_MPS_TOTAL'.
  
  summary(PF$PF_LEGUMES*28.3495)
  hist(PF$PF_LEGUMES)   # 0-15. PF_Legumes are oz. equivalent.
  hist(PF$PF_LEGUMES*28.3495)   # 1 oz. = 28.3495 grams.
  summary(PF$PF_MPS_TOTAL)       # 0-31 oz.
  hist(PF$PF_MPS_TOTAL*28.3495) 
  
# ---------------------------------------------------------------------------------------------------------------
# PF_TOTAL_LEG <- PF_TOTAL + PF_LEGUME
  totals$PF_TOTAL_LEG <- totals$PF_TOTAL + totals$PF_LEGUME
  totals$PF_LEG_perTOTAL <- totals$PF_LEGUME / totals$PF_TOTAL_LEG *100 

  plot(totals$PF_LEGUMES, totals$PF_LEG_perTOTAL)
  
  hist(totals$PF_TOTAL_LEG)  
  hist(totals$PF_LEG_perTOTAL)  
  summary(totals$PF_LEG_perTOTAL)  
  # There is 1 NA...
  totals[ is.na(totals$PF_LEG_perTOTAL), ] 
  # No protein intake, that is why. (But this person consumed dairy foods.)

# Compare those who are following special diets and who are not.  
  boxplot(x= totals$DRQSDIET, totals$PF_LEG_perTOTAL)
# Those who are following a diet (DRQSDIET=1) have low PF_LEG_perTOTAL... hmmm   
  
# dplyr has quantile function... 
  library(tidyverse)
  # temp <- temp %>% mutate(quartile = ntile(value, 4))
  totals <-  totals %>% mutate(quartile = ntile(PF_LEG_perTOTAL, 4)) # percentage  
  totals$PF_LEGUMES_g <-  totals$PF_LEGUMES *28.3495        # Convert to grams
  totals <-  totals %>% mutate(quartile = ntile(PF_LEGUMES_g, 4)) # Amount of legumes
  table(totals$quartile)
  
  plot(x=totals$quartile, y=totals$PF_LEG_perTOTAL)
  plot(x=totals$quartile, y=totals$PF_LEGUMES_g)
  summary(totals$PF_LEG_perTOTAL)
  summary(totals$PF_LEGUMES)
  
  qua1 = subset(totals, quartile==1)
  summary(qua1$PF_LEG_perTOTAL)
  
  qua2 = subset(totals, quartile==2)
  summary(qua2$PF_LEG_perTOTAL)
  
  qua3 = subset(totals, quartile==3)
  summary(qua3$PF_LEG_perTOTAL)
  
  qua4 = subset(totals, quartile==4)
  summary(qua4$PF_LEG_perTOTAL)
  
  plot(x= totals$quartile, y= totals$PF_LEG_perTOTAL)
  
# Quantile 1 and 2 have zero legume consumption. 
# Select only those who consumed legumes.
  totals_leg <- subset(totals, PF_LEGUMES > 0)

  nrow(totals_leg) /  nrow(totals)  * 100
  # 37% of the individuals consumed legumes at least some during the two days.
  
  # Split the legume consumption into 3 tiles.
  totals_leg <-  totals_leg %>% mutate(leg_tritile = ntile(PF_LEG_perTOTAL, 3))
  totals_leg <-  totals_leg %>% mutate(leg_tritile = ntile(PF_LEGUMES_g,    3))
  table( totals_leg$leg_tritile)
  plot(totals_leg$leg_tritile, totals_leg$PF_LEG_perTOTAL)
  # Group 3 is still very high.. but better.. 

  Tri1 = subset(totals_leg, leg_tritile==1)
  summary(Tri1$PF_LEG_perTOTAL)
  summary(Tri1$PF_LEGUMES_g)
  
  Tri2 = subset(totals_leg, leg_tritile==2)
  summary(Tri2$PF_LEG_perTOTAL)
  summary(Tri2$PF_LEGUMES_g)
  
  Tri3 = subset(totals_leg, leg_tritile==3)
  summary(Tri3$PF_LEG_perTOTAL)
  summary(Tri3$PF_LEGUMES_g)
  

  
    
  #eg
  foo <- data.frame(a = 1:177,
                    b = runif(177, 50, 200),
                    stringsAsFactors = FALSE)
  head(foo)
  
  foo2 = 
    foo %>%
    mutate(quantile = ntile(b, 4))
 head(foo2)  
 
 table(foo2$quantile)
 