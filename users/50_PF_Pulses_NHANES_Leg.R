# ===============================================================================================================
# Quartiles of pulse consumers. 
# Evaluate the diversity of pulse consumption patterns and add "LegGroup" variable.
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
# Probably because legumes could be counted as Veg or protein foods, but not both.
# So it's giving researchers choices whether to include legumes as veg or protein, probably.

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
  cor.test(totals$PF_LEGUMES, totals$PF_LEG_perTOTAL)
  
  hist(totals$PF_TOTAL_LEG)  
  hist(totals$PF_LEG_perTOTAL)  
  summary(totals$PF_LEG_perTOTAL)  
  # There is 1 NA...
  totals[ is.na(totals$PF_LEG_perTOTAL), ] 
  # No protein intake, that is why. (But this person consumed dairy foods.)

# Compare those who are following special diets and who are not.  
  boxplot(x= totals$DRQSDIET, totals$PF_LEG_perTOTAL)
# Those who are following a diet (DRQSDIET=1) have low PF_LEG_perTOTAL... hmmm   
  
# ---------------------------------------------------------------------------------------------------------------
# Split people into 4 groups.
  
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
  
# Quantile 1 and 2 have zero legume consumption. not good.
  
  
# ---------------------------------------------------------------------------------------------------------------
# Select only those who consumed legumes first.
  totals_leg <- subset(totals, PF_LEGUMES > 0)

  nrow(totals_leg) /  nrow(totals)  * 100
  # 37% of the individuals consumed legumes at least some during the two days.
  
  # Split the legume consumption group into 3-tiles.
  # totals_leg <-  totals_leg %>% mutate(leg_tritile = ntile(PF_LEG_perTOTAL, 3))
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
  
# Add LegGroup names
  head(totals_leg$leg_tritile, 10)
  tabletomerge <- data.frame(leg_tritile=c(1,2,3),
                             LegGroup=c("Leg1", "Leg2", "Leg3"))            
  tabletomerge
  table(totals_leg$leg_tritile, useNA = "ifany")
  
  # Merge the two tables, thereby adding "Leg1" etc to totals_leg.
  totals_leg  <- merge(x=totals_leg, y=tabletomerge, all.x=T, by="leg_tritile")
  totals_leg[1:10, c("leg_tritile", "LegGroup")] 
  totals_leg[600:610, c("leg_tritile", "LegGroup")] 
  # it's sorted now, but looks good.
  
  # Add Leg0 as well, so that the "LegGroup" column in totals have Leg0 - Leg4.    
  totals$LegGroup <- NA
  totals_leg_SEQN_LegGroup <- totals_leg[, c("SEQN", "LegGroup")]
  head(totals_leg_SEQN_LegGroup)
  
  for(i in 1:nrow(totals)){
    if(totals$PF_LEGUMES[i] == 0){
      
      totals$LegGroup[i] <- "Leg0"
    
    }else{
      
      # Find which row the SEQN is.
      rownumber <- which( totals$SEQN[i] == totals_leg_SEQN_LegGroup$SEQN )
      
      # Pull the LegGroup (column 2) of that pariticular SEQN from the table.
      totals$LegGroup[i] <- 
        totals_leg_SEQN_LegGroup[rownumber, 2]
    } 
  }
  
  table(totals$LegGroup, useNA = "ifany")  
  
  is(totals$LegGroup)
  
  plot(as.factor(totals$LegGroup), totals$PF_LEGUMES_g)
  # Looks good!
  head(totals)
  
# So up to here, we looked at the distribution of PF_LEGUME, and grouped participants according to
# PF_LEGUME_g.
  write.table(totals, "Total_D12_FC_QC_mean_QC_demo_ga_body_meta_Leg.txt",
              sep="\t", row.names=F, quote=F)
  
  
  
  