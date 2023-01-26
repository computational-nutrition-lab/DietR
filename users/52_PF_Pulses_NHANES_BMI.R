# ===============================================================================================================
# Look at difference in BMI between "LegGroup" .
# Version 1
# Created on 01/26/2023 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

  library(SASxport)

  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
  source("lib/data_overview.R") 
  source("lib/add_gender_and_age.R") # to use AddGenderAgeGroups function.  

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF")  

# ===============================================================================================================
# Load data with LegGroup variable.
# ===============================================================================================================

  totals <- read.delim("../Total_D12_FC_QC_mean_QC_demo_ga_body_meta_Leg.txt")
  head(totals, 1)
  dim(totals)

# Age
  hist(totals$RIDAGEYR)
  max(totals$RIDAGEYR)
# Gender
  table(totals$Gender, useNA="ifany")
# BMI
  hist(totals$BMXBMI)
# Diets 
  table(totals$DRQSDIET, useNA="ifany")
  # 1    2 
  # 674 3533 
  

# BMI
  summary(totals$BMXBMI)
  
# Take only those that have BMI.
  totals_c <- totals[complete.cases(totals$BMXBMI), ]  
  
  plot(as.factor(totals_c$LegGroup), totals_c$BMXBMI)
  
# ANOVA.
  df <- totals_c
  hist(df$BMXBMI)
  df$logBMI <- log(df$BMXBMI) 
  hist(df$logBMI)
  
  myanova <- aov(BMXBMI ~ LegGroup, data=df)
  myanova <- aov(logBMI ~ LegGroup, data=df) 
  # When log-transformed, the residuals are normally distributed, but still ANOVA is not significant (p=0.12)
  summary(myanova) 
  # BMXBMI p=0.184. not significant.
  
  res1 <- residuals(myanova)
  hist(res1)
  qqnorm(res1, plot.it=TRUE)
  qqline(res1)
  boxplot(res1 ~ df$LegGroup)
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  anova(lm(res1sq ~ df$LegGroup))
  # OK.
  
  anova(lm(df$BMXBMI ~ df$LegGroup))   
  # If ANOVA is significant, you can do a pairwise t-test.
  pairwise.t.test(df$BMXBMI, df$LegGroup, p.adjust.method = "holm") 
  
# ---------------------------------------------------------------------------------------------------------------
# HSD test.
  # Define what to repeat first... 
  # *** REPEAT ***
  # ANOVA table.
  summary(model)
  out <- HSD.test(model, "LegGroup", group=TRUE, console=TRUE)
  
  letters <- out$groups
  letters$LegGroup <- rownames(letters) 
  
  means <- out$means
  means$LegGroup <- rownames(means) 
  
  # Add letters to the means table.
  means_abc <- merge(means, letters[, c("LegGroup", "groups")], all.x = T, by="LegGroup")
  means_abc
  # *** REPEAT ***
  
# Run the mean separation and lettering for each phenotype.  
# BMXBMI
  model <- aov(BMXBMI ~ LegGroup, data=df)
  
  ### *** REPEAT ***
  write.table(means_abc, "means_abs_BMXBMI_BMI.txt", sep="\t", row.names=F, quote=F)
    
# logBMI
  model <- aov(logBMI ~ LegGroup, data=df)
  
  ### *** REPEAT ***
  write.table(means_abc, "means_abs_logBMI.txt", sep="\t", row.names=F, quote=F)

    
# ---------------------------------------------------------------------------------------------------------------
