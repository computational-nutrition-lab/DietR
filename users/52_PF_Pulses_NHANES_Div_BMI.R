# ===============================================================================================================
# Look at difference in BMI between "DivGroup" .
# Version 1
# Created on 02/09/2023 by Rie Sadohara
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
# Load data with DivGroup variable.
# ===============================================================================================================

  totals <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup.txt")
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
  
  plot(as.factor(totals_c$DivGroup), totals_c$BMXBMI)
  
# ANOVA.
  df <- totals_c
  hist(df$BMXBMI)
  df$BMXBMI_log <- log(df$BMXBMI) 
  hist(df$BMXBMI_log)
  
  myanova <- aov(BMXBMI     ~ DivGroup, data=df)
  myanova <- aov(BMXBMI_log ~ DivGroup, data=df) 
  # When log-transformed, the residuals are normally distributed, but still ANOVA is not significant (p=0.12)
  summary(myanova) 
  # as is: BMXBMI p= 2.76e-07. significant.
  # log:   logBMI p= 7.53e-08. significant.
  
  res1 <- residuals(myanova)
  hist(res1)
  qqnorm(res1, plot.it=TRUE)
  qqline(res1)
  boxplot(res1 ~ df$DivGroup)
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  anova(lm(res1sq ~ df$DivGroup))
  # OK.
  
  anova(lm(df$BMXBMI     ~ df$DivGroup))   
  anova(lm(df$BMXBMI_log ~ df$DivGroup))   
  # If ANOVA is significant, you can do a pairwise t-test.
  pairwise.t.test(df$BMXBMI, df$DivGroup, p.adjust.method = "holm") 
  
# ---------------------------------------------------------------------------------------------------------------
# HSD test.
  
# Run the mean separation and lettering for each phenotype.  
# BMXBMI
  model <- aov(BMXBMI ~ DivGroup, data=df)
  
  ### *** REPEAT the sectin below ***
  
  write.table(means_abc, "Div_means_abs_BMXBMI_BMI.txt", sep="\t", row.names=F, quote=F)
    
# logBMI
  model <- aov(BMXBMI_log ~ DivGroup, data=df)
  
  ### *** REPEAT ***
  
  write.table(means_abc, "Div_means_abs_logBMI.txt", sep="\t", row.names=F, quote=F)

# KCAL
  summary(totals$KCAL) # no missing data.
  summary(df$KCAL) # no missing data.
  model <- aov(KCAL ~ DivGroup, data=df)
  
  ### *** REPEAT ***
  
  write.table(means_abc, "Div_means_abs_KCAL_n4163.txt", sep="\t", row.names=F, quote=F)
  
library(agricolae)
# Define what to repeat first... 
# *** REPEAT FROM HERE ***
  # ANOVA table.
  summary(model)
  out <- HSD.test(model, "DivGroup", group=TRUE, console=TRUE)
  
  letters <- out$groups
  letters$DivGroup <- rownames(letters) 
  
  means <- out$means
  means$DivGroup <- rownames(means) 
  
  # Add letters to the means table.
  means_abc <- merge(means, letters[, c("DivGroup", "groups")], all.x = T, by="DivGroup")
  means_abc
  # *** REPEAT TILL HERE ***
  
# ---------------------------------------------------------------------------------------------------------------
