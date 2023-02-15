# ===============================================================================================================
# Analyze n=1776 individuals that have no missing data in 
# BMI, cholesterol measurements, and KCAL intake.
# Version 1
# Created on 02/15/2023 by Rie Sadohara
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
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/n1776")  

# ===============================================================================================================
# Load the prepared data with LegGroup variable and DivGroup variable.
# ===============================================================================================================
  
  totals_c_hdd <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_Div_cholesterol.txt")
  dim(totals_c_hdd)
  # should be 1776 rows.
  
# Ensure there is no  missing data.
  library(naniar)
  vis_miss(totals_c_hdd[, c("SEQN", "LBDHDD", "LBXTR", "LBDLDL", "LBXTC", "BMXBMI", "KCAL")])
  
# Make the DivGroup as a factor.
  totals_c_hdd$DivGroup <- factor(totals_c_hdd$DivGroup, 
                                  levels = c('DivNA', 'Div0', 'Div1', 'Div2'))

# Let's look at LegGroups
  table(totals_c_hdd$DivGroup, useNA = "ifany")
  
  # DivNA  Div0  Div1  Div2 
  # 886   563   169   158 
  
  SummaryStats(inputdf=totals_c_hdd, outfn="SummaryStats_totals_c_hdd.txt")
  
# ---------------------------------------------------------------------------------------------------------------
# Distribution of each group
  df <- totals_c_hdd

# Age
  hist(df$RIDAGEYR)
  summary(df$RIDAGEYR)
  plot(as.factor(df$DivGroup), df$RIDAGEYR) 

# Gender
  table(df$Gender, useNA = "ifany")
  table(df$DivGroup, df$Gender)

  plot(as.factor(df$Gender_Age), df$LBDHDD)  
  
# LBDHDD (HDL)
  hist(df$LBDHDD)
  plot(as.factor(df$DivGroup), df$LBDHDD) 
  df$LBDHDD_log <- log(df$LBDHDD)
  
# LBXTR (triglyceride)
  plot(as.factor(df$DivGroup), df$LBXTR) 
  hist(df$LBXTR)
  # may need to log transform  
  df$LBXTR_log <- log(df$LBXTR)
  hist(df$LBXTR_log)

# LBDLDL
  hist(df$LBDLDL)
  plot(as.factor(df$DivGroup), df$LBDLDL) 
  df$LBDLDL_log <- log(df$LBDLDL)
  hist(df$LBDLDL_log)

# LBXTC
  hist(df$LBXTC)
  plot(as.factor(df$DivGroup), df$LBXTC) 

# KCAL
  hist(df$KCAL)
  plot(as.factor(df$DivGroup), df$KCAL) 
  
  df$KCAL_log <- log(df$KCAL)
  hist(df$KCAL_log)
  plot(as.factor(df$DivGroup), df$KCAL_log) 

# BMI
  hist(df$BMXBMI)
  plot(as.factor(df$DivGroup), df$BMXBMI) 
  
# ---------------------------------------------------------------------------------------------------------------
# can I adjust for age and gender?
  ??ancova
  ancova.full <- aov(lm(Response_variable ~ Main_factor + Age + Gender + Age:Main_factor + Gender:Main_factor))   
  ancova.full <- aov(lm(KCAL ~ DivGroup + RIDAGEYR + Gender + RIDAGEYR:KCAL + Gender:KCAL, data=df))   
  summary(ancova.full)
  
  install.packages("car")
  library(car)
  car::Anova(ancova.full, type="III")
  
#  
  
  
# ---------------------------------------------------------------------------------------------------------------
# Run ANOVA
  myanova <- aov(LBDHDD     ~ DivGroup, data=df)
  myanova <- aov(LBDHDD_log ~ DivGroup, data=df)
  myanova <- aov(LBXTR      ~ DivGroup, data=df)
  myanova <- aov(LBXTR_log  ~ DivGroup, data=df)
  myanova <- aov(LBDLDL     ~ DivGroup, data=df)
  myanova <- aov(LBDLDL_log ~ DivGroup, data=df)
  myanova <- aov(LBXTC      ~ DivGroup, data=df)
  myanova <- aov(KCAL       ~ DivGroup, data=df)
  myanova <- aov(KCAL_log   ~ DivGroup, data=df)
  myanova <- aov(BMXBMI     ~ DivGroup, data=df)
  
  summary(myanova)
  
  res1 <- residuals(myanova)
  hist(res1)
  qqnorm(res1, plot.it=TRUE)
  qqline(res1)
  boxplot(res1 ~ df$DivGroup)
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  anova(lm(res1sq ~ df$DivGroup))
  
  # If ANOVA is significant, you can do a pairwise t-test.
  pairwise.t.test(df$LBDHDD, df$DivGroup, p.adjust.method = "fdr") 
  pairwise.t.test(df$LBDHDD_log, df$DivGroup, p.adjust.method = "fdr") # p value got higher... hmm?  

# ---------------------------------------------------------------------------------------------------------------
## Tukey's HSD, base R. Even though this function has p.adjust.method argument, the resulting 
 # p-values are the same; what you put into the argument doesn't matter. It makes sense because
 # Tukey HSD corrects family-wise error rate already.
  
# HDL  
       TukeyHSD(aov(LBDHDD     ~ DivGroup, data=df))
  plot(TukeyHSD(aov(LBDHDD     ~ DivGroup, data=df))) # You can even plot! 
  
# KCAL
       TukeyHSD(aov(KCAL     ~ DivGroup, data=df))
  plot(TukeyHSD(aov(KCAL     ~ DivGroup, data=df))) # You can even plot! 
       
  
# ---------------------------------------------------------------------------------------------------------------
## Tukey's Honestly Significant Difference. by Filipe. When you want letters separation.

# Run the mean separation and lettering for each phenotype.  

# LBDHDD
  model <- aov(LBDHDD     ~ DivGroup, data=df)
  model <- aov(LBDHDD_log ~ DivGroup, data=df)
  
  ### *** REPEAT ***
  write.table(means_abc, "Div_means_abs_LBDHDD_HDL.txt", sep="\t", row.names=F, quote=F)
  write.table(means_abc, "Div_means_abs_LBDHDD_HDL_log.txt", sep="\t", row.names=F, quote=F)
  # write.table(means_abc, "Div_means_abs_LBDHDD_HDL_n4204.txt", sep="\t", row.names=F, quote=F)
  
# LBXTR
  model <- aov(LBXTR ~ DivGroup, data=df)
  model <- aov(LBXTR_log ~ DivGroup, data=df)
  
  ### *** REPEAT ***
  write.table(means_abc, "Div_means_abs_LBXTR_tri.txt", sep="\t", row.names=F, quote=F)

# LBDLDL
  model <- aov(LBDLDL     ~ DivGroup, data=df)
  model <- aov(LBDLDL_log ~ DivGroup, data=df)
  
  ### *** REPEAT ***
  write.table(means_abc, "Div_means_abs_LBDLDL_LDL.txt", sep="\t", row.names=F, quote=F)
  write.table(means_abc, "Div_means_abs_LBDLDL_LDL_log.txt", sep="\t", row.names=F, quote=F)
  
#LBXTC - total cholesterol.
  model <- aov(LBXTC ~ DivGroup, data=df)
  
  ### *** REPEAT ***
  write.table(means_abc, "Div_means_abs_LBXTC_TotalCho.txt", sep="\t", row.names=F, quote=F)

# KCAL
  # summary(totals$KCAL) # no missing data.
  summary(df$KCAL) # no missing data.
  model <- aov(KCAL     ~ DivGroup, data=df)
  model <- aov(KCAL_log ~ DivGroup, data=df)
  
  ### *** REPEAT ***
  
  write.table(means_abc, "Div_means_abs_KCAL_n1790.txt", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Define what to repeat first...
  library(agricolae)
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