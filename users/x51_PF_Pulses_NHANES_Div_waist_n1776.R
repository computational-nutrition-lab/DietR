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
  # should be 1776 rows, after removing .
  
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
  
  SummaryStats(inputdf=totals_c_hdd, outfn="SummaryStats_totals_c_hdd_n1776.txt")
  
# ---------------------------------------------------------------------------------------------------------------
# Distribution of each group
  df <- totals_c_hdd

# Age
  hist(df$RIDAGEYR)
  summary(df$RIDAGEYR)
  plot(as.factor(df$DivGroup), df$RIDAGEYR) 

# Gender
  table(df$Gender, useNA = "ifany")
  table(df$RIAGENDR, useNA = "ifany") # 1 is male.
  table(df$DivGroup, df$Gender)

  plot(as.factor(df$Gender_Age), df$LBDHDD)  
  plot(as.factor(df$Gender_Age), df$KCAL)  
  
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
  df$BMXBMI_log <- log(df$BMXBMI)
  hist(df$BMXBMI_log) # Looks good.
  
  
# ---------------------------------------------------------------------------------------------------------------
# can I adjust for age and gender?
  ??ancova
  # ancova.full <- aov(lm(Response_variable ~ Main_factor + Age + Gender + Age:Main_factor + Gender:Main_factor))   
  ancova.full <- aov(lm(KCAL ~ DivGroup + RIDAGEYR + Gender + RIDAGEYR:DivGroup + Gender:DivGroup, data=df))   
  ancova.full <- aov(lm(KCAL ~ DivGroup + RIDAGEYR + Gender , data=df))   
  ancova.full <- aov(lm(BMXBMI ~ DivGroup + RIDAGEYR + Gender, data=df))   
  ancova.full <- aov(lm(BMXBMI ~ DivGroup + KCAL + RIDAGEYR + Gender + DivGroup:KCAL  
                                 + DivGroup:RIDAGEYR + DivGroup:Gender , data=df))  
  
  ancova.full <- aov(lm( LBDHDD ~ DivGroup + KCAL + RIDAGEYR + Gender + DivGroup:KCAL  
                        + DivGroup:RIDAGEYR + DivGroup:Gender , data=df))   
  ancova.full <- aov(lm( FIBE ~ DivGroup + KCAL + RIDAGEYR + Gender + DivGroup:KCAL  
                         + DivGroup:RIDAGEYR + DivGroup:Gender , data=df))   
 
  # Models with different complexity to predict waist circumference.
  anova.simple <-     aov(lm( BMXWAIST ~ DivGroup, data=df))  
  ancova.agegender <- aov(lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender, data=df)) 
  ancova.full <-      aov(lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + FIBE + 
                                         PF_TOTAL_LEG + KCAL, data=df))   
  
  library(car)
  # For Simple model, summary(anova) and car::Anova(type III) have the same results. Makes sense. 
  summary(anova.simple)
  car::Anova(anova.simple,     type="III")
  
  # Simple + Age + Gender
  car::Anova(ancova.agegender, type="III")
  # Age, Gender has effect; but DivGroup is still significant.
  
  # Simple + Age + Gender + fiber intake, total consumption of legume/seeds/nuts as protein foods (PF_TOTAL_LEG), KCAL.
  car::Anova(ancova.full,      type="III")
  # With the fiber etc., the difference between DivGroups is marginal.  
  
  boxplot(df$DivGroup, df$PF_TOTAL_LEG)
  

#    
  
  
  
  
# ---------------------------------------------------------------------------------------------------------------
# Run ANOVA
  myanova <- aov(LBDHDD     ~ DivGroup, data=df)

 #### how to plot 4 charts related to your residuals. ####
  res1 <- residuals(myanova)
  # Generate a 2x2 plot field. 
  par(mfrow = c(2, 2)) 
  # Histogram of res1.
  hist(res1)
  # QQ plot of res1. 
  qqnorm(res1, plot.it=TRUE)
  qqline(res1)
  # Boxplot
  boxplot(res1 ~ df$DivGroup)
  title("Boxplot of res1")
  # produce residual vs. fitted plot
  plot(fitted(myanova), res1)
  #add a horizontal line at 0 
  abline(0,0)
  title("Fitted vs. Res1 plot")
  #### Till here. ####
  
  #### Levene's test ####
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  Levenes_test <- anova(lm(res1sq ~ df$DivGroup))
  Levenes_test
  ########
  
  p <- modelsummary
  # p[[1]][1]
  # pvalue <- p[[1]][5]

  # If ANOVA is significant, you can do a pairwise t-test.
  pairwise.t.test(df$LBDHDD,     df$DivGroup, p.adjust.method = "none") 
  pairwise.t.test(df$LBDHDD_log, df$DivGroup, p.adjust.method = "fdr") # p value got higher... hmm?  
  pairwise.t.test(df$BMXBMI,     df$DivGroup, p.adjust.method = "none") 
  pairwise.t.test(df$BMXBMI_log, df$DivGroup, p.adjust.method = "none") 
  pairwise.t.test(df$KCAL,       df$DivGroup, p.adjust.method = "none") 
  
  aggregate(df$LBDHDD, list(df$DivGroup), FUN=mean)
  
