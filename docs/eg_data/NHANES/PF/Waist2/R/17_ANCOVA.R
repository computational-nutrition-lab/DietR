# ===============================================================================================================
# Waist 2
# Perform ANCOVA with demographic variables as covariates.
# Version 1
# Created on 05/25/2023 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Load data and packages.
# ===============================================================================================================

  library(ggplot2)
  library(dplyr)

# Set your working directory to the main directory.
# Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist2")  

# ---------------------------------------------------------------------------------------------------------------
# Load data.
  df <- read.delim('Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3676_DivGroup_DemoCat_Amt_Var.txt')  

# DO NOT FORGET TO DEFINE DivGroup as a factor!
  df$DivGroup <- factor(df$DivGroup, 
                        levels = c('DivNA', 'Div0', 'Div1', 'Div2'))  
  table(df$DivGroup, useNA = 'ifany')
  
  # DivNA  Div0  Div1  Div2 
  # 1840  1114   361   361 

# ===============================================================================================================
# Run ANCOVA with age_3, Gender, FIPL (income), eth_5, edu, and KCAL.
# ===============================================================================================================
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# BMI as response.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plain means.
  plainmeans <- aggregate(df$BMXBMI, list(df$DivGroup), FUN=mean)  
  write.table(plainmeans, "clipboard", sep="\t", row.names = F, quote=F)
  # Group.1        x
  # 1   DivNA 29.90511
  # 2    Div0 29.84354
  # 3    Div1 28.88199
  # 4    Div2 27.71911

    table(df$age_3, useNA="ifany")
  boxplot(BMXBMI ~ age_3, data=df)    
    table(df$RIAGENDR, useNA="ifany")
  boxplot(BMXBMI ~ RIAGENDR, data=df)    
    table(df$eth_5, useNA="ifany")
  boxplot(BMXBMI ~ eth_5, data=df)    
    table(df$FIPL, useNA="ifany")
  boxplot(BMXBMI ~ FIPL, data=df)    
    table(df$edu, useNA="ifany")
  boxplot(BMXBMI ~ edu, data=df)    
  plot(df$KCAL, df$BMXBMI)    
    
# ---------------------------------------------------------------------------------------------------------------
  
  lm_7 <-  lm( BMXBMI ~ DivGroup + age_3 + RIAGENDR + eth_5 + FIPL + edu + KCAL, data=df)
  
  typeiii7 <-  car::Anova(lm_7, type="III")
  typeiii7
  # FIPL and KCAL do not have an effect..  

  write.table(typeiii7, "clipboard", sep="\t", row.names = T)  
  
  lm.emmeans <- emmeans::emmeans(lm_7, pairwise ~ DivGroup )
  lm.emmeans
  
  write.table(lm.emmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.emmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ---------------------------------------------------------------------------------------------------------------
# Run ANCOVA without FIPL and KCAL.
  lm_5 <-  lm( BMXBMI ~ DivGroup + age_3 + RIAGENDR + eth_5 + edu , data=df)
  
  typeiii5 <-  car::Anova(lm_5, type="III")
  typeiii5
  write.table(typeiii5, "clipboard", sep="\t", row.names = T)  
  
  lm.emmeans <- emmeans::emmeans(lm_5, pairwise ~ DivGroup )
  lm.emmeans
  
  write.table(lm.emmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.emmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ---------------------------------------------------------------------------------------------------------------
# Run ANCOVA without Asians
  df2 <- subset(df, eth_5 != 6)
  boxplot(BMXBMI ~ eth_5, df2)
  table(df2$DivGroup)
  
  lm_72 <-  lm( BMXBMI ~ DivGroup + age_3 + RIAGENDR + eth_5 + FIPL + edu + KCAL, data=df2)
  
  typeiii72 <-  car::Anova(lm_72, type="III")
  typeiii72
  write.table(typeiii72, "clipboard", sep="\t", row.names = T)  
  # FIPL and KCAL do not have an effect..  
  
  write.table(typeiii72, "clipboard", sep="\t", row.names = T)  
  
  lm.emmeans <- emmeans::emmeans(lm_72, pairwise ~ DivGroup )
  lm.emmeans
  
  write.table(lm.emmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.emmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Waist as response.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plain means.
  plainmeans <- aggregate(df$BMXWAIST, list(df$DivGroup), FUN=mean)  
  write.table(plainmeans, "clipboard", sep="\t", row.names = F, quote=F)
  # Group.1         x
  # 1   DivNA 101.19842
  # 2    Div0 100.79004
  # 3    Div1  98.92410
  # 4    Div2  95.77008
  
  table(df$age_3, useNA="ifany")
  boxplot(BMXWAIST ~ age_3, data=df)    
  table(df$RIAGENDR, useNA="ifany")
  boxplot(BMXWAIST ~ RIAGENDR, data=df)    
  table(df$eth_5, useNA="ifany")
  boxplot(BMXWAIST ~ eth_5, data=df)    
  table(df$FIPL, useNA="ifany")
  boxplot(BMXWAIST ~ FIPL, data=df)    
  table(df$edu, useNA="ifany")
  boxplot(BMXWAIST ~ edu, data=df)    
  plot(df$KCAL, df$BMXWAIST)    
  
# ---------------------------------------------------------------------------------------------------------------
  
  lm_7 <-  lm( BMXWAIST ~ DivGroup + age_3 + RIAGENDR + eth_5 + FIPL + edu + KCAL, data=df)
  
  typeiii7 <-  car::Anova(lm_7, type="III")
  typeiii7
  # FIPL and KCAL do not have an effect..  
  
  write.table(typeiii7, "clipboard", sep="\t", row.names = T)  
  
  lm.emmeans <- emmeans::emmeans(lm_7, pairwise ~ DivGroup )
  lm.emmeans
  
  write.table(lm.emmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.emmeans[2], "clipboard", sep="\t", row.names = F)  

# ---------------------------------------------------------------------------------------------------------------
# Run ANCOVA without FIPL and KCAL.
  lm_5 <-  lm( BMXWAIST ~ DivGroup + age_3 + RIAGENDR + eth_5 + edu , data=df)
  summary(lm_5)
  typeiii5 <-  car::Anova(lm_5, type="III")
  typeiii5
  
  write.table(typeiii5, "clipboard", sep="\t", row.names = T)  
  
  lm.emmeans <- emmeans::emmeans(lm_5, pairwise ~ DivGroup )
  lm.emmeans
  
  write.table(lm.emmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.emmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ---------------------------------------------------------------------------------------------------------------
  # Run ANCOVA without Asians
  df2 <- subset(df, eth_5 != 6)
  boxplot(BMXWAIST ~ eth_5, df2)
  table(df2$DivGroup)
  
  lm_72 <-  lm( BMXWAIST ~ DivGroup + age_3 + RIAGENDR + eth_5 + FIPL + edu + KCAL, data=df2)
  
  typeiii72 <-  car::Anova(lm_72, type="III")
  typeiii72
  write.table(typeiii72, "clipboard", sep="\t", row.names = T)  
  # FIPL and KCAL do not have an effect..  
  
  write.table(typeiii72, "clipboard", sep="\t", row.names = T)  
  
  lm.emmeans <- emmeans::emmeans(lm_72, pairwise ~ DivGroup )
  lm.emmeans
  
  write.table(lm.emmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.emmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# kcal as response.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  