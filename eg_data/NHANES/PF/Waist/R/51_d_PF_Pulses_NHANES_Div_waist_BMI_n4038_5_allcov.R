# ===============================================================================================================
# Build ancova models after meeting with Mark and David.
# Version 1
# Created on 04/02/2023 by Rie Sadohara
# ===============================================================================================================

  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
# source("lib/data_overview.R") 
# source("lib/add_gender_and_age.R") # to use AddGenderAgeGroups function.  

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

# change PF_TOTAL_LEG to PF_ALL.
  colnames(totals_c_wa)[ which ( names(totals_c_wa) == "PF_TOTAL_LEG")] <- "PF_ALL"

# Let's look at the Groups
  table(totals_c_wa$DivGroup, useNA = "ifany")

# DivNA  Div0  Div1  Div2 
# 2012  1246   387   393 

# ---------------------------------------------------------------------------------------------------------------
# Distribution of each group
  df <- totals_c_wa

  table(df$RIAGENDR, useNA = "ifany") # 1 is male.

# ===============================================================================================================
# Build a model with everything.
# ===============================================================================================================
  
# Abby's comments on 04/01/2023.
  # For the model, build a full model with all the covariates. If possible, add:
  # 1. A measure of physical activity from NHANES
  # 2. Keep KCAL in the model
  # 3. Keep fiber in the model (consider including as Fiber per 1000 kcal)
  # 4. Keep protein in the model, either total protein or total protein servings as you had is probably good. It's 
  #    not likely to be worth it to remove the protein from legumes from this variable.
  # 5. For the overall model, report the emmeans as you have been doing, and include the Type III SS for 
  #    the models/variables. I think you can get these with car::Anova(model, type = 3).

# Make a variable FIBE/1000kcal.  
  df$FIBE1000kcal <- df$FIBE / (df$KCAL*0.001)
  hist(df$FIBE1000kcal)  
  summary(df$FIBE1000kcal)  
  hist(df$FIBE)  
  hist(df$KCAL)  

# The higher the diversity, the higher FIBE consumption. and the higher FIBE/1000KCAL. 
  boxplot(FIBE ~ DivGroup, data=df)
  boxplot(FIBE1000kcal ~ DivGroup, data=df)
  plot(df$FIBE, df$FIBE1000kcal)
  plot(df$KCAL, df$FIBE)

  # boxplot(PF_ALL ~ DivGroup, data=df)
  # boxplot(PROT ~ DivGroup, data=df)
  # plot(df$PROT, df$PF_ALL)
  # cor.test(df$PROT, df$PF_ALL)
  # cor.test(df$PROT, df$FIBE)
  # df[1:10, c("SEQN", "PROT", "PF_ALL", "PF_TOTAL", "PF_LEGUMES")]
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# WAIST as response.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  aggregate(df$BMXWAIST, list(df$DivGroup), FUN=mean)
  #   Group.1         x
  # 1   DivNA 101.07003
  # 2    Div0 100.65546
  # 3    Div1  98.91059
  # 4    Div2  95.78931
  # DivNA-Div2 = 101.07003-95.78931 = 5.28072 cm. A positive value.  
  
# ===============================================================================================================
# BMXWAIST ~ DivGroup + Age + Gender + KCAL + FIBE + PF_ALL.
# ===============================================================================================================

  lm.agkfp <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + KCAL + FIBE + PF_ALL, data=df)
  typeiii <-  car::Anova(lm.agkfp, type="III")
  typeiii
  # All the terms have an effect.
  write.table(typeiii, "clipboard", sep="\t", row.names = T)  
  
  lm.agkemmeans <- emmeans::emmeans(lm.agkfp, pairwise ~ DivGroup )
  lm.agkemmeans
  # The more diverse, the less waist.
  # DivNA-Div2= 5.223 cm, p<.0001. 
  # Div0-Div2 = 4.504 cm, p<.0001. 
  
  write.table(lm.agkemmeans, "clipboard", sep="\t", row.names = F)  
  write.table(lm.agkemmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ===============================================================================================================
# Use FIBE/1000kcal instead.
#  BMXWAIST ~ DivGroup + Age + Gender + KCAL + FIBE/1000kcal + PF_ALL.
# ===============================================================================================================
  
  lm.agkf1000p <- lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + KCAL + FIBE1000kcal + PF_ALL, data=df)
  typeiii <-  car::Anova(lm.agkf1000p, type="III")
  typeiii
  # KCAL does not have an effect. hmm.
  
  write.table(typeiii, "clipboard", sep="\t", row.names = T)  
  
  lm.agkf1000pemmeans <- emmeans::emmeans(lm.agkf1000p, pairwise ~ DivGroup )
  lm.agkf1000pemmeans
  # The more diverse, the less waist.
  # DivNA-Div2= 5.397 cm, p<.0001. 
  # Div0-Div2 = 4.639 cm, p<.0001. 
  
  write.table(lm.agkf1000pemmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.agkf1000pemmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ===============================================================================================================
# Since KCAL was not significant, remove KCAL and only have FIBE/1000kcal.
# BMXWAIST ~ DivGroup + Age + Gender + FIBE/1000kcal + PF_ALL.
# ===============================================================================================================
  
  lm.agf1000p <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender  + FIBE1000kcal + PF_ALL, data=df)
  typeiii <-  car::Anova(lm.agf1000p, type="III")
  typeiii
  
  write.table(typeiii, "clipboard", sep="\t", row.names = T)  
  
  lm.agf1000pemmeans <- emmeans::emmeans(lm.agf1000p, pairwise ~ DivGroup )
  lm.agf1000pemmeans
  # The more diverse, the less waist.
  # DivNA-Div2= 5.276 cm, p<.0001. 
  # Div0-Div2 = 4.548 cm, p<.0001. 
  
  write.table(lm.agf1000pemmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.agf1000pemmeans[2], "clipboard", sep="\t", row.names = F)  
  
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# KCAL as the response.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ===============================================================================================================
# KCAL ~ DivGroup + Age + Gender + FIBE + PF_ALL.
# ===============================================================================================================

  lm.kcal.agfp <-    lm( KCAL ~ DivGroup + RIDAGEYR + Gender + FIBE + PF_ALL, data=df)
  typeiii <-  car::Anova(lm.kcal.agfp, type="III")
  typeiii
  # All the terms have an effect.
  write.table(typeiii, "clipboard", sep="\t", row.names = T)  
  
  lm.kcal.agfpemmeans <- emmeans::emmeans(lm.kcal.agfp, pairwise ~ DivGroup )
  lm.kcal.agfpemmeans
  write.table(lm.kcal.agfpemmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.kcal.agfpemmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ===============================================================================================================
# Add FIBE/1000kcal
# KCAL ~ DivGroup + Age + Gender + FIBE/1000kcal + PF_ALL.
# ===============================================================================================================

  
  lm.kcal.ag1000fp <-    lm( KCAL ~ DivGroup + RIDAGEYR + Gender + FIBE1000kcal + PF_ALL, data=df)
  
  typeiii <-  car::Anova(lm.kcal.ag1000fp, type="III")
  typeiii
  # All the terms have an effect.
  write.table(typeiii, "clipboard", sep="\t", row.names = T)  
  
  lm.kcal.ag1000fpemmeans <- emmeans::emmeans(lm.kcal.ag1000fp, pairwise ~ DivGroup )
  lm.kcal.ag1000fpemmeans
  write.table(lm.kcal.ag1000fpemmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.kcal.ag1000fpemmeans[2], "clipboard", sep="\t", row.names = F)  
  
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# BMI as the response.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ===============================================================================================================
# BMI ~ DivGroup + Age + Gender + KCAL + FIBE + PF_ALL.
# ===============================================================================================================
  
  lm.BMI.agkfp <-    lm( BMXBMI ~ DivGroup + RIDAGEYR + Gender + KCAL + FIBE + PF_ALL, data=df)
  typeiii <-  car::Anova(lm.BMI.agkfp, type="III")
  typeiii
  # All the terms have an effect.
  write.table(typeiii, "clipboard", sep="\t", row.names = T)  
  
  lm.BMI.agkfpemmeans <- emmeans::emmeans(lm.BMI.agkfp, pairwise ~ DivGroup )
  lm.BMI.agkfpemmeans
  write.table(lm.BMI.agkfpemmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.BMI.agkfpemmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ===============================================================================================================
# BMI ~ DivGroup + Age + Gender + KCAL + FIBE/1000kcal + PF_ALL.
# ===============================================================================================================
  
  lm.BMI.agkf1000kcalp <-    lm( BMXBMI ~ DivGroup + RIDAGEYR + Gender + KCAL + FIBE1000kcal + PF_ALL, data=df)
  typeiii <-  car::Anova(lm.BMI.agkf1000kcalp, type="III")
  typeiii
  # KCAL is not significant.
  write.table(typeiii, "clipboard", sep="\t", row.names = T)  
  
  lm.BMI.agkf1000kcalpemmeans <- emmeans::emmeans(lm.BMI.agkf1000kcalp, pairwise ~ DivGroup )
  lm.BMI.agkf1000kcalpemmeans
  write.table(lm.BMI.agkf1000kcalpemmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.BMI.agkf1000kcalpemmeans[2], "clipboard", sep="\t", row.names = F)  
  
# ===============================================================================================================
# Since KCAL was not significant, remove KCAL and only have FIBE/1000kcal.
#  BMI ~ DivGroup + Age + Gender + FIBE/1000kcal + PF_ALL.
# ===============================================================================================================
  
  lm.BMI.agf1000p <-  lm( BMXBMI ~ DivGroup + RIDAGEYR + Gender + FIBE1000kcal + PF_ALL, data=df)
  typeiii <-  car::Anova(lm.BMI.agf1000p, type="III")
  typeiii
  
  write.table(typeiii, "clipboard", sep="\t", row.names = T)  
  
  lm.BMI.agf1000pemmeans <- emmeans::emmeans(lm.BMI.agf1000p, pairwise ~ DivGroup )
  lm.BMI.agf1000pemmeans

  write.table(lm.BMI.agf1000pemmeans,    "clipboard", sep="\t", row.names = F)  
  write.table(lm.BMI.agf1000pemmeans[2], "clipboard", sep="\t", row.names = F)  
  
  
      
# ---------------------------------------------------------------------------------------------------------------
 