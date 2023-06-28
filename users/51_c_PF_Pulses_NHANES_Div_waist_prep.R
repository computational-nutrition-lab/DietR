# ===============================================================================================================
# Subset waist variable and related covariates in totals, and take complete cases only 
# to analyze grouping by "DivGroup" variable.
# Version 1
# Created on 02/22/2023 by Rie Sadohara
# Added outlier removal on 05/19/2023.
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

# Load the totals with DivGroup and 4207 people. 
  totals <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup.txt")
  dim(totals) # 4207 rows.
  head(totals, 1)
  
# Define the DivGroup as a factor.
  totals$DivGroup <- factor(totals$DivGroup, 
                            levels = c('DivNA', 'Div0', 'Div1', 'Div2'))
  
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
  
# ---------------------------------------------------------------------------------------------------------------
# Take complete cases only.
  totals_c <- totals[complete.cases(totals[, c("SEQN", "BMXWAIST",
                                               "FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL", 
                                               "Gender", "RIDAGEYR")]), ]

  dim(totals_c)
  # 4046. 
  
  naniar::vis_miss(totals_c[, c("SEQN", "BMXWAIST", "FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL", 
                                "Gender", "RIDAGEYR")])
  
  # Some checking
  summary(totals_c$BMXBMI) # Note that BMI has 44 missing data. 
  plot( totals_c$DivGroup, totals_c$FIBE) 
  plot( totals_c$DivGroup, totals_c$PF_TOTAL_LEG) 
  plot( totals_c$DivGroup, totals_c$PF_LEGUMES) 
  plot( totals_c$DivGroup, totals_c$KCAL)

# ---------------------------------------------------------------------------------------------------------------
# Get basic summary statistics for all the variabless.
  SummaryStats(inputdf=totals_c, outfn="Waist/SummaryStats_totals_c_Waist.txt")

# ===============================================================================================================
# Look for outliers.
# ===============================================================================================================
  boxplot(totals_c$BMXWAIST)
  boxplot(totals_c$FIBE)
  boxplot(totals_c$PF_TOTAL_LEG)
  boxplot(totals_c$PF_LEGUMES)
  boxplot(totals_c$KCAL)
  max(totals_c$RIDAGEYR)
  
# Fiber (gr) seems to have two outliers...?
  boxplot(totals_c$FIBE)  
  hist(totals_c$FIBE)  # The outlier cannot be seen, but the X axis extends beyond 200. 
  max(totals_c$FIBE)  
  Q3 <- summary(totals_c$FIBE)[5]
  Q3
  # Borderline value for outlier. 
  upper <- Q3 + IQR(totals_c$FIBE) *1.5  
  upper
  # How many rows are there that have values above upper outlier threshold?
  nrow( subset(totals_c, FIBE > upper)  )
  # 121! hmmm... the rule of thumb for outlier Q3 + IQRx1.5 seem to regard too many as an outlier.   
  
  # Look at the dietary data of the two outliers.
  totals_c[which (totals_c$FIBE > 80 ), ]
  # These people ate  alot of soy, nuts products and legume products. does not seem to be outliers... 
  # I will keep them for now.
  
# ---------------------------------------------------------------------------------------------------------------
# PF_LEGUMES seems to have an outlier too?
  boxplot(totals_c$PF_LEGUMES)  
  hist(totals_c$PF_LEGUMES)  # non-normal distribution.
  
  Q3 <- summary(totals_c$PF_LEGUMES)[5]
  Q3
  # Borderline value for outlier. 
  upper <- Q3 + IQR(totals_c$PF_LEGUMES) *1.5  
  upper
  # How many rows are there that have PF_LEGUMES value of above upper outlier threshold?
  nrow( subset(totals_c, PF_LEGUMES > upper)  ) # 411 wow.
  
  head(totals_c[order(totals_c$PF_LEGUMES,decreasing=T), c("SEQN", "PF_LEGUMES", "FIBE")])
  # max = 16.35919 oz. 
  # 16.35919 oz.x 28.35 = 463.783 g. 463g of legume consumption.. possible if cooked, maybe?  
  # SEQN=90993 is the one that has outlier in FIBE too. 
  
  # 2nd highest = 12.241609 oz.
  # 12.241609 * 28.35 = 347.0496 g. hmmm 
  
# ---------------------------------------------------------------------------------------------------------------
# Let's keep them for now...
  write.table("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup_waist.txt", x=totals_c,
              sep="\t", row.names = F, quote=F)
  
  