# ===============================================================================================================
# Look at the Div group's metadata.
# Version 1
# Created on 02/14/2023 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
#  
# ===============================================================================================================

# Set your working directory to the main directory.
Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

  # library(SASxport)

  source("lib/specify_data_dir.R")
  source("lib/twoway_table.R")
  # source("lib/ggplot2themes.R") 
  # source("lib/data_overview.R") 
  # source("lib/add_gender_and_age.R") # to use AddGenderAgeGroups function.  

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF")  

# ---------------------------------------------------------------------------------------------------------------
 
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
  table(totals$DivGroup, useNA="ifany")

# ---------------------------------------------------------------------------------------------------------------
  
  TwoWayTableWithTotalsTxt(data = totals,
                           var1 = 'DRQSDIET', 
                           var2 = 'DivGroup', 
                           out.txt.fn = "Div_Diet.txt",
                           sort.by.var2.total = F)
  
 twoway_table
 
# Change colnames 
  names(twoway_table)[2:3]
  
# Select only those who are following specific diet(s).
  totals_dietfollowers <- subset(totals, DRQSDIET == 1)  
  table(totals_dietfollowers$DRQSDIET, useNA = "ifany")

 # DRQSDT1 - Weight loss/Low calorie diet
  table(totals_dietfollowers$DRQSDT1, useNA="ifany")
# Still, there are NAs in the DRQSDT1 variable.
    
# add percentage of 1 (following a specific diet) over total.
 twoway_table$following <- twoway_table$`1` / twoway_table$Total * 100
 twoway_table
 
# twowaytable doesn't work well when both variables have <NA>s.
# Need to replace NAs with "."

# list of diets.
 # DRQSDT1 - Weight loss/Low calorie diet
 # DRQSDT2 - Low fat/Low cholesterol diet
 # DRQSDT3 - Low salt/Low sodium diet
 # DRQSDT4 - Sugar free/Low sugar diet
 # DRQSDT5 - Low fiber diet
 # DRQSDT6 - High fiber diet
 # DRQSDT7 - Diabetic diet
 # DRQSDT8 - Weight gain/Muscle building diet
 # DRQSDT9 - Low carbohydrate diet
 # DRQSDT10 - High protein diet
 # DRQSDT11 - Gluten-free/Celiac diet
 # DRQSDT12 - Renal/Kidney diet
 # DRQSDT91 - Other special diet

 # No vegetarian/vegan!
 
 DBQ700 asks "How healthy is your diet?". Interesting, but I need to donwload this metadata first. 
 
  
# ---------------------------------------------------------------------------------------------------------------
  
