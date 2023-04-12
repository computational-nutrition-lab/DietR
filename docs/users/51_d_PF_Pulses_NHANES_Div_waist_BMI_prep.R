# ===============================================================================================================
# Subset waist variable and related covariates in totals and BMI, and take complete cases only 
# to analyze grouping by "DivGroup" variable.
# Version 1
# Created on 02/27/2023 by Rie Sadohara
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
  totals_c <- totals[complete.cases(totals[, c("SEQN", "BMXWAIST", "BMXBMI",
                                               "FIBE", "PF_LEGUMES", "PF_TOTAL_LEG", "KCAL", 
                                               "RIAGENDR", "RIDAGEYR")]), ]

  dim(totals_c)
  # 4038. 
  
  naniar::vis_miss(totals_c[, c("SEQN", "BMXWAIST","BMXBMI", "FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL", 
                                "Gender", "RIDAGEYR")])
  
  # Some checking
  summary(totals_c$BMXBMI)  
  table(totals_c$DivGroup)  
  plot( totals_c$DivGroup, totals_c$BMXWAIST) 
  plot( totals_c$DivGroup, totals_c$BMXBMI) 
  plot( totals_c$DivGroup, totals_c$FIBE) 
  plot( totals_c$DivGroup, totals_c$PF_TOTAL_LEG) 
  plot( totals_c$DivGroup, totals_c$PF_LEGUMES) 
  plot( totals_c$DivGroup, totals_c$KCAL)
  
  # Need to know the range of diversity index.
  # SEQNdiv_2 was made in script: 50_PF_Pulses_NHANES_Div.R.
  head(SEQNdiv_2)
  # SEQN   Shannon   Simpson Invsimpson  XSEQN
  # 1 84804 0.4885588 0.3097996   1.448854 X84804
  # 2 86670 0.9144513 0.5555141   2.249790 X86670
  # 3 88668 0.3179368 0.1748179   1.211854 X88668
  # 4 90694 0.6534051 0.4607873   1.854556 X90694
  # 5 92090 0.5459573 0.3601875   1.562958 X92090
  # 6 92397 0.5929170 0.4031662   1.675508 X92397
  
  Div1s <-  subset(totals_c, DivGroup == "Div1")["SEQN"] # save as a dataframe.
  nrow(Div1s)
  head(Div1s)
  Div1s_sha <- merge(Div1s, SEQNdiv_2[, c("SEQN", "Shannon")], all.x=T)
  head(Div1s_sha)
  summary(Div1s_sha)
  # Shannon: min 0.02739, max 0.65866.
  
  Div2s <-  subset(totals_c, DivGroup == "Div2")["SEQN"]
  nrow(Div2s)
  Div2s_sha <- merge(Div2s, SEQNdiv_2[, c("SEQN", "Shannon")], all.x=T)
  head(Div2s_sha)
  summary(Div2s_sha)
  # Shannon: min 0.6590, max 1.9508.
  
  
# ---------------------------------------------------------------------------------------------------------------
# Get basic summary statistics for all the variabless.
  SummaryStats(inputdf=totals_c, outfn="Waist/SummaryStats_totals_c_Waist_BMI.txt")

# Load in.
  summary <- read.delim("Waist/SummaryStats_totals_c_Waist_BMI.txt")
  head(summary)
  
# ===============================================================================================================
# Look for outliers.
# ===============================================================================================================
  boxplot(totals_c$BMXWAIST)
  boxplot(totals_c$BMXBMI)
  boxplot(totals_c$FIBE)
  boxplot(totals_c$PF_TOTAL_LEG)
  boxplot(totals_c$PF_LEGUMES)
  boxplot(totals_c$KCAL)
  max(totals_c$RIDAGEYR)
  
# Fiber (gr) seems to have two outliers...?
  boxplot(totals_c$FIBE)  
  hist(totals_c$FIBE)  # The outlier cannot be seen, but the X axis extends beyond 200. 
  max(totals_c$FIBE)  

  # Borderline value for upper outlier. 
  upper <- Q3 + IQR(totals_c$FIBE) *1.5  
  upper
  # How many rows are there that have values above upper outlier threshold?
  nrow( subset(totals_c, FIBE > upper)  )
  # 121! hmmm... the rule of thumb for outlier Q3 + IQRx1.5 seem to regard too many as an outlier.   
  
  # Look at the dietary data of the two outliers.
  totals_c[which (totals_c$FIBE > 80 ), ]
  # SEQN: 85955, 90993
  # These people ate  a lot of soy, nuts products and legume products. does not seem to be outliers... 
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
  
  head(totals_c[order(totals_c$PF_LEGUMES,decreasing = T), c("SEQN", "PF_LEGUMES", "FIBE")])
  # max = 16.35919 oz. of SEQN 90993. 
  # 16.35919 oz.x 28.35 = 463.783 g. 463 g of legume consumption.. possible if cooked, maybe?  
  # SEQN=90993 is the one that has outlier in FIBE too. 
  
  # 2nd highest = 12.241609 oz.
  # 12.241609 * 28.35 = 347.0496 g. hmmm 
# ---------------------------------------------------------------------------------------------------------------
# Look at these SEQN
  food <- read.delim("../Food_D12_FC_QC_demo.txt") 
  dim(food)
  # [1] 132959    153
  
  SEQN90993 <- food[ which(food$SEQN == 90993 ), ]
  dim(SEQN90993)
  SEQN90993[, c('V_TOTAL', "PF_TOTAL", "PF_SOY", "PF_LEGUMES","FoodCode")]
  
  foodnames <- read.delim("../FPED/FPED_1516_FoodNames_forR.txt")
  head(foodnames)
  colnames(foodnames)[1] <- "FoodCode"
  
  SEQN90993_fdnm <- merge(SEQN90993, foodnames, all.x = T, by="FoodCode")
  head(SEQN90993_fdnm)

# let's look at this person's diet..  
  SEQN90993_fdnm[, c('V_TOTAL', "PF_TOTAL", "PF_SOY", "PF_LEGUMES","FoodCode", "DESCRIPTION_f")]
  
  # DESCRIPTION_f
  # 1                                                                                 Milk_fat_free_(skim)
  # 2                                                                               Cheese_cottage_low_fat
  # 3                                                                               Cheese_cottage_low_fat
  # 4                                                                     Lentils_dry_cooked_made_with_oil
  # 5                                                                     Lentils_dry_cooked_made_with_oil
  # 6                                                                     Lentils_dry_cooked_made_with_oil
  # 7                                                                                             Soy_nuts
  # 8                                                                                       Cookie_fig_bar
  # 9                                                                                 Crackers_woven_wheat
  # 10                                                                    Crackers_woven_wheat_reduced_fat
  # 11                                                          Rice_white_cooked_fat_not_added_in_cooking
  # 12                                                          Rice_white_cooked_fat_not_added_in_cooking
  # 13                                                          Rice_white_cooked_fat_not_added_in_cooking
  # 14                                                           Cereal_(General_Mills_Cheerios_Honey_Nut)
  # 15                                                                                           Apple_raw
  # 16                                                                                           Apple_raw
  # 17                                                                   Potato_home_fries_with_vegetables
  # 18                                                                  Cabbage_green_cooked_made_with_oil
  # 19                                                         Cauliflower_cooked_from_fresh_made_with_oil
  # 20                                                                       Eggplant_cooked_made_with_oil
  # 21 Vegetable_combination_excluding_carrots_broccoli_and_dark-green_leafy_cooked_no_sauce_made_with_oil
  # 22  Vegetable_combinations_including_carrots_broccoli_and_or_dark-green_leafy_cooked_with_tomato_sauce
  # 23  Vegetable_combinations_including_carrots_broccoli_and_or_dark-green_leafy_cooked_with_tomato_sauce
  # 24                                                                        Coffee_instant_reconstituted
  # 25                                                                        Coffee_instant_reconstituted
  # 26                                                                                           Water_tap
  # 27                                                                                           Water_tap
  #   
# Note that this is the average of 2 days.
# male, 59 years old. he says he is not following specific diet, but he eats plant-based foods only.
# Meal occasion info...?
# If the three lentil meals are different meal occasations, like breakfast, lunch, and dinner, then 
# it's more believable. Otherwise, it could be duplication.
  
# Wanted to use SumByOccasion function to calculate totals by meal occasions, but there is no
# SumByOccasion for NHANES....

# Just look at this person's items data. SEQN=90993
  QCeditems <- read.delim('../../NHANES/Food_D12_FC_QC_demo_QCed.txt')

  QCeditems90993 <- subset(QCeditems, SEQN=="90993")
  
  # Meal occasion and food description. 
  # The original Occasion variable is DR1_030Z, DR2_030Z, according to the food data documentation, but the "DR1" and "DR2"
  # were removed in the processing, so just 030Z remained. R added 'X' in front of each variable when loading it again. 
  # So, the occasion variable is "X_030Z" in the items data. 
  mof <- QCeditems90993[, c("SEQN", "Main.food.description" , "Day", "X_030Z")]
  mof[order(mof$Day,  mof$X_030Z), ]
      
# SEQN90993 did have lentils and rice meals at different meal occasions on both days, and they are not duplicates. 
# So, these records are unlikely to be errors. Let's keep them, then... 
  
  
# ---------------------------------------------------------------------------------------------------------------
# Let's keep them for now...
  write.table("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup_waistBMI.txt", 
              x=totals_c,
              sep="\t", row.names = F, quote=F)
  
  