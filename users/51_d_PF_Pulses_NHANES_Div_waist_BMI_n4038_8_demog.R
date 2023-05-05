# ===============================================================================================================
# Make a demographics table for each of the DivGroups.
# Version 1
# Created on 05/05/2023 by Rie Sadohara
# ===============================================================================================================

  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  library(SASxport)
  library(dplyr)
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
# source("lib/data_overview.R") 

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist/")  

# ===============================================================================================================
# Load the prepared data with the DivGroup variable.
# ===============================================================================================================

# Load the data saved in . 
  totals_c_wa <- read.delim("../Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup_waistBMI.txt")

  dim(totals_c_wa)
# should be 4038 rows, after removing rows containing missing data.

# Ensure there is no missing data.
  naniar::vis_miss(totals_c_wa[, c("SEQN","BMXWAIST","BMXBMI","FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL", 
                                   "Gender", "RIDAGEYR")])

# Make the DivGroup as a factor.
  totals_c_wa$DivGroup <- factor(totals_c_wa$DivGroup, 
                                 levels = c('DivNA', 'Div0', 'Div1', 'Div2'))
# Make Gender as a factor.
  table(totals_c_wa$Gender)
# totals_c_wa$Gender <- factor(totals_c_wa$Gender, 
# levels = c('F', 'M'))

# Let's look at the Groups
  table(totals_c_wa$DivGroup, useNA = "ifany")

# DivNA  Div0  Div1  Div2 
# 2012  1246   387   393 

# Distribution of each group
  df <- totals_c_wa

# ---------------------------------------------------------------------------------------------------------------
  
# ===============================================================================================================
# Demographics table
# ===============================================================================================================
  gender, age, race/ethnicity, Family IPR, Education

# Gender    
  table(df$RIAGENDR,df$`DivGroup` , useNA = "always")
  
# Age    
  table(df$AgeGroup,df$`DivGroup` , useNA = "always")
  
# Race/Ethnicity.
  # Code or Value	| Value Description	| Count	| Cumulative	
  # 1	Mexican American	1921	1921	
  # 2	Other Hispanic	1308	3229	
  # 3	Non-Hispanic White	3066	6295	
  # 4	Non-Hispanic Black	2129	8424	
  # 6	Non-Hispanic Asian	1042	9466	
  # 7	Other Race - Including Multi-Racial	505	9971
  # .	Missing	0	9971
  table(df$RIDRETH3,df$`DivGroup` , useNA = "always")
  eth <- data.frame(table(df$RIDRETH3, df$`DivGroup` , useNA = "ifany")) # Make a long table.
  is(eth)
  colnames(eth) <- c("Ethnicity", "DivGroup", "value")

  ggplot(eth, aes(fill=Ethnicity, y=value, x=DivGroup)) + 
    geom_bar(position="fill", stat="identity")  + 
    # scale_fill_viridis_d() +
    scale_fill_viridis_d(labels = c("Mex Am", "Other Hispanic", "NH White", "NH Black", "NH Asian", "Other")) 
  
# Family IPR
# Need to know the number of family members, family income.
# https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references
# Poverty guideline in 2015: $11770 for first person, and $4160 for additional person.
# INDFMIN2 - Annual family income
# DMDFMSIZ - Total number of people in the Family
#   household: all individuals in the same address.
#   family: all individuals in a household who are related by blood, marriage, or adoption.

# Education
  # DMDEDUC3 - Education level - Children/Youth 6-19
  # 11	11th grade
  # 12  12th grade, no diploma
  # 13	High school graduate
  # 14	GED or equivalent
  # DMDEDUC2 - Education level - Adults 20+  
  
  
  
# ---------------------------------------------------------------------------------------------------------------
# Download other questionnairs  
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PAQ_I.XPT", 
                destfile = "../../Raw_data/Physical_PAQ_I.XPT", mode="wb")
  

  
  