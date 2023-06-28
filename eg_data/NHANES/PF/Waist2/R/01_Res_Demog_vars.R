# ===============================================================================================================
# Waist 2.
# Check missing data in the response and demographic variables, then remove them. 
# Version 1
# Created on 05/19/2023 by Rie Sadohara
# Replaced "OTU" with "IFC" and "n3677" with "n3642" on 06/28/2023 by Rie Sadohara
# Output as comments were updated. 
# ===============================================================================================================
  source("lib/specify_data_dir.R")

# Set your working directory to the main directory.
# Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  

# **** LOOK AT THE NHANES VARIABLE DESCRIPTIONS AND CHECK THE NUMBER OF MISSING DATA ETC. ****  
# https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.htm#RIDAGEYR

# ===============================================================================================================
# Load totals data and check if there is missing data.
# ===============================================================================================================

# Load averaged totals data, nutrition & food categories with demographic, gender-age, body measurements, and
# metadata. From line 118 in 04_add_meta_GLU_index_NHANES.R.
  totals <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta.txt")

# This should have 4,164 people.    
  nrow(totals)   

# Variables of interest ... 
  vars <- c("BMXBMI", "BMXWAIST", "RIDAGEYR", "RIAGENDR", "RIDRETH3", "INDFMPIR", "DMDEDUC3", "DMDEDUC2", "KCAL")
  
# ---------------------------------------------------------------------------------------------------------------
# BMI
  library(dplyr)
  summary(totals$BMXBMI) # 43 missing data.
  totals %>% filter(BMXBMI == ".") %>% nrow()  # No ".".
  
  # Remove the 43 rows that have missing BMI.
  totals <- totals[!is.na(totals$BMXBMI), ]
  nrow(totals) # 4121.
  
  hist(totals$BMXBMI)
  head(totals[order(totals$BMXBMI, decreasing = T), "BMXBMI"])
  
# ---------------------------------------------------------------------------------------------------------------
# Waist
  summary(totals$BMXWAIST) # 123 missing data.
  totals %>% filter(BMXWAIST == ".") %>% nrow()  # No ".".
  
  # Remove the 125 rows that have missing waist.
  totals <- totals[!is.na(totals$BMXWAIST), ]
  nrow(totals) # 3998.

  hist(totals$BMXWAIST)
  head(totals[order(totals$BMXWAIST, decreasing = T), "BMXWAIST"])
  
# Waist and BMI do not seem to have outliers.  
  
# ---------------------------------------------------------------------------------------------------------------
# Age
  hist(totals$RIDAGEYR)
  max(totals$RIDAGEYR)
  summary(totals$RIDAGEYR) # no missing data.
  totals %>% filter(RIDAGEYR == ".") %>% nrow()  # No ".".
  
# ---------------------------------------------------------------------------------------------------------------
# Gender
  table(totals$RIAGENDR, useNA="ifany") # no missing data.
  totals %>% filter(RIAGENDR == ".") %>% nrow()  # No ".".
  
# ---------------------------------------------------------------------------------------------------------------
# Ethnicity
  # Race/Ethnicity. RIDRETH3
  # Code or Value	| Value Description	| Count	| Cumulative	
  # 1	Mexican American	1921	1921	
  # 2	Other Hispanic	1308	3229	
  # 3	Non-Hispanic White	3066	6295	
  # 4	Non-Hispanic Black	2129	8424	
  # 6	Non-Hispanic Asian	1042	9466	
  # 7	Other Race - Including Multi-Racial	505	9971
  # .	Missing	0	9971

# Since missing data is marked as ".", need to look for ".", not "NA".
  totals %>% filter(RIDRETH3 == ".") %>% nrow()  # No ".".
  
  summary(totals$RIDRETH3)
    
# ---------------------------------------------------------------------------------------------------------------
# Family income and poverty ratio
  # https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references
  # Poverty guideline in 2015: $11770 for first person, and $4160 for additional person.
  # INDFMIN2 - Annual family income
  # DMDFMSIZ - Total number of people in the Family
  #   household: all individuals in the same address.
  #   family:    all individuals in a household who are related by blood, marriage, or adoption.
  
  # INDFMPIR - Ratio of family income to poverty
  # "." is missing.
  totals %>% filter(INDFMPIR == ".") %>% nrow()  # no ".", BUT!!
  totals %>% filter( is.na(INDFMPIR)) %>% nrow() # some NAs!
  summary(totals$INDFMPIR) # There are 356 NAs.

  # Remove the 356 rows that have missing data.
  totals <- totals[!is.na(totals$INDFMPIR), ]
  nrow(totals) # 3642.
  
  summary(totals$INDFMPIR)
  hist(totals$INDFMPIR)

# ---------------------------------------------------------------------------------------------------------------
# Education level
  # DMDEDUC3 - Education level - Children/Youth 6-19
  #   0-12:  < HS, 
  #   13-15, 55, 66:  HS some college 
  #   77 refused; 99 don't know. "." missing.
  totals %>% filter(DMDEDUC3 == 77 | DMDEDUC3 == 99 | DMDEDUC3 == ".") %>% nrow()  # No 77, 99, or ".".
  
  table(totals$DMDEDUC3, useNA = "ifany") # Has a lot of missing data, but because they are adults.
  # Select people 19 yo or younger, then see.
  totals %>% filter(RIDAGEYR <= 19) %>% filter(is.na(DMDEDUC3)) %>% nrow()  # For children, no missing data; good.
  
  # DMDEDUC2 - Education level - adults (20 or older)
  #   < HS,
  #   HS or some collage or
  #   Collage graduate or above
  #   7 refused, 9 don't know, "." missing.
  totals %>% filter(DMDEDUC2 == 7 | DMDEDUC2 == 9 | DMDEDUC2 == ".") %>% nrow() # no 7, 9, or ".".
  table(totals$DMDEDUC2, useNA = "ifany") # Has a lot of missing data, but because they are children.
  # Select people 20 yo or above, then see.
  totals %>% filter(RIDAGEYR >= 20) %>% filter(is.na(DMDEDUC2)) %>% nrow()  # For adults, no missing data; good.
  
# ---------------------------------------------------------------------------------------------------------------
# kcal
  summary(totals$KCAL)
  totals %>% filter(is.na(KCAL)) %>% nrow()    # no missing data.
  totals %>% filter( KCAL == ".") %>% nrow()   # no missing data.
  totals %>% filter( KCAL == " ") %>% nrow()   # no missing data.
  totals %>% filter( KCAL == "") %>% nrow()    # no missing data.
  
  summary(totals$KCAL)
  hist(totals$KCAL)
  head(totals[order(totals$KCAL, decreasing = T), "KCAL"])
  
  nrow(totals) # Should be 3642.
  
  naniar::vis_miss(totals[, vars])

# Looks good....  
# This new totals has 3642 rows and has  no missing data in the response and demographics variables of interest.     
  write.table(totals, "PF/Waist2/Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3642.txt", sep="\t", row.names = F, quote=F) 
  # n=3642 is after QC-ing with males and females separately.  
  
# DONE! 5/19.
# DONE! 6/28 after QC-ing males females separately.
  
  
# ---------------------------------------------------------------------------------------------------------------
  
# Diets 
  table(totals$DRQSDIET, useNA="ifany")

  
  
