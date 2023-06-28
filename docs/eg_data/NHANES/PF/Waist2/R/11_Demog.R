# ===============================================================================================================
# Demographic info of the DivGroups.
# Version 1
# Created on 05/24/2023 by Rie Sadohara
# Replaced "n3676" with "n3641" o on 06/28/2023 by Rie Sadohara
# Output as comments were updated.
# ===============================================================================================================

# ===============================================================================================================
# Load data.
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
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist2/")  
  
# Demographic data variables expalanation: 
  # https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.htm#DMDEDUC2

# ===============================================================================================================
# Load the prepared data with the DivGroup variable.
# ===============================================================================================================

# Load the data.
  totals_c_wa <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3641_DivGroup.txt")

  dim(totals_c_wa)
# should be 3641 rows, after removing rows containing missing data.

# Ensure there is no missing data.
  naniar::vis_miss(totals_c_wa[, c("SEQN","BMXWAIST","BMXBMI","RIDAGEYR", "RIAGENDR", "RIDRETH3", "INDFMPIR", 
                                   "DMDEDUC3", "DMDEDUC2", "KCAL")])

# Make the DivGroup as a factor.
  totals_c_wa$DivGroup <- factor(totals_c_wa$DivGroup, 
                               levels = c('DivNA', 'Div0', 'Div1', 'Div2'))
# Make Gender as a factor.
  table(totals_c_wa$Gender, useNA="ifany")
# totals_c_wa$Gender <- factor(totals_c_wa$Gender, 
# levels = c('F', 'M'))

# Let's look at the Groups
  table(totals_c_wa$DivGroup, useNA="ifany")

# DivNA  Div0  Div1  Div2 
#  1819  1105   360   357

# Distribution of each group
  df <- totals_c_wa

# ===============================================================================================================
# Demographics table
# ===============================================================================================================
  # gender, age, race/ethnicity, Family IPR, Education

# ---------------------------------------------------------------------------------------------------------------
# Gender    
  gen <- table(df$RIAGENDR, df$`DivGroup` , useNA = "ifany")
  gen
  write.table(gen, "clipboard", sep="\t", row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Age    
  age <- table(df$AgeGroup, df$`DivGroup` , useNA = "ifany")
  write.table(age, "clipboard", sep="\t", row.names=T, col.names = F, quote=F)

# OR group into 3 groups: 18-39, 40-59, 60-80+
  summary(df$RIDAGEYR)

  df$age_3 <- NA
  for(i in 1:nrow(df)){
    
    if(df$RIDAGEYR[i] <= 39 ){
      df$age_3[i] <- "18-39"
    }else if(df$RIDAGEYR[i] <= 59){
      df$age_3[i] <- "40-59"
    }else{
      df$age_3[i] <- "60+"
    }
  }
# Check.
  table(df$AgeGroup, df$`age_3` , useNA = "ifany")

  age <- table(df$age_3, df$`DivGroup` , useNA = "ifany")  
  write.table(age, "clipboard", sep="\t", row.names=T, col.names = F, quote=F)
  rbind(gen, age)

# ---------------------------------------------------------------------------------------------------------------
# Race/Ethnicity. RIDRETH3
# Code or Value	| Value Description	| Count	| Cumulative	
# 1	Mexican American	1921	1921	
# 2	Other Hispanic	1308	3229	
# 3	Non-Hispanic White	3066	6295	
# 4	Non-Hispanic Black	2129	8424	
# 6	Non-Hispanic Asian	1042	9466	
# 7	Other Race - Including Multi-Racial	505	9971
# .	Missing	0	9971
  eth <- table(df$RIDRETH3, df$`DivGroup` , useNA = "ifany")
  eth
  
# Want to combine 1.Mexican American and 2.Other Hispanic.
  df$eth_5 <- NA
  for(i in 1:nrow(df)){
    
    if(df$RIDRETH3[i] <= 2 ){
      df$eth_5[i] <- "1or2"
    }else{
      df$eth_5[i] <- df$RIDRETH3[i]
    }
  }
  
  # Check.
  table(df$RIDRETH3, df$`eth_5` , useNA = "ifany")
  
  # Make freqtable again.
  eth2 <- table(df$eth_5, df$`DivGroup` , useNA = "ifany")
  eth2
  # DivNA Div0 Div1 Div2
  # 1or2   493  383   99  108
  # 3      665  395  135  128
  # 4      471  192   46   33
  # 6      113   92   70   82
  # 7       77   43   10    6
  
  write.table(eth2, "clipboard", sep="\t", row.names=T, col.names = F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# stacked barchart
  eth3 <- data.frame(table(df$eth_5, df$`DivGroup` , useNA = "ifany")) # Make a long table.
  is(eth3)
  colnames(eth3) <- c("Ethnicity", "DivGroup", "value")

  ggplot(eth3, aes(fill=Ethnicity, y=value, x=DivGroup)) + 
    geom_bar(position="fill", stat="identity")  + 
    # scale_fill_viridis_d() 
    scale_fill_viridis_d(labels = c("Mex Am, Hispanic", "NH White", "NH Black", "NH Asian", "Other"))

# ---------------------------------------------------------------------------------------------------------------
# Family IPR
# https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references
# Poverty guideline in 2015: $11770 for first person, and $4160 for additional person.
# INDFMIN2 - Annual family income
# DMDFMSIZ - Total number of people in the Family
#   household: all individuals in the same address.
#   family:    all individuals in a household who are related by blood, marriage, or adoption.

# INDFMPIR - Ratio of family income to poverty
# 77 = Refused, 99= Don't know. So, we need to remove them.
  library(dplyr)
  df %>% filter(INDFMPIR == 77 | INDFMPIR == 99) %>% count() 
  summary(df$INDFMPIR)
# No need, all values are 1-5. 

#  <1.85 , 1.85-2.99, >=3.00
  df$FIPL <-   # Family income poverty line
    ifelse(df$INDFMPIR < 1.85, 
           '<1.85',
           ifelse(df$INDFMPIR < 3.00,
                  '1.85-2.99', 
                  '>= 3.00')
    )
# Check the ranges are correct.
  df %>% group_by(FIPL) %>% summarise(min=min(INDFMPIR), max=max(INDFMPIR))

  povertyline <- table(df$FIPL, df$`DivGroup` , useNA = "ifany")
  write.table(povertyline[c(1,3,2), ], "clipboard", sep="\t", row.names=T, col.names = F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Education
# DMDEDUC3 - Education level - Children/Youth 6-19
# 0-12:  < HS, 
# 13-15, 55, 66:  HS some college 
  
# DMDEDUC2 - Education level - adults (20 or older)
#   1	Less than 9th grade	688	688	
#   2	9-11th grade (Includes 12th grade with no diploma)	676	1364	
#   3	High school graduate/GED or equivalent	1236	2600	
#   4	Some college or AA degree	1692	4292	
#   5	College graduate or above	1422	5714	
#   7	Refused	0	5714	
#   9	Don't Know	5	5719	
# .	Missing	4252	9971

# Let's group into: 
# 1-2: < HS,
# 3-4: HS or some collage or
# 5: Collage graduate or above
# 7,9,.: missing 
  
  df$edu <- NA

  table(df$DMDEDUC2, useNA = "ifany")  # missing data in adults' edu level is because they are kids.
  table(df$DMDEDUC3, useNA = "ifany")  # missing data in kids' edu level is because they are adults.
  summary(df$RIDAGEYR)  # there is no missing data in age.

  for( i in 1:nrow(df) ){
    
    if(df$RIDAGEYR[i] <= 19 ){
      # for 18-19 yo.
      
      if(df$DMDEDUC3[i] <=12){
        df$edu[i] <-  "< HS" 
      }else if(df$DMDEDUC3[i] <=15 | df$DMDEDUC3[i] ==55 | df$DMDEDUC3[i] ==66 ){
        df$edu[i] <-  "HS grad or some collage" # "More than HS" is included in this category, for simplicity... 
      }else{
        df$edu[i] <-  NA  # refused, missing, or don't know.
      }
      
    }else{  # adults: DMDEDUC2 
      
      if(df$DMDEDUC2 [i] <= 2){
        df$edu[i] <-  "< HS" 
      }else if(df$DMDEDUC2[i] <= 4 ){
        df$edu[i] <-  "HS grad or some collage" 
      }else if(df$DMDEDUC2[i] == 5){
        df$edu[i] <- "Collage grad or above" 
      }else{
        df$edu[i] <-  NA  # refused, missing, or don't know.
      }
    }  
  }

# Check.
table(df$edu, useNA = 'ifany') # there is no missing data in age.
  df[10:100, c('DMDEDUC2','DMDEDUC3', 'edu')] 

  educ <- table(df$edu, df$`DivGroup` , useNA = "ifany")
  educ
  write.table(educ[c(1,3,2), ], "clipboard", sep="\t", row.names=T, col.names = F, quote=F)


# ---------------------------------------------------------------------------------------------------------------
# Now, df has new categorical variables such as "age_3", "eth_5", "FIPL", and "edu".
# Save this as a new total.
  tail( colnames(df))
  dim( df)
  # 3641 x 267
  write.table(df, "Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3641_DivGroup_DemoCat.txt", 
              sep="\t", row.names=F, quote=F)
  

# ---------------------------------------------------------------------------------------------------------------
# Read data
  df <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3641_DivGroup_DemoCat.txt")
  dim(df)

# Make DivGroup as a factor.
  df$DivGroup <- factor(df$DivGroup, 
                        levels = c('DivNA', 'Div0', 'Div1', 'Div2'))
  
  tail(colnames(df))

# Chi-square test for DivGroup and categorical variable levels...
  table(df$age_3, useNA = 'ifany')
  
# Gender
  chisq.test(df$RIAGENDR, df$DivGroup)
  # p-value = 0.2831  # This changed a lot before and after QC-ing males and females separately. 
  
# Age group  
  chisq.test(df$age_3, df$DivGroup)
  # p-value = 0.008878
      
# Ethnicity group  
  chisq.test(df$eth_5, df$DivGroup)
  # p-value = 2.2e-16
  
# FIPL group  
  chisq.test(df$FIPL, df$DivGroup)
  # p-value = 5.456e-16
  
# edu group  
  chisq.test(df$edu, df$DivGroup)
  # p-value = 2.2e-16
       
   
# P-trend
  subsetted <- subset(df, age_3== "18-39")
  table(subsetted$age_3)
  table(subsetted$age_3, subsetted$DivGroup)

  subsetted[1:10, c('age_3', 'DivGroup')]
  
# p-difference
# e.g. Within DivNA, the proportion of ethnicity is different or not?
# But I cannot test it because this is just counts and there is no SD... 
  subsetted <- subset(df, eth_5 == "1or2")
  table(subsetted$DivGroup)
  table(subsetted$eth_5, subsetted$DivGroup)
  
  
  