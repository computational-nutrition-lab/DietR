# ===============================================================================================================
# Make food/nutrient intake table for each of the DivGroups.
# Version 1
# Created on 05/08/2023 by Rie Sadohara
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
  BMXWT (body weight)
  
# ===============================================================================================================
# KCAL intake
# ===============================================================================================================
# Missing data? -- No.  
  summary(df$KCAL)
  summary(df$BMXBMI)

# Create a density plot of BMI by DivGroup.
  BMIbox <- ggplot(data=df, aes(x=DivGroup, y=BMXBMI, fill=DivGroup)) +
    geom_boxplot( outlier.shape=16, outlier.alpha=0.5  ) + space_axes + no_grid +
    # scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
    labs(y="BMI", x=NULL)  
  BMIbox
  
  myanova <- aov(BMXBMI ~ DivGroup, data=df)
  summary(myanova)
  
  source("~/GitHub/R_Toolbox/anova_assumption_plots.R")
  ANOVA_assumption(input.anova = myanova, input.factor = "DivGroup", df=df)
  
  # Levene's test OK, but normality...? 
  shapiro.test(df$BMXBMI)
  
  hist(df$BMXBMI)
  
  pairwise.t.test(df$BMXBMI, df$DivGroup, p.adjust.method = "holm") 
  
  
  
  # log 
  
  
    
# ===============================================================================================================
# Demographics table
# ===============================================================================================================
  gender, age, race/ethnicity, Family IPR, Education

# ---------------------------------------------------------------------------------------------------------------
# Gender    
  gen <- table(df$RIAGENDR,df$`DivGroup` , useNA = "ifany")
  write.table(gen, "clipboard", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Age    
  age <- table(df$AgeGroup,df$`DivGroup` , useNA = "ifany")
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
  
  age <- table(df$age_3,df$`DivGroup` , useNA = "ifany")
  write.table(age, "clipboard", sep="\t", row.names=T, col.names = F, quote=F)
  
  
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
  eth <- table(df$RIDRETH3,df$`DivGroup` , useNA = "ifany")
  write.table(eth, "clipboard", sep="\t", row.names=T, col.names = F, quote=F)
  
  eth <- data.frame(table(df$RIDRETH3, df$`DivGroup` , useNA = "ifany")) # Make a long table.
  is(eth)
  colnames(eth) <- c("Ethnicity", "DivGroup", "value")

  ggplot(eth, aes(fill=Ethnicity, y=value, x=DivGroup)) + 
    geom_bar(position="fill", stat="identity")  + 
    # scale_fill_viridis_d() +
    scale_fill_viridis_d(labels = c("Mex Am", "Other Hispanic", "NH White", "NH Black", "NH Asian", "Other")) 
  
# ---------------------------------------------------------------------------------------------------------------
# Family IPR
# Need to know the number of family members, family income.
# https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references
# Poverty guideline in 2015: $11770 for first person, and $4160 for additional person.
# INDFMIN2 - Annual family income
# DMDFMSIZ - Total number of people in the Family
#   household: all individuals in the same address.
#   family:    all individuals in a household who are related by blood, marriage, or adoption.

# INDFMPIR - Ratio of family income to poverty
# 77 = Refused, 99= Don't know. So, we need to remove them. 
  df %>% filter(INDFMPIR == 77 | INDFMPIR == 99) %>% count() 
  summary(df$INDFMPIR)
  # No need, all values are 1-5. Need to remove 361 missing datapoints, though.
  
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
  write.table(povertyline, "clipboard", sep="\t", row.names=T, col.names = F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Education
  # DMDEDUC3 - Education level - Children/Youth 6-19
  # 0-12:  < HS, 
  # 13-15, 55, 66:  HS some college 
  # DMDEDUC2 - Education level - adults (20 or older)
  # < HS,
  # HS or some collage or
  # Collage graduate or above

  df$edu <- NA
  
  table(df$DMDEDUC2)  # there is no missing data
  table(df$DMDEDUC3)  # there is no missing data
  summary(df$RIDAGEYR)  # there is no missing data in age.
  
  for( i in 1:nrow(df) ){
    
    if(df$RIDAGEYR[i] <= 19 ){
      # for 18-19 yo.
      
      if(df$DMDEDUC3[i] <=12){
         df$edu[i] <-  "< HS" 
      }else if(df$DMDEDUC3[i] <=15 |df$DMDEDUC3[i] ==55 | df$DMDEDUC3[i] ==66 ){
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
  table(df$edu) # there is no missing data in age.
  df[10:100, c('DMDEDUC2','DMDEDUC3', 'edu')] 
  
  educ <- table(df$edu, df$`DivGroup` , useNA = "ifany")
  write.table(educ, "clipboard", sep="\t", row.names=T, col.names = F, quote=F)
  
    

  
    
# ---------------------------------------------------------------------------------------------------------------
# Download other questionnairs  
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PAQ_I.XPT", 
                destfile = "../../Raw_data/Physical_PAQ_I.XPT", mode="wb")
  

  
  
  
  