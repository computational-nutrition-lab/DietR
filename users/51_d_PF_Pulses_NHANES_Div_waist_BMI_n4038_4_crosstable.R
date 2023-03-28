# ===============================================================================================================
# The purpose of this script.
# Version X
# Created on MM/DD/YYYY by xxxxxxxx
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

# Let's look at the Groups
  table(totals_c_wa$DivGroup, useNA = "ifany")

# DivNA  Div0  Div1  Div2 
# 2012  1246   387   393 

# ---------------------------------------------------------------------------------------------------------------
# Distribution of each group
  df <- totals_c_wa

  table(df$RIAGENDR, useNA = "ifany") # 1 is male.
  
# Cross tables of Gender and KCAL.
  source("~/GitHub/R_Toolbox/twoway_table.R")
  
  TwoWayTableWithTotalsTxt(data=df, var1 = "RIAGENDR", var2 = "DivGroup", sort.by.var2.total = T,
                           out.txt.fn = "RIAGENDER_DivGroup.txt")  
  twoway_table

  library(dplyr)

  # male 
  mkcal <- df %>% filter(RIAGENDR == 1) %>% group_by(DivGroup) %>% summarise(MeanKCAL = mean(KCAL))   
  colnames(mkcal)[2] <- "Male"
  
  # female
  fkcal <- df %>% filter(RIAGENDR == 2) %>% group_by(DivGroup) %>% summarise(MeanKCAL = mean(KCAL))   
  colnames(fkcal)[2] <- "Female"

  KCAL_Gen <- full_join(mkcal, fkcal, by="DivGroup")
  
  # DivGroup  Male Female
  # 1 DivNA    2199.  1699.
  # 2 Div0     2300.  1783.
  # 3 Div1     2426.  1874.
  # 4 Div2     2497.  1945.

  write.table(KCAL_Gen, "clipboard", sep="\t", row.names = F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------

# Cross tables of PROT and KCAL.    
  
  cor.test(df$PF_TOTAL_LEG, df$KCAL)
  plot(df$PF_TOTAL_LEG, df$KCAL)
  plot(df$PF_TOTAL_LEG, df$KCAL)
  boxplot(df$PF_TOTAL_LEG)
  
  summary(df$PF_TOTAL_LEG)
  
  df$protg <- 
    ifelse(df$PF_TOTAL_LEG < 5, 
           "L", 
           ifelse(df$PF_TOTAL_LEG < 10, 
                  "M",
                  "H"))
  table(df$protg, useNA = "ifany")  

# prot low 
  protlkcal <- df %>% filter(protg == "L") %>% group_by(DivGroup) %>% summarise(MeanKCAL = mean(KCAL))   
  colnames(protlkcal)[2] <- "Low_0_5"
  
# prot middle 
  protmkcal <- df %>% filter(protg == "M") %>% group_by(DivGroup) %>% summarise(MeanKCAL = mean(KCAL))   
  colnames(protmkcal)[2] <- "Mid_5_10"
  
# prot high 
  prothkcal <- df %>% filter(protg == "H") %>% group_by(DivGroup) %>% summarise(MeanKCAL = mean(KCAL))   
  colnames(prothkcal)[2] <- "High_10"

  KCAL_protg <- full_join(protlkcal, protmkcal, by="DivGroup")
  KCAL_protg <- full_join(KCAL_protg, prothkcal, by="DivGroup")
  KCAL_protg
  
  # DivGroup Low_0_5 Mid_5_10 High_10
  # 1 DivNA      1640.    2119.   2764.
  # 2 Div0       1610.    2055.   2627.
  # 3 Div1       1652.    2060.   2625.
  # 4 Div2       1723.    2081.   2652.
  
  write.table(KCAL_protg, "clipboard", sep="\t", row.names = F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
  
# Cross tables of FIBE and KCAL.    
  
  cor.test(df$FIBE, df$KCAL)
  plot(df$FIBE, df$KCAL)
  boxplot(df$FIBE)
  
  summary(df$FIBE)
  
  df$fibeg <- 
    ifelse(df$FIBE < 10, 
           "L", 
           ifelse(df$FIBE < 20, 
                  "M",
                  "H"))
  table(df$fibeg, useNA = "ifany")  
  
  # fibe low 
  fibelkcal <- df %>% filter(fibeg == "L") %>% group_by(DivGroup) %>% summarise(MeanKCAL = mean(KCAL))   
  colnames(fibelkcal)[2] <- "Low_0_10"
  
  # fibe middle 
  fibemkcal <- df %>% filter(fibeg == "M") %>% group_by(DivGroup) %>% summarise(MeanKCAL = mean(KCAL))   
  colnames(fibemkcal)[2] <- "Mid_10_15"
  
  # fibe high 
  fibehkcal <- df %>% filter(fibeg == "H") %>% group_by(DivGroup) %>% summarise(MeanKCAL = mean(KCAL))   
  colnames(fibehkcal)[2] <- "High_15"
  
  KCAL_fibeg <- full_join(fibelkcal, fibemkcal, by="DivGroup")
  KCAL_fibeg <- full_join(KCAL_fibeg, fibehkcal, by="DivGroup")
  KCAL_fibeg
  
  # DivGroup Low_0_10 Mid_10_15 High_15
  # 1 DivNA       1503.     2013.   2604.
  # 2 Div0        1376.     1885.   2481.
  # 3 Div1        1506.     1886.   2346.
  # 4 Div2        1447.     1766.   2382.
  
  write.table(KCAL_fibeg, "clipboard", sep="\t", row.names = F, quote=F)
  