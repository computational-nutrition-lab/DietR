# ===============================================================================================================
# Add cholesterol variable to totals with "DivGroup" variable.
# Version 1
# Created on 02/09/2023 by Rie Sadohara
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

  totals <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup.txt")
  dim(totals)
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
# Download HDL cholesterol data.
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HDL_I.XPT", 
                destfile= "../Raw_data/HDL_I.XPT", mode="wb")
  
  HDLrawdata <- read.xport("../Raw_data/HDL_I.XPT")
  
  head(HDLrawdata)
  plot(HDLrawdata$LBDHDD, HDLrawdata$LBDHDDSI) # same, but different units.

# ---------------------------------------------------------------------------------------------------------------
# Download LDL cholesterol data. 
  # FYI: [LDL-cholesterol] = [total cholesterol] - [HDL-cholesterol] - [triglycerides/5]
  
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/TRIGLY_I.XPT", 
                destfile= "Raw_data/TRIGLY_I.XPT", mode="wb")

  TRIGLYrawdata <- read.xport("../Raw_data/TRIGLY_I.XPT")
  
  head(TRIGLYrawdata)
  # LBXTR  - Triglyceride (mg/dL)
  # LBDLDL - LDL-cholesterol (mg/dL)
  plot(TRIGLYrawdata$LBXTR, TRIGLYrawdata$LBDLDL)

# Look at LBXTR - Triglyceride (mg/dL)
  summary(TRIGLYrawdata$LBXTR)
  hist(TRIGLYrawdata$LBXTR)
  # This seems to have an outlier. 2141 mg/dL, where the mean is 106. 
  head( TRIGLYrawdata[order(TRIGLYrawdata$LBXTR, decreasing = T),  ] )
 
# Look at LBDLDL - LDL-cholesterol (mg/dL)
  summary(TRIGLYrawdata$LBDLDL)
  hist(TRIGLYrawdata$LBDLDL) # normal distribution. 
  head( TRIGLYrawdata[order(TRIGLYrawdata$LBDLDL, decreasing = T),  ] )

# ---------------------------------------------------------------------------------------------------------------
# Download Total cholesterol data. 
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/TCHOL_I.XPT", 
                destfile= "Raw_data/TCHOL_I.XPT", mode="wb")
  
  TCHOLrawdata <- read.xport("../Raw_data/TCHOL_I.XPT")
  # LBXTC - Total Cholesterol (mg/dL)
  
  head(TCHOLrawdata)
  hist(TCHOLrawdata$LBXTC)
  summary(TCHOLrawdata$LBXTC)
  head(TCHOLrawdata[order(TCHOLrawdata$LBXTC, decreasing = T),])
  # There are two rows that have 545 and 540.
  
# ---------------------------------------------------------------------------------------------------------------
# Add LBDHDD to totals.
  totals <- merge(totals, HDLrawdata[,    c("SEQN", "LBDHDD")], by="SEQN", all.x=T)
  totals <- merge(totals, TRIGLYrawdata[, c("SEQN", "LBXTR", "LBDLDL")], by="SEQN", all.x=T)
  totals <- merge(totals, TCHOLrawdata[,  c("SEQN", "LBXTC")], by="SEQN", all.x=T)
  
  colnames(totals)

  library(naniar)  
  vis_miss(totals[, c("SEQN", "LBDHDD", "LBXTR", "LBDLDL", "LBXTC", "BMXBMI", "KCAL")])
  # ~57% is missing LDL and TR!! 
  nrow(totals)
  # totals_c <- totals[complete.cases(totals[, c("SEQN", "LBDHDD", "LBXTR", "LBDLDL", "LBXTC")]), ]
  totals_c <- totals[complete.cases(totals[, c("SEQN", "LBDHDD", "LBXTR", "LBDLDL", "LBXTC", "BMXBMI", "KCAL")]), ]
  # totals_c <- totals[complete.cases(totals[, c("SEQN", "LBDHDD")]), ]
  nrow(totals_c)
  # 1777/4207 = 0.42
  # HDD has 4024 rows.

  head(totals_c)  
  summary(totals_c$LBDHDD)  
  summary(totals_c$LBDLDL)  
  summary(totals_c$LBXTC)  
  summary(totals_c$LBXTR)  
  
  SummaryStats(inputdf=totals_c, outfn="n1777/SummaryStats_totals_c_Div.txt")
  
  boxplot(totals_c$LBDHDD)  # There's an outlier.
  hist(totals_c$LBDHDD)  # The outlier cannot be seen, but the X axis extends beyond 200. 
  max(totals_c$LBDHDD)  
  Q3 <- summary(totals_c$LBDHDD)[5]
  Q3
  # Borderline value for outlier. 
  upper <- Q3 + IQR(totals_c$LBDHDD) *1.5  
  upper
  # How many rows are there that have LBDHDD value of above upper outlier threshold?
  nrow( subset(totals_c, LBDHDD > upper)  )
  # 36! hmmm... 
  
  # Let's remove just the one that is well above all the other data.
  which (totals_c$LBDHDD == max(totals_c$LBDHDD) )
  
  # Exclude 78th row.
  totals_c_hdd <- totals_c[-78, ] 
  max(totals_c_hdd$LBDHDD)  
  # Now the max HDD is 129.
  hist(totals_c_hdd$LBDHDD) 
  # Better distribution.
  nrow(totals_c_hdd)
  # Now there are 1776 rows.
  
  boxplot(totals_c_hdd$LBDLDL)  
  boxplot(totals_c_hdd$LBXTC)  
  boxplot(totals_c_hdd$LBXTR)  
  
# rows with complete cases of BMI, KCAL, Total cholesterol, HDL, LDL, and tryglycerides,
# there were 1777 rows. 
# After removing one outlier in HDL (226 mg/dL),  there are 1776 rows.
  
  write.table(x=totals_c_hdd, "n1777/Total_D12_FC_QC_mean_QC_demo_ga_body_meta_Div_cholesterol.txt",
              sep="\t", row.names=F, quote=F)
  
