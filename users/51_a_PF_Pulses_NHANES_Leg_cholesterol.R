# ===============================================================================================================
# Add cholesterol variable to totals with "LegGroup" variable.
# Version 1
# Created on 01/26/2023 by Rie Sadohara
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

  totals <- read.delim("../Total_D12_FC_QC_mean_QC_demo_ga_body_meta_Leg.txt")
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
  
# ---------------------------------------------------------------------------------------------------------------
# Download HDL cholesterol data. 
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HDL_I.XPT", 
                destfile= "Raw_data/HDL_I.XPT", mode="wb")
  
  HDLrawdata <- read.xport("Raw_data/HDL_I.XPT")
  
  head(HDLrawdata)
  plot(HDLrawdata$LBDHDD, HDLrawdata$LBDHDDSI)


# ---------------------------------------------------------------------------------------------------------------
# Download LDL cholesterol data. 
  # FYI: [LDL-cholesterol] = [total cholesterol] - [HDL-cholesterol] - [triglycerides/5]
  
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/TRIGLY_I.XPT", 
                destfile= "Raw_data/TRIGLY_I.XPT", mode="wb")

  TRIGLYrawdata <- read.xport("Raw_data/TRIGLY_I.XPT")
  
  head(TRIGLYrawdata)
  # LBXTR - Triglyceride (mg/dL)
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
  
  TCHOLrawdata <- read.xport("Raw_data/TCHOL_I.XPT")
  # LBXTC - Total Cholesterol (mg/dL)
  
  head(TCHOLrawdata)
  hist(TCHOLrawdata$LBXTC)
  summary(TCHOLrawdata$LBXTC)
  
# ---------------------------------------------------------------------------------------------------------------
# Add LBDHDD to totals.
  totals <- merge(totals, HDLrawdata[, c("SEQN", "LBDHDD")], by="SEQN", all.x=T)
  totals <- merge(totals, TRIGLYrawdata[, c("SEQN", "LBXTR", "LBDLDL")], by="SEQN", all.x=T)
  totals <- merge(totals, TCHOLrawdata[, c("SEQN", "LBXTC")], by="SEQN", all.x=T)
  
  colnames(totals)

  library(naniar)  
  vis_miss(totals[, c("SEQN", "LBDHDD", "LBXTR", "LBDLDL", "LBXTC")])
  # ~57% is missing LDL and TR!! 
  nrow(totals)
  totals_c <- totals[complete.cases(totals[, c("SEQN", "LBDHDD", "LBXTR", "LBDLDL", "LBXTC")]), ]
  nrow(totals_c)
  # 1791/4207 = 0.43. Hmm
  
  head(totals_c)  
  summary(totals_c$LBDHDD)  
  summary(totals_c$LBDLDL)  
  summary(totals_c$LBXTC)  
  summary(totals_c$LBXTR)  
  
  
  SummaryStats(inputdf=totals_c, outfn="SummaryStats_totals_c.txxt")
  
  
  boxplot(totals_c$LBDHDD)  # There's an outlier.
  hist(totals_c$LBDHDD)  # The outlier cannot be seen, but the X axis extends beyond 200. 
  max(totals_c$LBDHDD)  
  Q3 <- summary(totals_c$LBDHDD)[5]
  
  # Borderline value for outlier. 
  upper <- Q3 + IQR(totals_c$LBDHDD) *1.5  
  
  # How many rows are there that have LBDHDD value of above upper outlier threshold?
  nrow( subset(totals_c, LBDHDD > upper)  )
  # 38! hmmm... 
  
  # Let's remove just the one that is well above all the other data.
  which (totals_c$LBDHDD == max(totals_c$LBDHDD) )
  # 79 th row.
  
  # Exclude 79th row.
  totals_c_hdd <- totals_c[-79, ] 
  max(totals_c_hdd$LBDHDD)  
  # Now the max HDD is 129.
  hist(totals_c_hdd$LBDHDD) 
  # Better distribution.
  
  boxplot(totals_c_hdd$LBDLDL)  
  boxplot(totals_c_hdd$LBXTC)  
  boxplot(totals_c_hdd$LBXTR)  
  
# rows with complete cases of Total cholesterol, HDL, LDL, and tryglycerides,
# there were 1791 rows. 
# After removing one outlier in HDL (226 mg/dL),  there are 1790 rows.
  
  write.table(x=totals_c_hdd, "Total_D12_FC_QC_mean_QC_demo_ga_body_meta_Leg_cholesterol.txt",
              sep="\t", row.names=F, quote=F)
  
# Let's look at LegGroups
  table(totals_c_hdd$LegGroup)
  
  # Leg0 Leg1 Leg2 Leg3 
  # 1122  235  226  207 
  
  SummaryStats(inputdf=totals_c_hdd, outfn="SummaryStats_totals_c_hdd.txxt")
  
# Distribution of each group
  df <- totals_c_hdd
  
  plot(as.factor(df$LegGroup), df$LBDHDD) 
  plot(as.factor(df$LegGroup), df$LBXTR) 
  plot(as.factor(df$LegGroup), df$LBDLDL) 
  plot(as.factor(df$LegGroup), df$LBXTC) 
  
  myanova <- aov(LBXTC ~ LegGroup, data=df)
  summary(myanova)
  
  res1 <- residuals(myanova)
  hist(res1)
  qqnorm(res1, plot.it=TRUE)
  qqline(res1)
  boxplot(res1 ~ df$LegGroup)
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  anova(lm(res1sq ~ df$LegGroup))
  # If ANOVA is significant, you can do a pairwise t-test.
  pairwise.t.test(df$LBDHDD, df$LegGroup, p.adjust.method = "holm") 

  anova(lm(df$LBDHDD ~ df$LegGroup))   
  
## Tukey's Honestly Significant Difference. by Filipe. 
  library(agricolae)
  
# Define what to repeat first... 
# *** REPEAT ***
  # ANOVA table.
  summary(model)
  out <- HSD.test(model, "LegGroup", group=TRUE, console=TRUE)
  
  letters <- out$groups
  letters$LegGroup <- rownames(letters) 
  
  means <- out$means
  means$LegGroup <- rownames(means) 
  
  # Add letters to the means table.
  means_abc <- merge(means, letters[, c("LegGroup", "groups")], all.x = T, by="LegGroup")
  means_abc
# *** REPEAT ***

# Run the mean separation and lettering for each phenotype.  
  pheno
# LBDHDD
  model <- aov(LBDHDD ~ LegGroup, data=df)
  
  write.table(means_abc, "means_abs_LBDHDD_HDL.txt", sep="\t", row.names=F, quote=F)
  
# LBXTR
  model <- aov(LBXTR ~ LegGroup, data=df)
  
  ### *** REPEAT ***
  write.table(means_abc, "means_abs_LBXTR_tri.txt", sep="\t", row.names=F, quote=F)

# LBDLDL
  model <- aov(LBDLDL ~ LegGroup, data=df)
  
  ### *** REPEAT ***
  write.table(means_abc, "means_abs_LBDLDL_LDL.txt", sep="\t", row.names=F, quote=F)
  
#LBXTC - total cholesterol.
  model <- aov(LBXTC ~ LegGroup, data=df)
  
  ### *** REPEAT ***
  write.table(means_abc, "means_abs_LBXTC_TotalCho.txt", sep="\t", row.names=F, quote=F)
  
        