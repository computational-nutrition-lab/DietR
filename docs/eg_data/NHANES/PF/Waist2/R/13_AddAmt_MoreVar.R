# ===============================================================================================================
# Waist 2
# Add amt_ave of nuts/seeds/legumes consumption and Calculate PF_TOTAL_LEG, PF_LEG_perTOTAL, and total_MPFAT.
# Version 1
# Created on 05/25/2023 by Rie Sadohara
# Replaced "n3676" with "n3641" o on 06/28/2023 by Rie Sadohara
# Output as comments were updated. 
# ===============================================================================================================

# ===============================================================================================================
# Load data and packages.
# ===============================================================================================================

  library(dplyr)

# Set your working directory to the main directory.
# Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist2")  

# ---------------------------------------------------------------------------------------------------------------

# Load the totals data.
  # totals_c_wa <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3676_DivGroup_DemoCat.txt")
  totals_c_wa <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3641_DivGroup_DemoCat.txt")

  dim(totals_c_wa)
# should be 3641 rows, after removing rows containing missing data.

# Ensure there is no missing data.
  naniar::vis_miss(totals_c_wa[, c("SEQN","BMXWAIST","BMXBMI","RIDAGEYR", "RIAGENDR", "RIDRETH3", "INDFMPIR", 
                                   "DMDEDUC3", "DMDEDUC2", "KCAL")])

# Make the DivGroup as a factor.
  totals_c_wa$DivGroup <- factor(totals_c_wa$DivGroup, 
                                 levels = c('DivNA', 'Div0', 'Div1', 'Div2'))
# Make Gender as a factor.
  # table(totals_c_wa$Gender)
# totals_c_wa$Gender <- factor(totals_c_wa$Gender, 
# levels = c('F', 'M'))

# Let's look at the Groups
  table(totals_c_wa$DivGroup, useNA="ifany")

# DivNA  Div0  Div1  Div2 
# 1819  1105   360   357 

# Define df.
  df <- totals_c_wa

# ===============================================================================================================
# Add nuts/seeds/legumes amount. 
# ===============================================================================================================

  colsum_s_2 <- read.delim("n3641_SEQN_4xxxxxx_amt.txt")

# Make SEQN column.
  colsum_s_2$SEQN <- substring(colsum_s_2$XSEQN, 2) # start from the 2nd letter. 
  head(colsum_s_2)

# Add amt_ave.   ### This overwrites df ###
  df <- merge(df, colsum_s_2[c("SEQN", "amt_ave")], by="SEQN", all.x=T)
  head(df,1)
  summary(df$amt_ave) # amt_ave added, but has NAs because DivNA group consumed no n/s/l foods. 

  # Replace "NA" with zero for the DivMA group.
  # # Safety check
  # vector <-  df$amt_ave
  # vector[is.na(vector)] <- 0
  # vector
  # data.frame(vec=head(vector, 20), amt=head(df$amt_ave, 20))

  # Replace "NA" with zero for the DivMA group.
  df$amt_ave[is.na(df$amt_ave)] <- 0  ### THis overwrites df ###
  summary(df$amt_ave) 

# Check that DivNA individuals have zero 'amt_ave'. Good.  
  df[1:15, c("DivGroup", "amt_ave")]  

# ===============================================================================================================
# Calculate PF_TOTAL_LEG, PF_LEG_perTOTAL, and total_MPFAT
# ===============================================================================================================

# PF_TOTAL does not include PF_LEGUME, so add them to make PF_TOTAL_LEG.
  df$PF_TOTAL_LEG <- df$PF_TOTAL + df$PF_LEGUMES
  summary(df$PF_TOTAL_LEG)

# Calculate the percentage of PF_LEGUME in PF_TOTAL_LEG.
# (i.e. the percentage of legumes in all the protein food intake)
  df$PF_LEG_perTOTAL <- df$PF_LEGUMES / df$PF_TOTAL_LEG *100
  summary(df$PF_LEG_perTOTAL)

# The person who had zero protein intake had NaN PF_LEG_perTOTAL. 
  head(df[order(df$PF_TOTAL_LEG), c("PF_TOTAL_LEG", "PF_LEG_perTOTAL")])

# Calculate total polyunsaturate fat intake.
  df$total_MPFAT <- df$MFAT + df$PFAT
  summary(df$total_MPFAT)
  head(df$DivGroup, 10)
  df %>% group_by(DivGroup) %>% summarise(means= mean(total_MPFAT))

# Save the totals with DivGroup, amt, and PF_LEG_perTOTAL etc.
  write.table(df, 'Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3641_DivGroup_DemoCat_Amt_Var.txt',
              sep="\t", row.names = F, quote=F)
