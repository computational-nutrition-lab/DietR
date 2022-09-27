# ==============================================================================================================
# Visualize the mean values of %kcal from carbohydrate, protein, and total fat.
# Version 1
# Created on 09/07/2022 by Rie Sadohara
# ==============================================================================================================

# Set working directory to "dietary_patterns".
  Session --> Set working direHctory --> Choose directory.  
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")
  source("lib/percent_kcal.R")

# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")
  
# You can come back to the main directory by:
  setwd(main_wd)
 
# --------------------------------------------------------------------------------------------------------------
# Load example totals data.
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/")

# Load the totals with demographic data.
  totals <- read.table("Total_D12_FC_QC_mean_QC_d.txt",  sep = "\t", header = T)

# Totals has the mean dietary intake of two days for each participant and also their metadata. 
# We are going to use the following columns in totals:
# RIAGENDR = gender
# RIDAGEYR = age
# CARB, PROT, TFAT, KCAL columns.

# --------------------------------------------------------------------------------------------------------------
# Add gender and age_groups to totals. The out put is named "totals_out".
  AddGenderAgeGroups(input=totals, age.col="RIDAGEYR", gender.col="RIAGENDR")
  
# Ensure grouping has been done correctly. 
  head(totals_out[, c("RIAGENDR", "Gender", "RIDAGEYR", "AgeGroup", "Gender_Age")])
  
# Re-name the output as totals to use in the following code. 
  totals <- totals_out
  
# --------------------------------------------------------------------------------------------------------------
# For NHANES, we will calculate the percentage of calories from each of the three macronutrients in the sum of 
# calories from the three macronutrients.
# Thus, the percentage of calories from CARB, PROT, and TFAT will add up to 100.   

# Calculate the %kcal of CARB, PROT, and TFAT for each user and take means by Gender_Age.   
  CPTpctKcalPerUser_NHANES(inputfn=totals, group='Gender_Age', across='SEQN', 
                           outfn="Total_D12_FC_QC_mean_QC_d_CPT_kcal.txt")
  
# Load the output.
  CPT_kcal <- read.table("Total_D12_FC_QC_mean_QC_d_CPT_kcal.txt", sep="\t", header=T)

# CPT_kcal has Group, macronutrient, n, mean, and sd of each group.
  head(CPT_kcal)
  
# Plot a barchart without SD.
  # Change the font size if necessary.
  # This assumes that CPT_kcal has "Group" column.
  stacked_wo_SD <- StackedwoSD_NHANES(data= CPT_kcal) + theme(axis.text.x=element_text(size=11))  
  stacked_wo_SD

  # Save as a .pdf.
  ggsave("Total_D12_FC_QC_mean_QC_d_CPT_kcal_wo_SD.pdf", stacked_wo_SD,
         device="pdf", width=6.2, height=4.2, units="in", dpi=300)
  
# --------------------------------------------------------------------------------------------------------------
# Plot the "dodge"-type of barchart (3 bars per group, NOT STACKED).
  # Change the font size if necessary.
  dodgedtypebarchart <- DodgedBarchart_NHANES(data= CPT_kcal) + theme(axis.text.x=element_text(size=11))  
  dodgedtypebarchart
  
  # Save as a .pdf.
  ggsave("Total_D12_FC_QC_mean_QC_d_CPT_kcal_dodgedtypebarchart.pdf", dodgedtypebarchart,
         device="pdf", width=9.0, height=4, units="in", dpi=300)
  
# --------------------------------------------------------------------------------------------------------------
# Using CPT_kcal, create a stacked barchart.
  
  # Create a vector that contains all the groups. 
  groups <- unique(CPT_kcal$Group)
  
  # Calculate sd_base and sd_forstacked for stacked barchart. 
  # Note that this function assumes all groups have CARB, PROT, and TFAT values.
  CalcStackedSD_NHANES(input.df= CPT_kcal, out.fn= "Total_D12_FC_QC_mean_QC_d_CPT_kcal_forstacked.txt")
  
  # Load the saved file that has SD for stacked barchart.
  CPT_kcal_forstacked_read <- read.table("Total_D12_FC_QC_mean_QC_d_CPT_kcal_forstacked.txt", sep="\t", header=T)
  
  # Stacked barchart with SD as error bars.
  stacked_with_SD <- StackedWithSD_NHANES(data= CPT_kcal_forstacked_read) + theme(axis.text.x=element_text(size=11))
  stacked_with_SD
  
  # Save as a .pdf.
  ggsave("Total_D12_FC_QC_mean_QC_d_CPT_kcal_with_SD.pdf", stacked_with_SD,
         device="pdf", width=6.2, height=4.3, units="in", dpi=300)
  
# Change the Y axis scale if necessary. Note that if the error bars of Carbohydrates disappear 
# after changing the limits of Y axis, it may be because the error bars are higher than the max Y.
# Ensure you have enough max Y value to accommodate the error bars.
  
  # You can also change the breakpoints of the Y axis.
  stacked_with_SD + scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100))
  
# --------------------------------------------------------------------------------------------------------------
  # Come back to the main directory
  setwd(main_wd)
  