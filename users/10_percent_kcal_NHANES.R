# ==============================================================================================================
# Visualize the mean values of %kcal from carbohydrate, protein, and total fat. - sort them by value
# Added to NHANES tutorial 11/21/2022
# Version 2
# Created on 09/07/2022 by Rie Sadohara
# ==============================================================================================================

# Set working directory to "dietary_patterns".
  Session --> Set working direHctory --> Choose directory.  
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Load the necessary functions.
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")
  source("lib/percent_kcal.R")

# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")
  
# You can come back to the main directory by:
  setwd(main_wd)
 
# --------------------------------------------------------------------------------------------------------------
# Load totals data with demographics.
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/")

# # Load the totals with demographic data.
#   totals <- read.table("Total_D12_FC_QC_mean_QC_d.txt",  sep = "\t", header = T)
# 
# # Totals has the mean dietary intake of two days for each participant and also their metadata. 
# # We are going to use the following columns in totals:
# # RIAGENDR = gender
# # RIDAGEYR = age
# # CARB, PROT, TFAT, KCAL columns.
# 
# # --------------------------------------------------------------------------------------------------------------
# # Add gender and age_groups to totals. The output is named "totals_out".
#   AddGenderAgeGroups(input=totals, age.col="RIDAGEYR", gender.col="RIAGENDR")
#   
# # Ensure grouping has been done correctly. 
#   head(totals_out[, c("RIAGENDR", "Gender", "RIDAGEYR", "AgeGroup", "Gender_Age")])
# 
# # Also you want to look at the frequency of the groups. As expected, people aged 18-19 are less frequent.
#   table(totals_out$Gender_Age)  
#   table(totals_out$AgeGroup)  
#   
# # Rename the output as totals to use in the following code. 
#   totals <- totals_out
  
# --------------------------------------------------------------------------------------------------------------
# we will calculate the percentage of calories from each of the three macronutrients in the sum of 
# calories from the three macronutrients.
# Thus, the percentage of calories from CARB, PROT, and TFAT will add up to 100.   

  ### use new totals
  totals <- read.table("~/GitHub/dietarry_patterns/eg_data/NHANES/Laboratory_data/QCtotal_d_ga_body_meta_glu_comp_2.txt",  sep = "\t", header = T)
  table(totals$Gender_Age)

# Calculate the %kcal of CARB, PROT, and TFAT for each user and take means by Gender_Age.   
  CPTpctKcalPerUser_NHANES(inputfn=totals, group='Gender_Age', across='SEQN', 
                           outfn="QCtotal_d_ga_body_meta_glu_comp_2_CPT_kcal.txt")
  
# Load the output.
  CPT_kcal <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_CPT_kcal.txt", sep="\t", header=T)

# CPT_kcal has Group, macronutrient, n, mean, and sd of each group.
  head(CPT_kcal)

  
# --------------------------------------------------------------------------------------------------------------
# Plot a barchart without SD.
# Order Gender_Age by a certain macronutrient by the "order.by" argument. 
# You can also specify the stacking order of all the macronutrients by the "macronu.order" argument. 
# Note that the last item will be on the bottom of the barchart.
  PlotStackedwoSD(data=CPT_kcal, 
                  order.by = "Protein", 
                  macronut.order=c("Carbohydrate", "Total Fat", "Protein"))
  
# The chart is saved as "stacked_wo_SD".  
  stacked_wo_SD
  
# Save as a .pdf.
  ggsave("Total_D12_FC_QC_mean_QC_d_CPT_kcal_wo_SD.pdf", stacked_wo_SD,
         device="pdf", width=6.2, height=4.2, units="in", dpi=300)

# Or you can plot Gender_Age in the alphabetical order by setting order.by="NULL".
  PlotStackedwoSD(data=CPT_kcal, 
                  order.by = "NULL", 
                  macronut.order=c("Carbohydrate", "Total Fat", "Protein")) 
  stacked_wo_SD
  
# --------------------------------------------------------------------------------------------------------------
# Plot the "dodge"-type of barchart (3 bars per group, NOT STACKED).
# Order Diets by a certain macronutrient by the "order.by" argument. You can also specify the plotting order
# of all the macronutrients by the "macronu.order" argument. Note that the first item will be the leftmost bar.

  PlotDodged(data= CPT_kcal, 
             order.by = "Protein", 
             macronut.order=c("Carbohydrate","Total Fat", "Protein"))
  
# The chart is saved as "dodged_w_SD".  
  dodged_w_SD
 
# Save it as a .pdf.
  ggsave("Total_D12_FC_QC_mean_QC_d_CPT_kcal_dodged_w_SD.pdf", dodged_w_SD,
         device="pdf", width=9.0, height=4, units="in", dpi=300)

# Similarly, you can plot Diets in the alphabetical order by setting order.by="NULL".  
  
# --------------------------------------------------------------------------------------------------------------
# Generate a stacked barchart with SD as error bars.
  
# Create a vector that contains all the group levels (Gender_Age, in this case). This "groups" vector will be
# used in the CalcStackedSD function within the PlotStackedWithSD function.
  
  groups <- unique(CPT_kcal$Group)

# Order Diet by a certain macronutrient by the "order.by" argument. You can also specify the stacking order of 
# all the macronutrients by the "macronu.order" argument. Note that the last item will be on the bottom of 
# the barchart.
  PlotStackedWithSD(data= CPT_kcal, 
                    order.by = "Protein", 
                    macronut.order=c("Carbohydrate", "Total Fat", "Protein"))
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
  