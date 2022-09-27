# ===============================================================================================================
# Visualize the mean values of %kcal from carbohydrate, protein, and total fat.
# Version 1
# Created on 12/16/2021 by Rie Sadohara
# ===============================================================================================================

# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")
  source("lib/percent_kcal.R")
  
# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# You can come back to the main directory by:
  setwd(main_wd)
  
# ===============================================================================================================
# Import data from your data directory 
# ===============================================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")
  
# Load the totals data.
  totals <- read.table("VVKAJ_Tot_m_QCed.txt",  sep = "\t", header = T)

# --------------------------------------------------------------------------------------------------------------
# Calculate the mean and SD of CARB, PROT, and TFAT.
  CPTgramsPerUser(inputfn= totals, user.name = "UserName", recall.no = "RecallNo",
                  outfn="VVKAJ_Tot_m_QCed_CPT_g.txt")
# Not used in the visualization below, but you may want to take a look at it.

 
# Calculate the mean % of energy intake (kcal) and SD of CARB, PROT, and TFAT.
  CPTpctKcalPerUser(inputfn=totals, user.name = "UserName", recall.no = "RecallNo", 
                   outfn="VVKAJ_Tot_m_QCed_CPT_kcal.txt")
 
# Load the %kcal values 
  CPT_kcal <- read.table("VVKAJ_Tot_m_QCed_CPT_kcal.txt", sep="\t", header=T)

# **NOTE** Do not alter the columnnames of CPT_kcal because the plotting functions below assume that 
# CPT_kcal has "UserName", "macronutrient", "n", "mean", and "sd" columns in it.

# --------------------------------------------------------------------------------------------------------------
# Plot a barchart without SD. 
  # Change the font size if necessary.
  stacked_wo_SD <- StackedwoSD(data= CPT_kcal) + theme(axis.text.x=element_text(size=11))  
  stacked_wo_SD
  
# Save as a .pdf.
  ggsave("VVKAJ_Tot_m_QCed_CPT_kcal_wo_SD.pdf", stacked_wo_SD, 
         device="pdf", width=6.2, height=4.2, units="in", dpi=300)

# --------------------------------------------------------------------------------------------------------------
 # Plot the "dodge"-type of barchart (3 bars per user, NOT STACKED).
  # Change the font size if necessary.
  dodgedtypebarchart <- DodgedBarchart(data= CPT_kcal) + theme(axis.text.x=element_text(size=11))  
  dodgedtypebarchart
  
  # Save as a .pdf.
  ggsave("VVKAJ_Tot_m_QCed_CPT_kcal_CPT_kcal_dodgedtypebarchart.pdf", dodgedtypebarchart,
         device="pdf", width=9.0, height=4, units="in", dpi=300)
 
# --------------------------------------------------------------------------------------------------------------
# Using CPT_kcal, create a stacked barchart.
  
# Create a vector that contains all the users (individuals). 
  individuals <- unique(CPT_kcal$UserName)

# Calculate sd_base and sd_forstacked for stacked barchart. 
# Note that this function assumes all users (individuals) have CARB, PROT, and TFAT values.
  CalcStackedSD(input.df= CPT_kcal, out.fn= "CPT_kcal_forstacked.txt")
  
# Load the saved file that has SD for stacked barchart.
  CPT_kcal_forstacked_read <- read.table("CPT_kcal_forstacked.txt", sep="\t", header=T)
  
# Stacked barchart with SD as error bars.
  stacked_with_SD <- StackedWithSD(data=CPT_kcal_forstacked_read) + theme(axis.text.x=element_text(size=11))
  stacked_with_SD
  
  # Save as a .pdf.
  ggsave("VVKAJ_Tot_m_QCed_CPT_kcal_CPT_kcal_with_SD.pdf", stacked_with_SD,
         device="pdf", width=6.2, height=4.3, units="in", dpi=300)
  
# Change the Y axis scale if necessary. Note that if the error bars of Carbohydrates disappear 
# after changing the limits of Y axis, it may be because the error bars are higher than the max Y.
# Ensure you have enough max Y value to accommodate the error bars.
  
# You can also change the breakpoints of the Y axis.
  stacked_with_SD + scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100))
  
# --------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)
  
  
  