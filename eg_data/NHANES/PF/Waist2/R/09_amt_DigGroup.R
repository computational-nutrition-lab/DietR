# ===============================================================================================================
# Waist2.
# Draw a boxplot of nuts/seeds/legumes amount and DivGroup 
# Version 1
# Created on 05/19/2023 by Rie Sadohara
# Replaced "n3676" with "n3641" o on 06/28/2023 by Rie Sadohara
# Output as comments were updated. 
# ===============================================================================================================

# Set your working directory to the main directory.
# Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

  library(ggplot2)
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist2")  

# ===============================================================================================================
# Load the amt table and totals with DivGroup 
# ===============================================================================================================

  colsum_s_2 <- read.delim("n3641_SEQN_4xxxxxx_amt.txt")
  head(colsum_s_2) # has XSEQN, amt, amt_ave.
 
  totalsDiv <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3641_DivGroup.txt")
  totalsDiv[1:4, 1:4]
  totalsDiv[1:2, 263:264] # has totals data and DivGroup assignment.
  
# Merge amt, amt_ave, and totals, DivGorup.
  totalsDiv$XSEQN <- paste("X", totalsDiv$SEQN, sep="") 
  
# Add DivGroup to colsum_s_2 by XSEQN.
  colsum_s_2_totals_div <- merge(colsum_s_2, totalsDiv, all.x = T, by="XSEQN")
  colnames(colsum_s_2_totals_div)
  
# Check.
  table(colsum_s_2_totals_div$DivGroup, useNA = "ifany")
  head(colsum_s_2_totals_div$amt_ave)
  
# ---------------------------------------------------------------------------------------------------------------
# Plot the amount by DivGroup.  
  box <- 
  ggplot(data= colsum_s_2_totals_div, aes(x=DivGroup, y= amt_ave, fill=DivGroup)) +
    geom_boxplot( outlier.shape=16, outlier.alpha=0.5  ) + space_axes + no_grid +
    # scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
    labs(y="Nuts/seeds/legumes (g/day)", x=NULL)
  box
  
  ggsave("n3641_SEQN_4xxxxxx_amt_ave_DivGroup.png", box,
         device="png", width=5.2, height=4.2, units="in")
  #   
  
# ---------------------------------------------------------------------------------------------------------------
  
