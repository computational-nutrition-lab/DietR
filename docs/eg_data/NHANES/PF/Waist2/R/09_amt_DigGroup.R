# ===============================================================================================================
# Waist2.
# Draw a boxplot of nuts/seeds/legumes amount and DivGroup 
# Version 1
# Created on 05/19/2023 by Rie Sadohara
# ===============================================================================================================
  library(ggplot2)

# Set your working directory to the main directory.
# Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist2")  

# ===============================================================================================================
# Load the amt table and totals with DivGroup 
# ===============================================================================================================

  colsum_s_2 <- read.delim("n3676_SEQN_4xxxxxx_amt.txt")
  head(colsum_s_2)
 
  totalsDiv <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3676_DivGroup.txt")
  totalsDiv[1:4, 1:4]
  totalsDiv$XSEQN <- paste("X", totalsDiv$SEQN, sep="")
  
# Add DivGroup to colsum_s_2 by XSEQN.
  colsum_s_2_div <- merge(colsum_s_2, totalsDiv, all.x = T, by="XSEQN")
  
# Check.
  table(colsum_s_2_div$DivGroup, useNA = "ifany")
  head(colsum_s_2_div$amt_ave)
  
# ---------------------------------------------------------------------------------------------------------------
# Plot the amount by DivGroup.  
  box <- 
  ggplot(data= colsum_s_2_div, aes(x=DivGroup, y= amt_ave, fill=DivGroup)) +
    geom_boxplot( outlier.shape=16, outlier.alpha=0.5  ) + space_axes + no_grid +
    # scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
    labs(y="Nuts/seeds/legumes (g/day)", x=NULL)
  box
  
  ggsave("n3676_SEQN_4xxxxxx_amt_ave_DivGroup.png", box,
         device="png", width=5.2, height=4.2, units="in")
  #   
  
# ---------------------------------------------------------------------------------------------------------------
  
