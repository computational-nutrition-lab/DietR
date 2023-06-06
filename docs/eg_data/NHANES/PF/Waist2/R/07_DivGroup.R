# ===============================================================================================================
# Waist 2
# Compute DivGroups from the QC-ed OTU table.
# Version 1
# Created on 05/19/2023 by Rie Sadohara
# ===============================================================================================================

  
  # Set your working directory to the main directory.
  # Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")
  source("lib/specify_data_dir.R")
  source("lib/diversity_nth_tile.R")
  
  # Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
  # Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist2")  

  library(vegan)

# ===============================================================================================================
# Load QC-ed OTU table.
# ===============================================================================================================

  otu_QCed <- read.delim("Foodtree/Food_D12_FC_QC_demo_QCed_n3676_4s_3Lv.food.otu.txt")

  otu_QCed[1:4, 1:4]

# ===============================================================================================================
# Code from users/NHANES_16_Div.R.
# Calculate diversity of 4xxxxxxxs for each SEQN
# ===============================================================================================================
  
  # Take out the foodID (description) and taxonomy from otu.
  otu2 <- otu_QCed[, 2: (ncol(otu_QCed)-1) ]
  
  # transpose so that the SEQN will come to rows.   
  otu2t <- as.data.frame(t(otu2)) 
  
  # Add taxonomy as the column names of otu2t. 
  colnames(otu2t) <- otu$X.FOODID
  
  # Each row of otu2t is SEQN. So, diversity needs to be calculated per each row.
  
  # Make a table to save results. 
  SEQNdiv <- as.data.frame(matrix(nrow = nrow(otu2t) , ncol = 4))
  colnames(SEQNdiv) <- c("SEQN", "Shannon", "Simpson", "Invsimpson")
  
# Do a loop to calculate Shannon's, Simpson, and inverse-Simpson diversity  for all SEQNs (in rows).
# This may take a few minutes.
  
  for( i in 1: nrow(otu2t) ){
    SEQNdiv[i, 1] <- rownames(otu2t)[i]
    SEQNdiv[i, 2] <- diversity(otu2t[i, ], 'shannon')
    SEQNdiv[i, 3] <- diversity(otu2t[i, ], 'simpson')
    SEQNdiv[i, 4] <- diversity(otu2t[i, ], 'invsimpson')
  } 
  
  head(SEQNdiv)
  
  # There should be no NA values. 
  table(is.na(SEQNdiv), useNA="always")  
  
  # Plot histograms of each of the diversity measures.
  par(mfrow = c(2, 2))
  hist(SEQNdiv$Shannon, main="Shannon diversity", xlab="", breaks=15)
  hist(SEQNdiv$Simpson, main="Simpson diversity", xlab="", breaks=15)
  hist(SEQNdiv$Invsimpson, main="Invsimpson diversity", xlab="", breaks=15)
  par(mfrow = c(1, 1))
  
  # Some have 0 diversity --> If a row has only one non-zero values, then diversity will be zero. e.g. c(20,0,0,0,0,0,0) 
  # i.e., those only consumed one nuts/seeds/legumes food have diversity of zero.
  # None of them are normally distributed because of a lot of zero diversity values.
  # For demonstration purposes, we will use Shannon's diversity. 
  
# ===============================================================================================================
# Divide NHANES participants into four groups based on their diversity in nuts/seeds/legumes consumption 
# ===============================================================================================================
  
  # Our goal is to mark samples as:
  # DivNA ... Did not report any foods with Food ID of 4xxxxxxxx. Shannon's diversity = NA.
  # Div0  ... Reported 1 food with Food ID of 4xxxxxxx.           Shannon's diversity = 0. 
  # Div1  ... Reported >1 foods with Food ID of 4xxxxxxx. lower.  Shannon's diversity > 1. 
  # Div2  ... Reported >1 foods with Food ID of 4xxxxxxx. upper.  Shannon's diversity > 1. 
  
  # Remove the "X" in the SEQNdiv$SEQN for merging. 
  SEQNdiv$SEQN <- gsub(SEQNdiv$SEQN, pattern = "X", replacement = "") 
  
# Load totals without the outlier (SEQN==92254).
  totals <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3676.txt")
  
# First, need to add the diversity values to totals. Only take the rows present in both datasets.
  totals_div <- merge(totals, SEQNdiv, by='SEQN')
  
# ---------------------------------------------------------------------------------------------------------------
# Select individuals whose diversity score is > 0, and group them into groups lower and upper (2-tiles) 
# based on their Shannon's diversity measure.
  DivNthTile(input= totals_div, div.var="Shannon", nth.tile=2)
  
  # Define Div1 and Div2. 
  out$DivGroup <- 
    ifelse(
      out$Div == 1,
      out$DivGroup <- 'Div1',
      out$DivGroup <- 'Div2'
    )
  
  # Select only the SEQN and DivGroup.
  SEQN_Div12 <- out[, c("SEQN", "DivGroup")]
  
  # Define Div0. ----------------------------------------
  # Subset those that have Shannon index =0.
  totals_div_zero <- subset(totals_div, Shannon == 0)
  
  # Add DivGroup variable, and insert "Div0".
  totals_div_zero$DivGroup <- 'Div0'
  
  # Select only the SEQN and DivGroup.
  SEQN_Div0 <- totals_div_zero[, c("SEQN", "DivGroup")]
  
  # Define DivNA. ----------------------------------------
  # Define "Not in" function.  By default it's not existent.
  `%!in%` <- Negate(`%in%`)
  
  # Subset those that are not in SEQNdiv.
  # those are the ones that did not consume nuts/seeds/legumes.
  totals_not_in_SEQNdiv <- totals[totals$SEQN %!in% SEQNdiv$SEQN, ]  
  
  # Add DivGroup variable, and insert "DivNA".
  totals_not_in_SEQNdiv$DivGroup <- 'DivNA'
  
  # Take only the SEQN and DivGroup.
  SEQN_DivNA <- totals_not_in_SEQNdiv[, c("SEQN", "DivGroup")]
  
# ---------------------------------------------------------------------------------------------------------------
# Combine SEQN_DivNA, SEQN_Div0, and SEQN_Div12 for merging.
  SEQN_Div_NA_012 <- rbind(SEQN_DivNA, SEQN_Div0, SEQN_Div12)
  
  # Now, all the SEQNs have DivGroups. 
  
  # Check that this should have the same number of rows as totals does. 
  identical(length(unique(SEQN_Div_NA_012$SEQN)), nrow(totals))
  
  # Merge DivGroups with the totals.
  totals_divgroup <- merge(totals, SEQN_Div_NA_012, all.x=T, by="SEQN")
  
  # Change DivGroup into a factor and specify the factor levels.
  totals_divgroup$DivGroup <- factor(totals_divgroup$DivGroup, 
                                     levels = c('DivNA', 'Div0', 'Div1', 'Div2') )
  
  # The individuals in totals were grouped into 4 groups depending on their consumption of 
  # 4xxxxxxx foods (or the lack thereof). The totals_divgroup has DivGroup column. 
  table(totals_divgroup$DivGroup, useNA = "ifany")
  
  # DivNA  Div0  Div1  Div2 
  # 1840  1114   361   361 

# Save the totals with DivGroup.
  write.table(totals_divgroup, "Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3676_DivGroup.txt",
              sep="\t", row.names=F, quote=F)

  
    
# Health outcomes and demographic data could be explored by DivGroup. 
  

