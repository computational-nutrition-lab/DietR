# ===============================================================================================================
# Look at the PCA results in detail.
# Version 1
# Created on 07/15/2022 by Rie Sadohara
# ===============================================================================================================

# Set working directory to "dietary_patterns".
  Session --> Set working direHctory --> Choose directory.  
  setwd(main_wd)

# Load necessary functions.
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")
  
# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")
  
# You can come back to the main directory by:
  setwd(main_wd)] 


# ===============================================================================================================
# Nutrients results 
# ===============================================================================================================
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/PCA_Nut_asis")

# Load the PCA result
  pcares <- read.table("VVKAJ_Nut_asis_PCs.txt", sep="\t", header=T)
  head(pcares,1)

# Load the variance explained by each PC.
  PC_var_exp <- read.table("VVKAJ_Nut_asis_PC_var_explained.txt", sep="\t", header=T)
  head(PC_var_exp)
  colnames(PC_var_exp)

  
# ---------------------------------------------------------------------------------------------------------------
# Generate a PC1 x PC2 plot with the users colored by their diets.
  PC12_diet <- ggplot(pcares, aes(x=PC1, y=PC2, color=Diet, fill=Diet)) + 
    geom_point(size=3) +
    no_grid + space_axes +
    scale_fill_manual( values = distinct100colors) +
    scale_color_manual(values = distinct100colors) +
    scale_x_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of X.
    scale_y_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of Y.
    labs(x = paste("PC1 (", round(PC_var_exp[1,2]*100, 1), "%)", sep=""),  
         y = paste("PC2 (", round(PC_var_exp[2,2]*100, 1), "%)", sep="")) +
    theme(aspect.ratio = 1)
  PC12_diet
  
# Save as a .pdf.
  ggsave("VVKAJ_Nut_asis_PC12_diet.pdf", PC12_diet, 
         device="pdf", width=7, height=6.5)


# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)    
  
  
  
  