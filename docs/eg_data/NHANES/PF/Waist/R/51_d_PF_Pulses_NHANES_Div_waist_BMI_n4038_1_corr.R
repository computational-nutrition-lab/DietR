# ===============================================================================================================
# Create a nice corr matrix for preprint.
# Version 1
# Created on 03/24/2023 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  source("lib/specify_data_dir.R")

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF")  
# ===============================================================================================================
# Load data with no missing data in BMI and waist circumference.
# ===============================================================================================================

  df <- read.delim('Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup_waistBMI.txt')
  dim(df)
  # Should be 4038 x 266. 
  tail(df)


# ===============================================================================================================
# Define Corplot with PAIR package with code from Amber
# ===============================================================================================================

# Diagonal correlation table.  Useful for lots of datasets with multiple variables.
  library(lattice)

# Correlation after removing NAs.  Choose spearman or pearson.
# Corr coeff in the lower panel ---------------------------------------------
  panel.cor <- function(x, y){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y, method="pearson", use="pairwise.complete.obs"), digits=2)  # Pearson = liniar, Spearman = rank-order.
    test <- cor.test(x,y) 
    
    
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                     symbols = c(  "***", "**",  "*", " ")) 
    txt <- paste0(r)
    cex.cor <- .8 /strwidth(txt)
    # text(0.5, 0.5, txt, cex = 2.5 * sqrt(abs(r)))  
    text(0.5, 0.5, txt, cex = 5 * sqrt(abs(r)))     # For 7x7 plot saved as 900x900 px.
    # text(0.8, 0.8, Signif, cex = 1.3 * sqrt(abs(r)))
    text(0.8, 0.8, Signif, cex = 3 * sqrt(abs(r)))  # For 7x7 plot saved as 900x900 px.
  }

# Customize upper panel ----------------------------------------------------
  upper.panel<-function(x, y){
    # points(x,y, pch = 19, col = "#A9D18E")
    panel.smooth(x, y, bg = "gray70", pch = 21, lwd = 2, cex.axis = 1.2,   # datapoints
                 cex = 1.2, col.smooth = "black", span = 2/3, iter = 3)   # line
  }


# Subset only the color values in 2 years, 2 env.
  colnames(df)
  # pheno = df[, c("BMXWAIST", "BMXBMI", "RIAGENDR", "RIDAGEYR", "FIBE", "PF_TOTAL_LEG", "KCAL")]
  pheno = df[, c("BMXWAIST", "BMXBMI", "KCAL", "RIAGENDR", "RIDAGEYR", "FIBE", "PF_TOTAL_LEG")]
  colnames(pheno) <- c("Waist", "BMI", "KCAL", "Gender", "Age", "FIBER", "PF_ALL")
  head(pheno)

# Change the colors of upper.panel function: datapoints color = black,  line color = #FF6347
# Create the plots
  pairs(pheno, cex.labels = 2.3,  # 1.5 is good for smaller plots like 4x4... 
        lower.panel = panel.cor,
        upper.panel = upper.panel)

# Export as .png or something...
# C:\Users\sadoh\OneDrive\Documents\GitHub\DietR\eg_data\NHANES\PF\Waist2\Corr




# ---------------------------------------------------------------------------------------------------------------
