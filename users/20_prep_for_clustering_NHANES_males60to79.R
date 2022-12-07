# ===============================================================================================================
# Prepare NHANES males 60-79 years old data for PCA and other cluster analysis.
# Version 1
# Created on 12/01/2022 by Rie Sadohara
# ===============================================================================================================

# READY TO BE COPIED TO TUTORIAL **********************************

# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Load the necessary functions 
  source("lib/specify_data_dir.R")
  source("lib/prep_data_for_clustering.R")

# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Load the data and omit some variables if desired.
# ===============================================================================================================
  
# Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data/")

# Load the subsetted totals data.   
  totals_males60to79 <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79.txt", 
                             sep="\t", header=T)

# There should be 237 individuals (rows) .
  dim(totals_males60to79)

# Are BMI and body weight correlated? - Yes.
  plot(    totals_males60to79$BMXBMI, totals_males60to79$BMXWT)
  cor.test(totals_males60to79$BMXBMI, totals_males60to79$BMXWT)

# There may be some variables that you would like to omit before performing PCA.
# Define which columns to drop.
  drops <- c("KCAL","GRMS", "MOIS", "NoOfItems")

# Take only the columns whose names are NOT in the drop vector. 
  totals_males60to79_2 <- totals_males60to79[ , !(names(totals_males60to79) %in% drops)]

# ===============================================================================================================
# NUT: PCA with nutrients and body weight
# ===============================================================================================================
# Prepare PCA input dataset with nutrients data and BMI and body weight of the participants.

# Obtain the column numbers for start.col="PROT" through end.col="P226" plus "BMXBMI" and "BMXWT".
  start_col <- match("PROT"   , names(totals_males60to79_2))  
  end_col   <- match("P226"   , names(totals_males60to79_2)) 
  BMI_col   <- match("BMXBMI" , names(totals_males60to79_2)) 
  WT_col    <- match("BMXWT"  , names(totals_males60to79_2)) 
  
# Pick up the BMI, body weight, and the nutrient variables.
  subsetted <- totals_males60to79_2[ , c(BMI_col, WT_col, start_col:end_col)]
  
# Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
# The output is a df called "subsetted_non0var".
  
# Check the columns (variables) remained.
  colnames(subsetted_non0var)  
  
# Check the number of rows and columns - 237 x 63. 63 variables remained. 
  dim(subsetted_non0var)
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variable if they are highly correlated.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
                                   min.cor = 0.75, 
                                   select.rep.fcn = 'mean', verbose = T)
  
# Filter out highly correlated variables from the original dataset.  
  selected_variables <- subsetted_non0var[, cbc_res$reps]
  
# ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***

# Among the variables in the same group, the one with the highest variance is kept.
# Check the retained variables. 
  colnames(selected_variables) 
  
# Check the number of rows and columns in the "selected_variables" dataset - 237 x 37. 37 variables remained. 
  dim(selected_variables)     

# ---------------------------------------------------------------------------------------------------------------
# Save the variables after removing correlated variables
  write.table(selected_variables,
              "males60to79_QCtotal_d_ga_body_meta_glu_comp_2_Nut_rv.txt", 
              sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc,
                 out.fn= "males60to79_QCtotal_d_ga_body_meta_glu_comp_2_Nut_corr_mat.txt")
  
# ===============================================================================================================
# CAT: PCA with food category and body weight
# ===============================================================================================================
# Prepare PCA input dataset with food category data and BMI and body weight of the participants.
  
# Obtain the column numbers for start.col="F_CITMLB" through end.col="A_DRINKS" plus "BMXBMI" and "BMXWT".
  start_col <- match("F_CITMLB", names(totals_males60to79_2))  
  end_col   <- match("A_DRINKS", names(totals_males60to79_2))   
  BMI_col   <- match("BMXBMI"  , names(totals_males60to79_2)) 
  WT_col    <- match("BMXWT"   , names(totals_males60to79_2)) 
  
# Pick up BMI, weight, and food category variables.
  subsetted <- totals_males60to79_2[ , c(BMI_col, WT_col, start_col:end_col)]
  
# Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  # The output is a df called "subsetted_non0var".
  
# Check the columns (variables) remained.
  colnames(subsetted_non0var)  
  dim(subsetted_non0var) # - 37 variables remained. 
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variable if they are highly correlated.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
                                   min.cor = 0.75, 
                                   select.rep.fcn = 'mean', verbose = T)
  
# Filter out highly correlated variables from the original dataset.  
  selected_variables <- subsetted_non0var[, cbc_res$reps]

# ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***

# Check to see the name of the original and filtered variables. 
# Among the variables in the same group, the one with the highest variance is kept.
# Check the retained variables. 
  colnames(selected_variables) 
  
# Check the number of rows and columns in the "selected_variables" dataset - 237 x 30. 30 variables remained. 
  dim(selected_variables)     

# ---------------------------------------------------------------------------------------------------------------
# Save the variables after removing correlated variables
  write.table(selected_variables,
              "males60to79_QCtotal_d_ga_body_meta_glu_comp_2_Cat_rv.txt", 
              sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc,
                 out.fn = "males60to79_QCtotal_d_ga_body_meta_glu_comp_2_Cat_corr_mat.txt")

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)   
  
