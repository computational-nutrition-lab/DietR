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

# Load the totals data that have males 60-79 years old.   
  totals_males60to79 <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79.txt", 
                                   sep="\t", header=T)

# There should be 237 individuals (rows).
  dim(totals_males60to79)

# # Are BMI and body weight correlated? - Yes.
#   plot(    totals_males60to79$BMXBMI, totals_males60to79$BMXWT)
#   cor.test(totals_males60to79$BMXBMI, totals_males60to79$BMXWT)

# There may be some variables that you would like to omit before performing PCA.
# Define which columns to drop.
  drops <- c("KCAL","GRMS", "MOIS", "NoOfItems")

# Take only the columns whose names are NOT in the drops vector. 
  totals_males60to79_2 <- totals_males60to79[ , !(names(totals_males60to79) %in% drops)]

# ===============================================================================================================
# NUT: PCA with nutrients and body weight
# ===============================================================================================================
# Prepare PCA input dataset with nutrients data and BMI of the participants.
# We are interested in BMI because the GLU_index groups had different BMI. Add any other variables of interest
# that you would like to include in the PCA.

# Obtain the column numbers for start.col="PROT" through end.col="P226" plus "BMXBMI" and "SEQN".
  SEQN_col   <- match("SEQN"  , names(totals_males60to79_2)) 
  BMI_col   <- match("BMXBMI" , names(totals_males60to79_2)) 
  start_col <- match("PROT"   , names(totals_males60to79_2))  
  end_col   <- match("P226"   , names(totals_males60to79_2)) 
  # WT_col    <- match("BMXWT"  , names(totals_males60to79_2)) 
  
# Pick up the BMI, body weight, and the nutrient variables.
  user_BMI_nut <- totals_males60to79_2[ , c(SEQN_col, BMI_col, start_col:end_col)]

# Process this input, user_BMI_nut, for clustering analysis as follows. 
  # 1: take complete cases, 
  # 2: save that as a .txt, 
  # 3: keep non-zero columns, 
  # 4: remove the userID,
  # 5: identify correlated variables and remove them,
  # 6: save with uncorrelated variables as a .txt,
  # 7: save correlation matrix as a .txt.  

  PrepForClustering(input_df = user_BMI_nut,
                  userID = "SEQN",
                  original_totals_df= totals_males60to79, 
                  complete_cases_fn=   "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut_fn.txt",
                  clustering_input_fn= "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut_rv_fn.txt",
                  corr_matrix_fn=      "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut_corr_mat_fn.txt")

  
############# BY HAND ##################    
# # An input dataset for PCA must have no missing data.
# # Show the number of missing data in each column of "subsetted" in the descending order.
#   colSums(is.na(subsetted))[order(colSums(is.na(subsetted)), decreasing = T)]
#   # BMI has 3 missing data (NAs).
#   
#   # Take only the rows with no missing data. 
#   subsetted_c <- subsetted[complete.cases(subsetted), ]
#   
#   # Take the rows of the original totals that are also present in "subsetted_c".
#   totals_males60to79_c <- totals_males60to79[totals_males60to79$SEQN %in% subsetted_c$SEQN, ]
# 
# # Check that those two have exactly the same individuals (with complete data).
#   identical(totals_males60to79_c$SEQN,  subsetted_c$SEQN) 
#   
# # Save the new selected totals to be used as the original input later in the PCA script.
#   write.table(totals_males60to79_c, "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut.txt", 
#               sep="\t", row.names=F, quote=F)
#   
# # Remove the SEQN column because it is the participants' ID and not appropriate to include in a PCA input.
#   subsetted_c_wo_SEQN = subsetted_c[, !names(subsetted_c) %in% "SEQN"]
#   
# # This dataset, subsetted_c_wo_SEQN with no missing data and no SEQN column, is the input for the 
# # following preparation for PCA.
#     
# # ---------------------------------------------------------------------------------------------------------------
# # Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# # The removed columns will be shown if any.
#   KeepNonZeroVarColumns(data = subsetted_c_wo_SEQN)
# # The output is a df called "subsetted_non0var".
#   
# # Check the columns (variables) remained.
#   colnames(subsetted_non0var)  
#   
# # Check the number of rows and columns - 234 x 64. 64 variables remained. 
#   dim(subsetted_non0var)
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Collapse variables by correlation: take only one variable if they are highly correlated.
#   cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
#                                    min.cor = 0.75, 
#                                    select.rep.fcn = 'mean', verbose = T)
#   
# # Filter out highly correlated variables from the original dataset.  
#   selected_variables <- subsetted_non0var[, cbc_res$reps]
#   
# # ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Save the variables after removing correlated variables
#   write.table(selected_variables,
#               "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut_rv.txt", 
#               sep="\t", row.names=F, quote=F)
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Save the correlation matrix for record in the results folder.
# # cc is the correlation matrix produced when variables are collapsed by correlation. 
#   SaveCorrMatrix(x=cc,
#                  out.fn= "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut_corr_mat.txt")

# # Among the variables in the same group, the one with the highest variance is kept.
# # Check the retained variables.
#   colnames(selected_variables) 
#     
# # Check the number of rows and columns in the "selected_variables" dataset - 237 x 37. 37 variables remained. 
#   dim(selected_variables)     
  
############# BY HAND END ##################    

#  
  
# ===============================================================================================================
# CAT: PCA with food category and body weight
# ===============================================================================================================
# Do the same preparation for the food category data.
# Prepare PCA input dataset with nutrients data and BMI of the participants.
# We are interested in BMI because the GLU_index groups had different BMI. Add any other variables of interest
# that you would like to include in the PCA.
  
# Obtain the column numbers for start.col="F_CITMLB" through end.col="A_DRINKS" plus "BMXBMI" and "SEQN".
  SEQN_col  <- match("SEQN"     , names(totals_males60to79_2)) 
  BMI_col   <- match("BMXBMI"   , names(totals_males60to79_2)) 
  start_col <- match("F_CITMLB" , names(totals_males60to79_2))  
  end_col   <- match("A_DRINKS" , names(totals_males60to79_2)) 
  
# Pick up the BMI, body weight, and the nutrient variables.
  user_BMI_cat <- totals_males60to79_2[ , c(SEQN_col, BMI_col, start_col:end_col)]

# Prep 
  PrepForClustering(input_df = user_BMI_cat,
                    userID = "SEQN",
                    original_totals_df= totals_males60to79, 
                    complete_cases_fn=   "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat_fn.txt",
                    clustering_input_fn= "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat_rv_fn.txt",
                    corr_matrix_fn=      "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat_corr_mat_fn.txt")
  
    
# # An input dataset for PCA must have no missing data.
# # Show the number of missing data in each column of "subsetted" in the descending order.
#   colSums(is.na(subsetted))[order(colSums(is.na(subsetted)), decreasing = T)]
#   # BMI has 3 missing data (NAs).
#   
# # Take only the rows with no missing data. 
#   subsetted_c <- subsetted[complete.cases(subsetted), ]
#   
# # Take the rows of the original totals that are also present in "subsetted_c".
#   totals_males60to79_c <- totals_males60to79[totals_males60to79$SEQN %in% subsetted_c$SEQN, ]
#   
# # Check that those two have exactly the same individuals (with complete data).
#   identical(totals_males60to79_c$SEQN,  subsetted_c$SEQN) 
#   
# # Save the new selected totals to be used as the original input later in the PCA script.
#   write.table(totals_males60to79_c, "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat.txt", 
#               sep="\t", row.names=F, quote=F)
#   
# # Remove the SEQN column because it is the participants' ID and not appropriate to include in a PCA input.
#   subsetted_c_wo_SEQN = subsetted_c[, !names(subsetted_c) %in% "SEQN"]
#   
# # This dataset, subsetted_c_wo_SEQN with no missing data and no SEQN column, is the input for the 
# # following preparation for PCA.
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# # The removed columns will be shown if any.
#   KeepNonZeroVarColumns(data = subsetted_c_wo_SEQN)
#   # The output is a df called "subsetted_non0var".
#   
# # Check the columns (variables) remained.
#   colnames(subsetted_non0var)  
#   
# # Check the number of rows and columns - 234 x 38. 38 variables remained. 
#   dim(subsetted_non0var)
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Collapse variables by correlation: take only one variable if they are highly correlated.
#   cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
#                                    min.cor = 0.75, 
#                                    select.rep.fcn = 'mean', verbose = T)
#   
# # Filter out highly correlated variables from the original dataset.  
#   selected_variables <- subsetted_non0var[, cbc_res$reps]
#   
# # ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
#   
# # Among the variables in the same group, the one with the highest variance is kept.
# # Check the retained variables. 
#   colnames(selected_variables) 
#   
# # Check the number of rows and columns in the "selected_variables" dataset - 234 x 31. 31 variables remained. 
#   dim(selected_variables)     
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Save the variables after removing correlated variables
#   write.table(selected_variables,
#               "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat_rv.txt", 
#               sep="\t", row.names=F, quote=F)
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Save the correlation matrix for record in the results folder.
# # cc is the correlation matrix produced when variables are collapsed by correlation. 
#   SaveCorrMatrix(x=cc,
#                  out.fn= "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat_corr_mat.txt")
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)   
  
  
  
#### OLD BELOW, CAN BE DELETED. ####  
# # Prepare PCA input dataset with food category data and BMI and body weight of the participants.
#   
# # Obtain the column numbers for start.col="F_CITMLB" through end.col="A_DRINKS" plus "BMXBMI" and "BMXWT".
#   start_col <- match("F_CITMLB", names(totals_males60to79_2))  
#   end_col   <- match("A_DRINKS", names(totals_males60to79_2))   
#   BMI_col   <- match("BMXBMI"  , names(totals_males60to79_2)) 
#   WT_col    <- match("BMXWT"   , names(totals_males60to79_2)) 
#   
# # Pick up BMI, weight, and food category variables.
#   subsetted <- totals_males60to79_2[ , c(BMI_col, WT_col, start_col:end_col)]
# 
# ######
#   
# # Check for missing data, as PCA will not work if your data contains "NA".
#   colSums(is.na(subsetted))
# 
# # BMXBMI and BMXWT has 3 NAs, so the rows (participants) missing these data need to be removed.    
# # Take only the rows with no missing data. 
#   subsetted_c <- subsetted[complete.cases(subsetted), ]
#   
# # Check the number of participants and the number of variables - 234 people and 39 variables.
#   dim(subsetted_c)
#   
# ######
#     
# # Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# # The removed columns will be shown if any.
#   KeepNonZeroVarColumns(data = subsetted_c)
#   # The output is a df called "subsetted_non0var".
#   
# # Check the columns (variables) remained.
#   colnames(subsetted_non0var)  
#   dim(subsetted_non0var) # - 39 variables remained. 
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Collapse variables by correlation: take only one variable if they are highly correlated.
#   cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
#                                    min.cor = 0.75, 
#                                    select.rep.fcn = 'mean', verbose = T)
#   
# # Filter out highly correlated variables from the original dataset.  
#   selected_variables <- subsetted_non0var[, cbc_res$reps]
# 
# # ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
# 
# # Check to see the name of the original and filtered variables. 
# # Among the variables in the same group, the one with the highest variance is kept.
# # Check the retained variables. 
#   colnames(selected_variables) 
#   # BMXBMI was removed because it is highly correlated with BMXWT. 
#   
# # Check the number of rows and columns in the "selected_variables" dataset - 234 x 31. 31 variables remained. 
#   dim(selected_variables)     
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Save the variables after removing correlated variables
#   write.table(selected_variables,
#               "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_Cat_rv.txt", 
#               sep="\t", row.names=F, quote=F)
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Save the correlation matrix for record in the results folder.
# # cc is the correlation matrix produced when variables are collapsed by correlation. 
#   SaveCorrMatrix(x=cc,
#                  out.fn = "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_Cat_corr_mat.txt")
#  

