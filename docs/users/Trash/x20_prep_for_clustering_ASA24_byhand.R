# ===============================================================================================================
# Prep ASA24 data by hand for clustering. 
# Made a function to do this (PrepForClustering), but keeping the script for record.
# Version 1
# Created on 12/12/2022 by Rie Sadohara
# ===============================================================================================================

# Here, we will prepare ASA24 totals data for PCA and clustering analyses.  
# We will need to calculate average dietary data per person across all days (if desired), 
# remove variables that have zero variance, and collapse variables by correlation
# (i.e. remove redundancy of variables that are highly correlated).


# Set your working directory as to the main directory.
Session --> Set working directory --> Choose directory.
setwd("~/GitHub/DietR")
# Name your main directory for future use. 
main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
source("lib/specify_data_dir.R")
source("lib/prep_data_for_clustering.R")

# You can come back to the main directory by:
setwd(main_wd)   

# ===============================================================================================================
# Import data and prepare them for analyses.
# ===============================================================================================================

# Specify the directory where the data is.
SpecifyDataDirectory(directory.name= "eg_data/VVKAJ/")

# ===============================================================================================================
# NUTRIENTS: Take average of each user across all days 
# ===============================================================================================================

# AverageBy is aldready done in the load_clean ASA24.R. So, no need to do that again. 
# Just load the QC-ed averaged totals.....

# Load the QC-ed totals that has one data/participant with metadata.
  VVKAJ_Tot_mean_m_QCed <- read.table("VVKAJ_Tot_mean_m_QCed.txt", sep="\t", header=T)  
  colnames(VVKAJ_Tot_mean_m_QCed)

# There may be some variables that you would like to omit before performing PCA.
# Define which columns to drop.
  drops <- c("FoodAmt", "KCAL", "MOIS")

# Take only the columns whose names are NOT in the drops vector. 
  VVKAJ_Tot_mean_m_QCed_2 <- VVKAJ_Tot_mean_m_QCed[ , !(names(VVKAJ_Tot_mean_m_QCed) %in% drops)]

# Obtain the column numbers for BMI, UserName, start.col="PROT" through end.col="P226".
  UserName_col <- match("UserName" , names(VVKAJ_Tot_mean_m_QCed_2)) 
  BMI_col   <-    match("BMI"      , names(VVKAJ_Tot_mean_m_QCed_2)) 
  start_col <-    match("PROT"     , names(VVKAJ_Tot_mean_m_QCed_2))  
  end_col   <-    match("B12_ADD"     , names(VVKAJ_Tot_mean_m_QCed_2)) 

# Pick up the BMI, body weight, and the nutrient variables.
  subsetted <- VVKAJ_Tot_mean_m_QCed_2[ , c(UserName_col, BMI_col, start_col:end_col)]

# Process the input, totals, for clustering analysis as follows. 
  # 1: take complete cases, 
  # 2: save that as a .txt, 
  # 3: keep non-zero columns, 
  # 4: remove the userID,
  # 5: identify correlated variables and remove them,
  # 6: save with uncorrelated variables as a .txt,
  # 7: save correlation matrix as a .txt.  
  
# An input dataset for PCA must have no missing data, so we need to check the number of
# missing data in each column of "subsetted" in the descending order.
  colSums(is.na(subsetted))[order(colSums(is.na(subsetted)), decreasing=T)]
  # There is no missing data in this example dataset, but the following steps to deal with missing data won't hurt.

# Take only the rows with no missing data.
  subsetted_c <- subsetted[complete.cases(subsetted), ]

# Take the rows of the original totals that are also present in "subsetted_c".
  VVKAJ_Tot_mean_m_QCed_c <- VVKAJ_Tot_mean_m_QCed[VVKAJ_Tot_mean_m_QCed$UserName %in% subsetted_c$UserName, ]

# Check that those two have exactly the same individuals (with complete data).
  identical(VVKAJ_Tot_mean_m_QCed_c$UserName,  subsetted_c$UserName)

# Save the new selected totals to be used as the original input later in the PCA script.
  write.table(VVKAJ_Tot_mean_m_QCed_c, "VVKAJ_Tot_mean_m_QCed_c_Nut_byhand.txt",
              sep="\t", row.names=F, quote=F)

# Remove the UserName column because it is the participants' ID and not appropriate to include in a PCA input.
  subsetted_c_wo_UserName = subsetted_c[, !names(subsetted_c) %in% "UserName"]

# This dataset, subsetted_c_wo_UserName with no missing data and no UesrName column, is the input for the
  # following preparation for PCA.


# Specify the data to be used, category to group by, and the range of columns (variables)
# to calculate the means of each variable.
  # For nutrients, specify start.col = "PROT",  and end.col = "B12_ADD"; 64 variables in total.
  AverageBy(data= totals, by= "UserName", start.col= "PROT", end.col= "B12_ADD")

# Save the averaged results.
  write.table(x=meansbycategorydf, "VVKAJ_Tot_m_QCed_Nut_ave_allvar_byhand.txt", sep="\t", row.names=F, quote=F)

# The column names should be the same as start.col-end.col.
  colnames(meansbycategorydf)

# The 'UserName' column has the users to calculate means for.
  meansbycategorydf$UserName

# Pick up only the columns with non-zero variance, in order to run PCA and cluster analysis etc.
# The removed columns will be shown if any.
# [,-1] is to exclude the UserName column that is not numeric and not used for variance calculation.
  KeepNonZeroVarColumns(data = meansbycategorydf[, -1])

# "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.

# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation.
  # Identify highly correlated variables.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var, min.cor = 0.75,
                                   select.rep.fcn = 'mean', verbose = T)

  # Filter out highly correlated variables from the original dataset.
  selected_variables <- subsetted_non0var[, cbc_res$reps]

  # ***NOTE: "selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***

  # Check the name of the original and filtered variables.
  # Among the variables in the same group, the one with the highest variance is kept
  #  (according to the explanation above.)
  # filtered
  head(selected_variables, 1)
  dim( selected_variables)

  # original
  head(subsetted_non0var, 1)
  dim( subsetted_non0var)

# ---------------------------------------------------------------------------------------------------------------
# Save the selected_variables as a .txt file. This will be the input for clustering analyses.
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Nut_ave_rv_byhand.txt", sep="\t", row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# "cc" is the correlation matrix produced when variables are collapsed by correlation by using
# the CollapseByCorrelation function.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Nut_ave_corr_matrix_byhand.txt")

  
  