# ===============================================================================================================
# Prepare data for PCA and other cluster analysis.
# Version 1
# Created on 01/13/2022 by Rie Sadohara
# ===============================================================================================================

# Here, we will prepare ASA24 totals data for PCA and clustering analyses.  
# We will need to calculate average dietary data per person across all days (if desired), 
# remove variables that have zero variance, and collapse variables by correlation
# (i.e. remove redundancy of variables that are highly correlated).


# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.

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

# ASA24 data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the totals data:
  # totals <- read.table("Totals_to_use.txt", sep = "\t", header= T)
  totals <- read.table("VVKAJ_Tot_m_QCed.txt", sep= "\t", header= T)

# Load the items.txt
# items <- read.table("VVKAJ_Items_f_s_m.txt", quote = "", sep = "\t", header = T)

# ===============================================================================================================
# NUTRIENTS: Use data as is. 
# ===============================================================================================================

# Subset nutrients data.
  # The columns specified as start.col, end.col, and all columns in between will be selected.
  # For nutrients, specify start.col = "PROT",  and end.col = "B12_ADD"; 64 variables in total.
  SubsetColumns(data = totals, start.col = "PROT", end.col = "B12_ADD")  
  
  # Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  # The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  # "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
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
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Nut_asis.txt", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
  # cc is the correlation matrix produced when variables are collapsed by correlation by using 
  # the CollapseByCorrelation function.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Nut_asis_corr_matrix.txt")
  
  
# ===============================================================================================================
# NUTRIENTS: Take average of each user across all days 
# ===============================================================================================================
# Specify the data to be used, category to group by, and the range of columns (variables) 
# to calculate the means of each variable.
  # For nutrients, specify start.col = "PROT",  and end.col = "B12_ADD"; 64 variables in total.
  AverageBy(data= totals, by= "UserName", start.col= "PROT", end.col= "B12_ADD")
  
  # Save the averaged results.
  write.table(x=meansbycategorydf, "VVKAJ_Tot_m_QCed_Nut_ave_allvar.txt", sep="\t", row.names=F, quote=F)
  
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
# Collapse variables by correlation: take only one variables if they are highly correlated.
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
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Nut_ave_subset.txt", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# "cc" is the correlation matrix produced when variables are collapsed by correlation by using 
# the CollapseByCorrelation function.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Nut_ave_corr_matrix.txt")
  
  
# ===============================================================================================================
# FOOD CATEGORIES: Use data as is.
# ===============================================================================================================
# Subset food items data.
  # The columns specified as start.col, end.col, and all columns in between will be selected.
  # For food category items, specify start.col = "F_TOTAL", end.col = "A_DRINKS"; 37 variables in total.
  SubsetColumns(data = totals, start.col = "F_TOTAL", end.col = "A_DRINKS")  
  
  # Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  # The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  # "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
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
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Cat_asis.txt", sep="\t", row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
  # cc is the correlation matrix produced when variables are collapsed by correlation by using 
  # the CollapseByCorrelation function.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Cat_asis_corr_matrix.txt")
  

# ===============================================================================================================
# FOOD CATEGORIES: Take average of each user across all days
# ===============================================================================================================
# Specify the data to be used, category to group by, and the range of columns (variables) 
# to calculate the means of each variable.
  # For food category items, specify start.col = "F_TOTAL", end.col = "A_DRINKS"; 37 variables in total.
  AverageBy(data= totals, by= "UserName", start.col= "F_TOTAL", end.col= "A_DRINKS")
  
  # Save the averaged results.
  write.table(x=meansbycategorydf, "VVKAJ_Tot_m_QCed_Cat_ave_allvar.txt", sep="\t", row.names=F, quote=F)
  
  # The column names should be UserName + start.col-end.col. 
  colnames(meansbycategorydf)
  
  # The 'UserName' column has the users to calculate means for.
  meansbycategorydf$UserName
  
  # Pick up only the columns with non-zero variance, in order to run PCA and cluster analysis etc.
  # The removed columns will be shown if any.
  # [,-1] is to exclude the UserName columns that is not numeric and not used for variance calculation. 
  KeepNonZeroVarColumns(data = meansbycategorydf[, -1])
  
  # "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
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
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Cat_ave_subset.txt", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation by using 
# the CollapseByCorrelation function.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Cat_ave_corr_matrix.txt")
  
  
# ===============================================================================================================
# Come back to the main directory
  setwd(main_wd) 
  
