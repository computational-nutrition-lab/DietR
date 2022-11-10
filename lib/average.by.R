# ========================================================================================
# Calculate average of totals.
# Copied from prep_data_for_clustering.R.
# Version 1
# Created on 11/10/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Replace special characters with "_".
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Average each participant: average by username.
# Need to take average per user in the totals because subsetted_non0var does not have UserName.

# Function to take average by a specified column.

AverageBy <- function(data, by, start.col, end.col, outfn){
  
  # Column Variables of "totals" as a dataframe.
  colvars <- names(data)
  # Get the first ID
  start_col_number <- match(start.col, colvars)
  # Get the last ID
  end_col_number <- match(end.col, colvars)
  # Subset the 'by' column and the columns in the start-end range.
  by_start_end <- data[, c(which(colnames(data)==by), start_col_number:end_col_number)]
  
  # Define which variables to take means. and a list to save results
  myvar <- colnames(by_start_end)[-1]
  # Define the category entries (username in this case) to calculate means for.
  category_by <- unique(by_start_end[, 1]) 
  # Create a dataframe with the rownames corresponding to the category entries to calculate means for. 
  meansbycategorydf_1 <- data.frame(row.names = category_by)
  
  # Calculate means for each colname in myvar. 
  for(i in 1:length(myvar)){
    # mymean <- tapply(X = df[, i+1], INDEX = as.factor(df[, 1]), FUN = mean )
    resarray <- tapply(X = by_start_end[, i+1], INDEX = as.factor(by_start_end[, 1]), FUN = mean)
    resdf <- data.frame(resarray)   # save the results (array) as a dataframe.
    colnames(resdf) <- myvar[i]     # name the column with the variable.
    meansbycategorydf_1 <- cbind(meansbycategorydf_1, resdf)   # add the new resdf to the existing result meansbycategorydf_1.
  }
  
  # Create a column that has the usernames.
  meansbycategorydf_1$UserName <- rownames(meansbycategorydf_1)
  
  # Bring the rownames to the first column.
  # Take out the rownames (usernames), which is in the last column. ["var_name"] take it out as a dataframe. 
  meansbycategorydf_1_usernames <- meansbycategorydf_1["UserName"]
  
  # Take out the rest of the columns: i.e. take all the columns except the last one (usernames).  
  meansbycategorydf_1_var <- meansbycategorydf_1[, -ncol(meansbycategorydf_1)]
  
  # Combine meansbycategorydf_usernames and meansbycategorydf_var.
  meansbycategorydf <- cbind(meansbycategorydf_1_usernames, meansbycategorydf_1_var)
  
  # Save meansbycategorydf as a .txt file.
  write.table(x=meansbycategorydf, outfn, sep="\t", row.names=F, quote=F)
  
}
# ---------------------------------------------------------------------------------------------------------------

  
