# ===============================================================================================================
# Load NHANES 2015-16 FOOD data, add food description, QC, and calculate total. 
# Version 1
# Created on 10/05/2022 by Rie Sadohara and Suzie Hoops
# ===============================================================================================================

# So far, all the edits made to this script are reflected on the tutorial (12/12/2022). 

# Folder structure 
#
#               |----- eg_data -- NHANES -- Raw_data -- has Food Items and Totals files 
#               |                                       downloaded from NHANES 15-16.
#               |
#  Main --------|----- lib -- functions
#  (DietR)      |
#               |----- users -- where this script is located
#

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR/")

# Name your main directory for future use.
  main_wd <- file.path(getwd())
  
# Install the SASxport package if it is not installed yet.
  if (!require("SASxport", quietly = TRUE)) install.packages("SASxport")

# Load SASeport, necessary to import NHANES data.
  library(SASxport)

# Load necessary functions.
  source("lib/specify_data_dir.R")
  source("lib/load_clean_NHANES.R")
  source("lib/QCOutliers.R")
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  

# ===============================================================================================================
# QC the food data: filter by age, completeness, >1 food item reported/day, data exists on both days. 
# ===============================================================================================================
    
# Download demographics data (DEMO_I.XPT) from NHANES website.
# Name the file and destination. mod="wb" is needed for Windows OS.
# Other OS users may need to delete it.
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT", 
                destfile= "Raw_data/DEMO_I.XPT", mode="wb")
  
# Load the demographics file.
  demog <- read.xport("Raw_data/DEMO_I.XPT")

# Demographics data has the age, gender, etc. of each participant. Read documentation on demographics 
# (https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.htm) for details.     
  head(demog,1)

# Only take the data of adults (18 years of age or older).  
  adults <- demog[demog$RIDAGEYR >= 18, ]
  
# Check the number of adults - should be 5,992. 
  length(unique(adults$SEQN)) 

# Load the formatted food data prepared in the previous section.   
  Food_D1_FC_cc_f <- read.table("Food_D1_FC_cc_f.txt", sep="\t", header=T)
  Food_D2_FC_cc_f <- read.table("Food_D2_FC_cc_f.txt", sep="\t", header=T)
  
# Check the number of complete and incomplete data or each day. According to the documentation  
# (https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1IFF_I.htm), value 4 is incomplete, 
# so 2,208 rows are marked incomplete for Day 1, and 1,902 rows for Day 2.
  table(Food_D1_FC_cc_f$DR1DRSTZ)  # Day 1
  table(Food_D2_FC_cc_f$DR2DRSTZ)  # Day 2

# Subset only those with complete data (STZ==1).
  food1 <- subset(Food_D1_FC_cc_f, DR1DRSTZ == 1)
  food2 <- subset(Food_D2_FC_cc_f, DR2DRSTZ == 1)

# Create a vector of SEQN of those who reported data for both days and are adults.   
  food1names <- unique(food1$SEQN) # 8,326 adults
  food2names <- unique(food2$SEQN) # 6,875 adults
  keepnames <- food1names[food1names %in% food2names]  # 6,863
  keepnames_adults <- keepnames[keepnames %in% adults$SEQN] # 4,405

# Similarly, keep those who reported more than 1 food item per day.
  freqtable1 <- as.data.frame(table(food1$SEQN))
  freqtable1_m <- freqtable1[freqtable1$Freq > 1, ]
  colnames(freqtable1_m)[1] <- "SEQN"
  keepnames_adults_mult1 <- keepnames_adults[keepnames_adults %in% freqtable1_m$SEQN] # 4405
  
# Take only the participants whose names are in keepnames_adults_mult1.
  food1b <- food1[food1$SEQN %in% keepnames_adults_mult1, ] # 66,304 rows
  
# Do the same for food2.
  freqtable2 <- as.data.frame(table(food2$SEQN))
  freqtable2_m <- freqtable2[freqtable2$Freq > 1, ]
  colnames(freqtable2_m)[1] <- "SEQN"
  keepnames_adults_mult2 <- keepnames_adults[keepnames_adults %in% freqtable2_m$SEQN] # 4401
  food2b <- food2[food2$SEQN %in% keepnames_adults_mult2, ] # 66,690 rows

# Create a vector of SEQN of those that have both day 1 and day 2 data.
  food1bnames <- unique(food1b$SEQN)
  food2bnames <- unique(food2b$SEQN)
  keepnames12 <- food1bnames[food1bnames %in% food2bnames] 
  
# Check the number of participants remained. 
  length(keepnames12)
  # 4,401 people met the criteria.


# ================================================================================================================  
# From here, procedures A and B serve the following purposes. (B consists of B-1, B-2, B-3, and B-4.)
# Procedure A: Further process food1b and food2b for building a food tree.  
# Procedure B-1-4: Calculate totals from the QC-ed food items and QC totals.
# ================================================================================================================  
  
# ================================================================================================================  
# Procedure A: Further process food1b and food2b for building a food tree.  
# ================================================================================================================  
  
# Make a day variable to distinguish them.
  food1b$Day <- 1
  food2b$Day <- 2

# Copy these datasets to avoid overwriting.
  food1e <- food1b
  food2e <- food2b

# Remove the prefixes "DR1I", "DR1" from the columnnames.
  colnames(food1e) <- gsub(colnames(food1e), pattern = "^DR1I", replacement = "")
  colnames(food1e) <- gsub(colnames(food1e), pattern = "^DR1",  replacement = "")
  colnames(food2e) <- gsub(colnames(food2e), pattern = "^DR2I", replacement = "")
  colnames(food2e) <- gsub(colnames(food2e), pattern = "^DR2",  replacement = "")

# Ensure the columns of food1c and food2c match before joining them. 
# (returns TRUE if the two are identical)
  identical(colnames(food1e), colnames(food2e))
  
# Combine food1 and food2 as a longtable (add food2 rows after food1 rows).
  food12e <- rbind(food1e, food2e)
  
# Select only the individuals listed in keepnames12. --> This will further be QCed at the end of this script.
  food12f <- food12e[food12e$SEQN %in% keepnames12, ]
  
# These need to be removed from tutorial, I think..., because input for food tree and for 
# # Save as a .txt. It will be a HUGE file. Use this as an input for food tree.
#   write.table(food12f, "Food_D12_FC_cc_f_s.txt", sep="\t", row.names=F, quote=F)
# 
# # Add the demographic data to food12f for data overview.  
#   food12f_demo <- merge(x=food12f, y=demog, by="SEQN", all.x=T)
#   length(unique(food12f_demo$SEQN))
# 
# # Save. It will be a HUGE file  and will take a few moments. Use this as an input for data overview.
#   write.table(food12f_demo, "Food_D12_FC_cc_f_s_demo.txt", sep="\t", row.names=F, quote=F)
# until here. 
   
# ================================================================================================================  
# Procedure B-1: Further process food1b and food2b for calculating totals and clustering.  
# ================================================================================================================  

# Copy datasets to avoid overwriting
  food1bb <- food1b
  food2bb <- food2b
  
# Change "FoodAmt" back to "DR1GRMS" to be consistent with the variable names in dayXvariables
  names(food1bb)[names(food1bb) == "FoodAmt"] <- "DR1IGRMS"
  names(food2bb)[names(food2bb) == "FoodAmt"] <- "DR2IGRMS"

# [NOTE] Prepare data to combine day 1 and day 2 data.
# Refer to the tutorial how to generate NHANES_Food_VarNames_FC_Day1.txt and
# NHANES_Food_VarNames_FC_Day2.txt. 
  
# Day 1
  # Import the list of variables to be selected in Day 1.  
  day1variables <- read.table('Food_VarNames/NHANES_Food_VarNames_FC_Day1.txt', header=F)

  # Select the variables to pick up from the food data
  var_to_use1 <- names(food1bb) %in% day1variables$V1
  
  # Pick up only the specified variables
  food1c <- food1bb[, var_to_use1]
  
  # Remove "DR1T", "DR1" from the column names 
  colnames(food1c) <- gsub(colnames(food1c), pattern = "^DR1I", replacement = "")
  colnames(food1c) <- gsub(colnames(food1c), pattern = "^DR1",  replacement = "")
  
  # Check the column names.
  colnames(food1c)
 
# ---------------------------------------------------------------------------------------------------------------
# Do the same for Day 2  
  day2variables <- read.table('Food_VarNames/NHANES_Food_VarNames_FC_Day2.txt', header=F)
  var_to_use2 <- names(food2bb) %in% day2variables$V1
  food2c <- food2bb[, var_to_use2]
  colnames(food2c) <- gsub(colnames(food2c), pattern = "^DR2I", replacement = "")
  colnames(food2c) <- gsub(colnames(food2c), pattern = "^DR2", replacement = "")
  
# Make a day variable before combining food1c and food2c. 
  food1c$Day <- 1
  food2c$Day <- 2

# Ensure the columns of food1c and food2c match before joining them.
  identical(colnames(food1c), colnames(food2c))
  
    # If not, create a dataframe that has the column names of both food1c and food2c,
    # examine the column names side-by-side.
      names_df <- data.frame(matrix(nrow= max(ncol(food1c), ncol(food2c)), ncol=2))
      colnames(names_df) <- c("food1c", "food2c")
      names_df$food1c <- colnames(food1c)
      names_df$food2c <- colnames(food2c)
      names_df

# Combine food1 and food2 as a longtable.
  food12c <- rbind(food1c, food2c)
  
# Pick up only the individuals listed in keepnames12.
  food12d <- food12c[food12c$SEQN %in% keepnames12, ]

# Add the demographic data to food12f for data overview.   ### This will be QC-ed later at the end of this script.
  food12d_demo <- merge(x=food12d, y=demog, by="SEQN", all.x=T)
  
# Save the combined and QC-ed food items as a .txt file. 
# This has nutrient information, food categories, and day variable for each food item reported,
# and shall be used to calculate totals in B-2. 
#### THIS WILL BE A HUGE FILE. ####
  write.table(food12d_demo, "Food_D12_FC_QC_demo.txt", sep="\t", quote=F, row.names=F)  
  
# ---------------------------------------------------------------------------------------------------------------
# You may also want to consider special diets that some participants are following: e.g. DASH diet, diabetic diet, 
# etc. Depending on your research question, you may want to exclude those following special diets.
# The diet information is found in totals day 1. We will revisit diet information in the next section.

# ===============================================================================================================
# B-2: Calculate totals/day/participant with the food data of the selected SEQNs.
# ===============================================================================================================

# Load the QC-ed food items.
  food12d_demo <- read.table("Food_D12_FC_QC_demo.txt", sep="\t", header=T)

# Calculate totals for day 1 and day 2, and combine the two datasets.
  TotalNHANES(food12d= food12d_demo,
              first.val= "GRMS", last.val= "A_DRINKS", 
              outfn = "Total_D12_FC_QC_eachday.txt")  

# Load the resultant total for each day.
  total12d <- read.table("Total_D12_FC_QC_eachday.txt", sep="\t", header=T)

# total12d has the sum of each variable (columns) for each day and participant. 
  head(total12d, 1)
  
# Merge total12d and demographics by SEQN.
  total12d_demo <- merge(x= total12d, y=demog, by="SEQN", all.x=TRUE)
  
# Save it as a .txt file. 
  write.table(total12d_demo, "Total_D12_FC_QC_eachday_demo.txt", sep="\t", quote=F, row.names=F)

# ===============================================================================================================
# B-3: Calculate the mean across days totals/participant.  
# ===============================================================================================================

# Calculate the mean of the two days of the totals data per participant. 
  AverageTotalNHANES(total12d = total12d, 
                     first.val= "GRMS", last.val= "NoOfItems", 
                     outfn= "Total_D12_FC_QC_mean.txt")  
  
# Load the mean total.
  meantotal12d <- read.table("Total_D12_FC_QC_mean.txt", sep="\t", header=T)


# ===============================================================================================================
# B-4: QC the mean total in the same way as ASA24. 
# ===============================================================================================================
  
# For individual food data, there is no code for cleaning.
# Outliers won't severely affect main analysis conclusions (ASA24 data cleaning doc).
# However, it is always a good idea to take a look at the distributions of variables of interest. 

# ---------------------------------------------------------------------------------------------------------------
# For totals, the same QC can be applied as ASA24 totals QC procedure.
# Totals data may contain outliers due to errors in dietary reporting. These errors may be due to omission or 
# inaccurate over- or under-estimation of portion size, leading to improbable nutrient totals. ASA24 provides 
# General Guidelines for Reviewing & Cleaning Data 
# (https://epi.grants.cancer.gov/asa24/resources/cleaning.html#guidelines) for identifying and removing 
# suspicious records. 

# Here, we will identify records that contain values that fall outside typically observed ranges of 
# kilocalories (KCAL), protein (PROT), total fat (TFAT), and vitamin C (VC). The ASA24 guide provides ranges 
# of beta carotene (BCAR), too, however, outlier checking for BCAR is omitted in this tutorial but can be 
# considered if you identify it as a nutrient that has a high variance in your study dataset.

# Please note that your input dataframe (QCtotals) will be overwritten after each outlier removal.  

# Run all these QC steps in this order. When asked, choose to remove the outliers
# that fall outside the specified range for each nutrient.

# Define the input data.  This dataframe will be modified after each filter.
  QCtotals <- meantotal12d
  
# Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "KCAL", min = 600, max = 5700)
  
# This function will print out rows that fall outside the specified min-max range, and a dialogue box 
# will appear outside the R Studio, asking whether to remove them. You should make sure to review these records 
# carefully to double-check if the removal is warranted. It is possible to have a valid record that could 
# meet the threshold for removal. Only you will know if you can trust the record when working with real data.

# If you find potential outlier(s) here, click "No", and view those 
# total(s) with their other nutrient intake information by running the following;
  KCAL_outliers <- subset(QCtotals, KCAL < 600 | KCAL > 5700)     
# Sort the rows by KCAL and show only the specified variables.
  KCAL_outliers[order(KCAL_outliers$KCAL, decreasing = T),
                c('SEQN', 'KCAL', 'GRMS', 'PROT', 'TFAT', 'CARB')]  
# If you think they are true outliers, then run the QCOutliers command for KCAL again, and click "Yes" to 
# remove the outlier. Here for this tutorial, we will remove this individual.
  
# Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "PROT", min = 10, max = 240)
  
# Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "TFAT", min = 15, max = 230)

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals,  
             target.colname = "VC", min = 5, max = 400)

# Look at how many rows (observations) were kept after QC.
# If you have removed all the detected outliers in the above order, there should be 4207 rows. 
  nrow(QCtotals)

# ---------------------------------------------------------------------------------------------------------------
# Add demographic data to the QC-ed total. 

# Merge QC-totals and demographics by SEQN.
  QCtotals_demo <- merge(x= QCtotals, y=demog, by="SEQN", all.x=TRUE)

# Save QCtotal_demo as a .txt file. 
  write.table(QCtotals_demo, "Total_D12_FC_QC_mean_QC_demo.txt", sep="\t", quote=F, row.names=F)
  
# Now, you have prepared formatted and QC-ed food items, totals for each day, and mean totals across two days 
# for each participant.
  
 
# ===============================================================================================================
# Remove the QC-ed individual(s) from the totals each day.
# ===============================================================================================================
  
# In the previous section, we have removed individual(s) that did not pass the QC from mean total data.
# We will remove those individual(s) from the totals (before taking means of days), so that we will have
# the same individuals in mean_total and total. 
  
# Among the individuals in total12d_demo, pick up only those in QCtotals_demo. 
  total12d_demo_QCed <- total12d_demo[total12d_demo$SEQN %in% QCtotals_demo$SEQN, ]
  
# Save as a .txt file. This will be the total for each of the "QC-ed" individuals for each day, if you would
# like to perform clustering analyses with Day 1 and Day 2 separately. 
  write.table(total12d_demo_QCed, "Total_D12_FC_QC_eachday_demo_QCed.txt", sep="\t", quote=F, row.names=F)

# ===============================================================================================================
# Similarly, remove the QC-ed individual(s) from the items to be consistent
# ===============================================================================================================
  
# Among the individuals in food12d_demo, pick up only those in QCtotals_demo. 
  food12d_demo_QCed <- food12d_demo[ food12d_demo$SEQN %in% QCtotals_demo$SEQN, ]
  
# Save as a .txt file. This will be the items for each of the "QC-ed" individuals for each day, to be 
# used for ordination etc. 
  write.table(food12d_demo_QCed, "Food_D12_FC_QC_demo_QCed.txt", sep="\t", quote=F, row.names=F)
    
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)  
  
