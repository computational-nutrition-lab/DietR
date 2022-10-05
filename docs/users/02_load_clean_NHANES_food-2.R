# ===============================================================================================================
# Load NHANES 2015-16 FOOD data, add food description, QC, and calculate total. 
# Version 1
# Created on 10/05/2022 by Rie Sadohara and Suzie Hoops
# ===============================================================================================================

# Folder structure 
# 
#                          |----- eg_data -- NHANES -- Raw_data -- has Food Items and Totals files 
#                          |                                       downloaded from NHANES 15-16.
#                          |
#  Main -------------------|----- lib -- functions
#  (dietary_patterns)      |
#                          |----- users -- where this script is located
#

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use.
  main_wd <- file.path(getwd())
  
# Install the SASxport package if it is not installed yet.
  if (!require("SASxport", quietly = TRUE)) install.packages("SASxport")

# Load SASeport, necessary to import NHANES data.
  library(SASxport)

# Load necessary functions.
  source("lib/load_clean_NHANES.R")
  source("lib/load_clean_ASA24.R")
  
# You can come back to the main directory by:
  setwd(main_wd)


# ===============================================================================================================
# QC the food data: filter by age, completeness, >1 food item reported/day, data exists on both days. 
# ===============================================================================================================
    
# Download demographics data (DEMO_I.XPT) from NHANES website.
# Name the file and destination. mod="wb" is needed for Windows OS.
# Other OS users may need to delete it.
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT", 
                destfile= "eg_data/NHANES/DR1IFF_I.XPT", mode="wb")
  
# Load the demographics file.
  demog <- read.xport("eg_data/NHANES/Raw_data/DEMO_I.XPT")

# Demographics data has the age, gender, etc. of each participant. Read documentation on demographics 
# (https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.htm) for details.     
  head(demog,1)

# Only take the data of adults (18 years of age or older).  
  adults <- demog[demog$RIDAGEYR >= 18, ]
  
# Check the number of adults - should be 5,992. 
  length(unique(adults$SEQN)) 

# Load the formatted food data prepared in the previous section.   
  Food_D1_FC_cc_f <- read.table("eg_data/NHANES/Food_D1_FC_cc_f.txt", sep="\t", header=T)
  Food_D2_FC_cc_f <- read.table("eg_data/NHANES/Food_D2_FC_cc_f.txt", sep="\t", header=T)
  
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
# From here, branch out into two Scenarios, A or B. (B consists of B-1, B-2, B-3, and B-4.)
# Scenario A: Further process food1b and food2b for building a food tree.  
# Scenario B-1-4: Calculate totals from the QC-ed food items and QC totals.
# ================================================================================================================  
  
# ================================================================================================================  
# Scenario A: Further process food1b and food2b for building a food tree.  
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
  identical(colnames(food1e), colnames(food2e))
  
# Combine food1 and food2 as a longtable (add food2 rows after food1 rows).
  food12e <- rbind(food1e, food2e)
  
# Select only the individuals listed in keepnames12.
  food12f <- food12e[food12e$SEQN %in% keepnames12, ]

# Save as a .txt. It will be a HUGE file. Use this as an input for food tree.
  write.table(food12f, "eg_data/NHANES/Food_D12_FC_cc_f_s.txt", sep="\t", row.names=F, quote=F)

# Add the demographic data to food12f for data overview.  
  food12f_d <- merge(x=food12f, y=demog, by="SEQN", all.x=T)

# Save. It will be a HUGE file. Use this as an input for data overview.
  write.table(food12f_d, "eg_data/NHANES/Food_D12_FC_cc_f_s_d.txt", sep="\t", row.names=F, quote=F)
   
# ================================================================================================================  
# Scenario B-1: Further processing of food1b and food2b for calculating totals and clustering.  
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
  day1variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_FC_Day1.txt', header=F)

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
  day2variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_FC_Day2.txt', header=F)
  var_to_use2 <- names(food2bb) %in% day2variables$V1
  food2c <- food2bb[, var_to_use2]
  colnames(food2c) <- gsub(colnames(food2c), pattern = "^DR2I", replacement = "")
  colnames(food2c) <- gsub(colnames(food2c), pattern = "^DR2", replacement = "")
  
# Make a day variable before combining food1c and food2c. 
  food1c$Day <- 1
  food2c$Day <- 2

# Ensure the columns of food1c and food2c match before joining them.
  identical(colnames(food1c), colnames(food2c))
  
# If not, create a table that has the column names of both food1c and food2c,
# examine the column names side-by-side.
  names <- data.frame(matrix(nrow= max(ncol(food1c), ncol(food2c)), ncol=2))
  colnames(names) <- c("food1c", "food2c")
  names$food1c <- colnames(food1c)
  names$food2c <- colnames(food2c)
  names

# Combine food1 and food2 as a longtable.
  food12c <- rbind(food1c, food2c)
  
# Pick up only the individuals listed in keepnames12.
  food12d <- food12c[food12c$SEQN %in% keepnames12, ]

# Save the combined and QC-ed food items as a .txt file. 
# This has nutrient information, food categories, and day variable for each food item reported,
# and shall be used to calculate totals in B-2. 
#### THIS WILL BE A HUGE FILE. ####
  write.table(food12d, "eg_data/NHANES/Food_D12_FC_QC.txt", sep="\t", quote=F, row.names=F)  

# ---------------------------------------------------------------------------------------------------------------
# You may also want to consider special diets that some participants are following: e.g. DASH diet, diabetic diet, 
# etc. Depending on your research question, you may want to exclude those following special diets.
# The diet information is found in totals day 1. We will revisit diet information in the next section.

# ===============================================================================================================
# B-2: Calculate totals/day/participant with the food data of the selected SEQNs.
# ===============================================================================================================

# Load the QC-ed food items.
  food12d <- read.table("eg_data/NHANES/Food_D12_FC_QC.txt", sep="\t", header=T)

# Calculate totals for day 1 and day 2, and combine the two datasets.
  TotalNHANES(food12d= food12d, 
              first.val= "GRMS", last.val= "A_DRINKS", 
              outfn = "eg_data/NHANES/Total_D12_FC_QC_eachday.txt" 
              )  

# Load the resultant total.
  total12d <- read.table("eg_data/NHANES/Total_D12_FC_QC_eachday.txt", sep="\t", header=T)
  
# total12d has the sum of each variable (columns) for each day and participant. 
  head(total12d, 2)
  

# ===============================================================================================================
# B-3: Calculate the mean of totals/participant. 
# ===============================================================================================================

# Calculate the mean of the two days of the totals data per participant. 
  AverageTotalNHANES(food12d= food12d, 
                     first.val= "GRMS", last.val= "NoOfItems", 
                     outfn= "eg_data/NHANES/Total_D12_FC_QC_mean.txt")  
  
# Load the mean total
  meantotal12b <- read.table("eg_data/NHANES/Total_D12_FC_QC_mean.txt", sep="\t", header=T)

    
# ===============================================================================================================
# B-4: QC the mean total in the same way as ASA24. 
# ===============================================================================================================
  
# For individual food data, there is no code for cleaning.
# Outliers won't severely affect main analysis conclusions (ASA24 data cleaning doc)
# But, it's always a good idea to take a look at the distributions of variables of interest. 
# Could calculate totals by occasion, similar to the ASA24 code.
  
# ---------------------------------------------------------------------------------------------------------------
# For totals, the same QC can be applied as ASA24 totals QC procedure.

# Run all these QC steps in this order.  When asked, choose to remove the outliers
# that fall outside the specified range for each nutrient.

# Define the input data.  This dataframe will be modified after each filter.
  QCtotals <- meantotal12b
  
  # Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "KCAL", min = 600, max = 5700)
  
  # Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "PROT", min = 10, max = 240)
  
  # Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "TFAT", min = 15, max = 230)

  # Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals,  
             target.colname = "VC", min = 5, max = 400)
  
      # or show the outliers if too many.
      VCoutliers <- Outlier_rows[, c('SEQN', 'KCAL', 'VC')]
      # Show the first n rows of the outliers in descending order. 
      head(VCoutliers[order(VCoutliers$VC, decreasing = T), ], n=10)

# Look at how many rows (observations) were kept after QC. 
  dim(QCtotals)
      
# ---------------------------------------------------------------------------------------------------------------
# Save QCtotal as a .txt file. 
  write.table(QCtotals, "eg_data/NHANES/Total_D12_FC_QC_mean_QC.txt", sep="\t", quote=F, row.names=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Add demograhic data to the QC-ed total. 
# Load the demographics file if you have not done so yet.
  demog <- read.xport("eg_data/NHANES/Raw_data/DEMO_I.XPT")
  
# Merge QC-totals and demographics by SEQN.
  QCtotals_d <- merge(x=QCtotals, y=demog, by="SEQN", all.x=TRUE)

# Save QCtotal_d as a .txt file. 
  write.table(QCtotals_d, "eg_data/NHANES/Total_D12_FC_QC_mean_QC_d.txt", sep="\t", quote=F, row.names=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)  
  