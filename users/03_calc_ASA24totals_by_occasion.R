# ========================================================================================
# Calculate totals by occasion, user, and day. 
# Useful for analyzing dietary intake per occasion (breakfast, lunch, etc..).
# Version 1
# Created on 03/16/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Load data
# ========================================================================================
# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")
  source("lib/calc_ASA24totals_by_occasion.R")

# Ensure your input files have no special characters that mess up loading:
  #  "
  #  '
  #  #
  #  &

# Use dietstudy data -----------------------------------------------------------
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")

  # Load the items.csv
    items <- read.table("Items_to_use.txt", quote = "", sep = "\t", header = T)

# # Load the totals.csv
#   totals <- read.table("Totals_to_use.txt", sep = "\t", header = T)

# Use VVKAJ data. ---------------------------------------------------------
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")
    
    # Load your items data.
      Items_raw <- read.csv("VVKAJ_2021-11-09_7963_Items_NoCommas.csv", sep = ",", header=T)
  
 
# # Load your metadata if you have one. 
#   metadata_1 <- read.csv("Metadata_1.csv", header=T)
#   metadata_2 <- read.csv("Food_map_txt_Metadata_2.csv", header=T)

# Come back to the main directory
  setwd(main.wd)  

# ========================================================================================
# Generate sum of foods consumed by each user, by day, and by occasion
  # The output Sum_by_User_Day_Occ has the sum of foods consumed by each user, day, and occasion,
  # with occasion names in words.
# ======================================================================================== 
# Use VVKAJ data. ---------------------------------------------------------
    SumByOccasion(items.data=Items_raw, User.Name='UserName', 
                  Recall.No='RecallNo',   Occ.No='Occ_No')
    
    AddOccNames(items.data=Items_raw, User.Name='UserName', 
                Recall.No='RecallNo', Occ.No='Occ_No', Occ.Name='Occ_Name'  )

# Use diet_study data. -----------------------------------------------------
    SumByOccasion(items.data=items, User.Name='UserName', 
                  Recall.No='RecordDayNo',   Occ.No='Occ_No')
    
    AddOccNames(items.data=items, User.Name='UserName', 
                Recall.No='RecordDayNo', Occ.No='Occ_No', Occ.Name='Occ_Name')

# ---------------------------------------------------------------------------------------------------------------
# Save as a csv file.
  write.csv(Sum_by_User_Day_Occ, 'dietstudy_Sum_by_User_Day_Occ.csv')
# This will be useful if a researcher wants to look at the sum of each eating occasion 
# per participant. (Because Totals file sums all the occasions in one day.)



