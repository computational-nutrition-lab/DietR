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

# Ensure your input files have no special characters that interfere loading:
  #  "
  #  '
  #  #
  #  &

# # Use dietstudy data -----------------------------------------------------------
# # Specify the directory where the data is.
#   SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
# 
#   # Load the items.csv
#     items <- read.table("Items_to_use.txt", quote = "", sep = "\t", header = T)
# 
# # # Load the totals.csv
# #   totals <- read.table("Totals_to_use.txt", sep = "\t", header = T)

# Use VVKAJ data. ---------------------------------------------------------
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ")
    
# Load your items data.
# Items_raw <- read.csv("VVKAJ_Items_NoCommas.csv", sep = ",", header=T)
  items_f_id_s_m <- read.table("VVKAJ_Items_f_id_s_m.txt", sep = "\t", header=T)
  
 
# # Load your metadata if you have one. 
#   metadata_1 <- read.csv("Metadata_1.csv", header=T)
#   metadata_2 <- read.csv("Food_map_txt_Metadata_2.csv", header=T)

# Come back to the main directory
  # setwd(main_wd)  

# ========================================================================================
# Generate sum of foods consumed by each user, by day, and by occasion
  # The output Sum_by_User_Day_Occ has the sum of foods consumed by each user, day, and occasion,
  # with occasion names in words.
# ======================================================================================== 
# Use VVKAJ data. ---------------------------------------------------------
# Calculate sum of nutrients and food categories of items by user and by occasion;
# output will be saved as: Items_by_User_Occ.  
  SumByOccasion(items.data=items_f_id_s_m, User.Name='UserName', 
                  Recall.No='RecallNo',   Occ.No='Occ_No')
  
# Take a look at the first 10 rows of the first 6 columns.
# Items are summed by occasion number for each participant's RecallNo. 
  Items_by_User_Occ[1:10, 1:6]

# Use Items_by_User_Occ and add occasion names (breakfast, snacks etc.) to it. 
# output will be saved as: Sum_by_User_Day_Occ.
  AddOccNames(items.data=items_f_id_s_m, User.Name='UserName', 
                Recall.No='RecallNo', Occ.No='Occ_No', Occ.Name='Occ_Name'  )

# Take a look at the first 6 rows. Spelt-out occasion names corresponding to occasion name 
# have been added at the end. 
  head(Sum_by_User_Day_Occ)
  
# # Use diet_study data. -----------------------------------------------------
#     SumByOccasion(items.data=items, User.Name='UserName', 
#                   Recall.No='RecordDayNo',   Occ.No='Occ_No')
#     
#     AddOccNames(items.data=items, User.Name='UserName', 
#                 Recall.No='RecordDayNo', Occ.No='Occ_No', Occ.Name='Occ_Name')

# ---------------------------------------------------------------------------------------------------------------
# Save Sum_by_User_Day_Occ as a txt file.
  write.table(Sum_by_User_Day_Occ, 'VVKAJ_Sum_by_User_Day_Occ.txt', sep="\t", row.names=F, quote=F)
# This will be useful if a researcher wants to look at the sum of each eating occasion 
# per participant. (Because Totals file sums all the occasions as one day)



