# ===============================================================================================================
# Load NHANES 2015-16 FOOD data, add food description, QC, and calculate total. 
# Version 2
# Created on 05/18/2022 by Rie Sadohara and Suzie Hoops
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
  source("lib/prep_data_for_clustering.R")
  source("lib/Food_tree_scripts/format.foods.r")
  
# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Prepare food codes, FPED, and food items files
# ===============================================================================================================

### Load and prepare food codes  
# Download food code data from NHANES website and save it in "Raw_data" folder. 
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DRXFCD_I.XPT", 
                destfile= "eg_data/NHANES/Raw_data/FoodCodes_DRXFCD_I.XPT", mode="wb")
  
# Prepare the food code table - replace special characters with "_" or "and".
  
  # Format the food table and save it as a .txt file.
  PrepareFoodCodeTable(raw.food.code.table = "eg_data/NHANES/Raw_data/FoodCodes_DRXFCD_I.XPT", 
                       out.fn =              "eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt")  
  
  # Load the formatted foodcode table.
  foodcodetable_f <- read.table("eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt", sep="\t", header=T)
  
  # Show the first 10 rows of the output and ensure special characters are gone; e.g., 
  # MILK, REDUCED FAT (2%) is now MILK, REDUCED FAT (2_).
  foodcodetable_f[1:10, ]
  
# ---------------------------------------------------------------------------------------------------------------
# Load and prepare FPED
  
# FPED has the composition of nutrients and food categories of each food item coded by food codes.
# FPED can be downloaded from USDA -ARS FPED databases 
# (https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/)

# You need to use the correct version of FPED. With NHANES, use FPED with the same release year
# as the year of NHANES you are analyzing. For this tutorial, FPED was downloaded from NHANES
# the FPED table is formatted by renaming the variables with R-loadable ones (e.g., "F_CITMLB (cup eq.)" 
# --> "F_CITMLB" and saved as "FPED_1516_forR.txt". 
 
    
# Load FPED15-16, needed for the AddFoodCat function.
  FPED <- read.table("eg_data/NHANES/FPED/FPED_1516_forR.txt", sep="\t", header=T)
  
  # Check the first 2 rows of FPED. 
  head(FPED, 2)
  
  # Important! Change the food code column name as Food_code.
  colnames(FPED)[1] <- "Food_code"

# ---------------------------------------------------------------------------------------------------------------
# Load "food items" data and add food descriptions
  
# Food data can be downloaded from NHANES website.
# Name the file and destination. mod="wb" is needed for Windows OS.
# Other OS users may need to delete it.
  # Download day 1 data.
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1IFF_I.XPT", 
                destfile= "eg_data/NHANES/DR1IFF_I.XPT", mode="wb")
  
  # Download day 2 data.
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR2IFF_I.XPT",
                destfile= "eg_data/NHANES/DR2IFF_I.XPT", mode="wb")
  
# [NOTE] Different alphabets are used on the variables' names in different release of NHANES 
# data.  Therefore, you will need to change the alphabet (and potentially the other parts of the 
# variable names) in order to run this script with other releases of NHANES. For example, 
# DR1IFDCD is a column name for the food code used in the NHANES 2015-2016, and the 
# alphabet for this release is "I".   

# ---------------------------------------------------------------------------------------------------------------
# Load and prepare Day 1 food items data
  
# Import items data Day 1, add food item descriptions, and save it as a txt file.
# OUTPUT WILL LIKELY BE A HUGE FILE.
  ImportNHANESFoodItems(data.name="eg_data/NHANES/DR1IFF_I.XPT", 
                        food.code.column = "DR1IFDCD", 
                        food.code.table = foodcodetable_f,
                        out.fn = "eg_data/NHANES/DR1IFF_I_d.txt") # 'd' stands for food descriptions

# Load the saved food items file.
  Food_D1 <- read.table("eg_data/NHANES/DR1IFF_I_d.txt", sep="\t", header=T)

# Count the number of participants - should be 8,505 people.
  length(unique(Food_D1$SEQN)) 

# Add the food category info and serving for each item. #### WILL TAKE A FEW MOMENTS. ####
  AddFoodCat(input.food= Food_D1,
             fped= FPED,
             grams= "DR1IGRMS", 
             out.fn= "eg_data/NHANES/Food_D1_FC.txt")
  # It is OK to see a message saying "NAs introduced by coercion."
  # NAs will be removed later in the filtering process.

# ---------------------------------------------------------------------------------------------------------------
# Load and prepare Day 2 food items data
  
# Import items data Day 2, add food item descriptions, and save it as a txt file.
  ImportNHANESFoodItems(data.name="eg_data/NHANES/DR2IFF_I.XPT",
                        food.code.column = "DR2IFDCD",
                        food.code.table = foodcodetable_f,
                        out.fn = "eg_data/NHANES/DR2IFF_I_d.txt")

# Add food item description and save it as a txt file.
  Food_D2 <- read.table("eg_data/NHANES/DR2IFF_I_d.txt", sep="\t", header=T)

# Count the number of participants - should be 7027 people.
  length(unique(Food_D2$SEQN)) 

# Do the same for Day 2. Add the food items info and serving for each item. 
#### WILL TAKE A FEW MOMENTS. ####
  AddFoodCat(input.food= Food_D2, 
             fped= FPED, 
             grams= "DR2IGRMS", 
             out.fn= "eg_data/NHANES/Food_D2_FC.txt")
  # It is OK to see a message saying "NAs introduced by coercion."
  # NAs will be removed later in the filtering process.

# ===============================================================================================================
# Load the Food_Dx_FC which has the food category data 
# ===============================================================================================================

# Change column names to more intuitive ones
  
# Food Day 1 with Food Category *** WILL BE A HUGE TABLE. ***
  Food_D1_FC <- read.table("eg_data/NHANES/Food_D1_FC.txt", sep="\t", header=T)
  
# Food Day 2 with Food Category *** WILL BE A HUGE TABLE. ***
  Food_D2_FC <- read.table("eg_data/NHANES/Food_D2_FC.txt", sep="\t", header=T)
  
# Change the colnames for downstream analyses
  names(Food_D1_FC)[names(Food_D1_FC) == "DR1IFDCD"] <- "FoodCode"
  names(Food_D1_FC)[names(Food_D1_FC) == "DR1IGRMS"] <- "FoodAmt"
  names(Food_D1_FC)[names(Food_D1_FC) == "DRXFCLD"] <- "Main.food.description"
  
  names(Food_D2_FC)[names(Food_D2_FC) == "DR2IFDCD"] <- "FoodCode"
  names(Food_D2_FC)[names(Food_D2_FC) == "DR2IGRMS"] <- "FoodAmt"
  names(Food_D2_FC)[names(Food_D2_FC) == "DRXFCLD"] <- "Main.food.description"
  
# Ensure the column names are changed. 
  colnames(Food_D1_FC)
  
# Save after changing the column names. "cc" stands for column names changed.
  write.table(Food_D1_FC, "eg_data/NHANES/Food_D1_FC_cc.txt", sep="\t", row.names=F, quote=F)
  write.table(Food_D2_FC, "eg_data/NHANES/Food_D2_FC_cc.txt", sep="\t", row.names=F, quote=F)
  
# Replace special characters with "_" using FormatFoods.
  
# FotmatFoods() function adds "Main.Food.Description" where special characters are removed/replaced, the previous
# Main.Food.Description as Old.Main.Food.Description, ModCode, and FoodID. $FoodID is a cha vector, but has .0 at the end. 
# MAKE SURE dedupe=F. If true (default!), duplicated foods will be removed! 
  FormatFoods(input_fn="eg_data/NHANES/Food_D1_FC_cc.txt", output_fn= "eg_data/NHANES/Food_D1_FC_cc_f.txt", dedupe=F)
  FormatFoods(input_fn="eg_data/NHANES/Food_D2_FC_cc.txt", output_fn= "eg_data/NHANES/Food_D2_FC_cc_f.txt", dedupe=F)

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)  
  