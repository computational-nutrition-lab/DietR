# ===============================================================================================================
# Change fotmat.file() to FormatFoods() to remove special characters and add FoodID column.
# Why? because FoodID column is needed in creating food tree.
# Version 1
# Created on 01/17/2023 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================

# ===============================================================================================================

# Set your working directory to the main directory.
Session --> Set working directory --> Choose directory.
setwd("~/GitHub/DietR")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
source("lib/specify_data_dir.R")  
source("lib/load_clean_ASA24.R")
source("lib/format.file.R")
source("lib/average.by.R") 
source("lib/QCOutliers.R")
# source("lib/Food_tree_scripts/format.foods.r")
source("lib/Food_tree_scripts/format.foods_2.r")

# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Load ASA24 data
# ===============================================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name= "eg_data/VVKAJ_formatfoods/")

# Load your unprocessed (raw) food items-level data (as downloaded from the ASA24 study website).
# The csv file will be loaded as a dataframe in R and be named as items_raw. 
  items_raw <- read.csv("Raw_data/VVKAJ_Items.csv", sep = ",", header=T) 

# Save it as a .txt file. 
  write.table(items_raw, "VVKAJ_Items.txt", sep="\t", row.names=F) 

# Special characters common in food names in dietary data such as "'", ",", "%" may interfere correct 
# data loading in R; thus, we replace them with an underscore "_".  The format.file  function takes only
# .txt files as input. 

#### Does it have Main.food.desription?
  items <- read.table("VVKAJ_Items.txt", sep="\t", header=T)
  colnames(items)
# It only has "Food_description".
# That is why I used format.file.

# So, I need to change Food_description to 'Main.food.decription' in order to make the FormatFoods function work?
  names(items)[names(items) == "Food_Description"] <- "Main.food.description"
  colnames(items)

  write.table(items, "VVKAJ_Items_mainfood.txt", sep="\t", row.names=F) 

  items_mainfood <- read.table("VVKAJ_Items_mainfood.txt", header=T, sep = "\t")
  head(items_mainfood) 
  # Has special characters.


FormatFoods(input_fn =  "VVKAJ_Items_mainfood.txt", 
            output_fn = "VVKAJ_Items_f.txt",
            dedupe = F)

# afterformatfoods <- read.table("VVKAJ_Items_mainfood_formatfoods.txt", header=T, sep = "\t")
  afterformatfoods <- read.table("VVKAJ_Items_f.txt", quote="", header=T, sep = "\t") # need quote=""
  afterformatfoods <- read.table("VVKAJ_Items_f.txt", colClasses = "character", quote="", header=T, sep = "\t") 
  dim(afterformatfoods)  
  head(afterformatfoods)

  afterformatfoods_colClasses <- read.table("VVKAJ_Items_f.txt", colClasses = "character", 
                                          quote="", header=T, sep = "\t") 
  head(afterformatfoods_colClasses)
  
# Food ID has .0, good.

#### IMPORTANT! ########################################################  
# So, what I needed to do: 
  # 1: change Food_description to Main.food.description
  # 2: run FormatFoods to replace special characters.
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name= "eg_data/VVKAJ_formatfoods/")
  
# Load your unprocessed (raw) food items-level data (as downloaded from the ASA24 study website).
# The csv file will be loaded as a dataframe in R and be named as items_raw. 
  items_raw <- read.csv("Raw_data/VVKAJ_Items.csv", sep = ",", header=T) 
  
# items_raw has a column called "Food_Description", but this needs to be called "Main.food.description".
# Change the column name.
  names(items_raw)[names(items_raw) == "Food_Description"] <- "Main.food.description"
  
# Check if any column names match with "Main.food.description". If there is a match, it will be printed.  
  names(items_raw)[names(items_raw) == "Main.food.description"]
  
# Save the items file as a .txt file. 
  write.table(items_raw, "VVKAJ_Items.txt", sep="\t", row.names=F) 

# Format foods so that special characters will be replaced with "_".    
  FormatFoods(input_fn =  "VVKAJ_Items_mainfood.txt", 
              output_fn = "VVKAJ_Items_f.txt",
              dedupe=F)
  
# Start from line 80 (after format.file) of 02_load_clean_ASA24.R. i.e.;
# Add a human-readable sample identifier (SampleID) with a desired prefix, and save it as a txt file. SampleIDs
# |
# Result: still, FoodID does not have .0 and therefore cannot be used in the food tree script.
# Need to find where FoodID lost .0 , and fix it...
# |
# Fixed!! Now make a copy of scripts to keep both the old and new versions...

# So, make a copy of 02_load_clean_ASA24.R and name it as 02_load_clean_ASA24_Dec2022.R. Then rename the 
  # copy as 02_load_clean_ASA24.R. And in that, paste the loading and formatting part above, then continue
  # with 02_load_clean_ASA24_FoodIDfixed.R.
  # -- Done.

############################################################  


