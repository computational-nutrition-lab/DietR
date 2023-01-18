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
  
# Food ID has .0.

# Start from line 80 (after format.file) of 02_load_clean_ASA24.R. i.e.;
# Add a human-readable sample identifier (SampleID) with a desired prefix, and save it as a txt file. SampleIDs
# |
# Result: still, FoodID does not have .0 and therefore cannot be used in the food tree script.
# Need to find where FoodID lost .0 , and fix it...
# |
# Fixed!! Now make a copy of scripts to keep both the old and new versions...





View(FormatFoods) 
## FormatFoods function inside
    fdata <- read.table("VVKAJ_Items_mainfood.txt", header = TRUE, sep="\t", strip.white = T) 
                        colClasses="character", quote="", strip.white=T)
    head(fdata)
    
    if(sum(colnames(fdata) == "Main.food.description") == 1){
      fdata$Old.Main.food.description <- fdata$Main.food.description
      # replace anything that isn't a number or character with an underscore (format for QIIME)
      fdata$Main.food.description <- gsub("[^[:alnum:]]+", "_", fdata$Main.food.description)
    }
    
    # add a default ModCode column if it doesn't exist
    if(sum(colnames(fdata) == "ModCode")==0) fdata$ModCode <- rep("0", nrow(fdata))
    
    # make a new food id that also uses the mod.code 
    fdata$FoodID <- paste(fdata$FoodCode, fdata$ModCode, sep=".")
    head(fdata$FoodCode)
    head(fdata$ModCode)
    head(fdata$FoodID)

      # IF dedupe=T:
      # grab the first occurence of any food id and we'll use that to construct the tree 
      # note that SuperTracker has duplicate names for each Food ID (important for mapping, but not for the actual tree)
      dedupedfdata <- fdata[!duplicated(fdata$FoodID),]
      
    # write everything out so that we have it for reference
    write.table(fdata, 'VVKAJ_Items_mainfood_formatfoods_byhand.txt', 
                sep = "\t", quote = FALSE, row.names = FALSE)
    
    byhand = read.table('VVKAJ_Items_mainfood_formatfoods_byhand.txt', sep = "\t", header = TRUE)
    head(byhand)  
####





# Specify column(s) to be processed in the "columns" argument.
# Specify the output file name in the outfn argument; "_f" stands for "formatted".  
format.file(filename = "VVKAJ_Items.txt",
            columns  = "Food_Description", 
            outfn    = "VVKAJ_Items_f.txt")  

