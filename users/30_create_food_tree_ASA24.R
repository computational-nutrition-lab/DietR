# ===============================================================================================================
# Generate a food tree from ASA24 data.
# Version 1
# Created on 02/17/2022 by Rie Sadohara
# ===============================================================================================================

# Before running the following code, one needs to have cleaned input data and obtained 
# 'New_totals' or 'totals', which are going to be used here. 

# The following code expects that input files are tab-delimited txt file. 
# Ensure your input files have no special characters that mess up loading:
#  "
#  '
#  #
#  &

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# ---------------------------------------------------------------------------------------------------------------
# Load the data.tree package necessary for newick.tree.r, and if it is not installed, install it. 
  if (!require("data.tree", quietly = TRUE))install.packages("data.tree")
  
# Load source scripts
  source("lib/specify_data_dir.R")
  source("lib/Food_tree_scripts/newick.tree.r")
  source("lib/Food_tree_scripts/check.db.r")
  source("lib/Food_tree_scripts/format.foods.r")
  source("lib/Food_tree_scripts/format.foods_2.r")
  source("lib/Food_tree_scripts/filter.db.by.diet.records.r")
  source("lib/Food_tree_scripts/make.food.tree.r")
  source("lib/Food_tree_scripts/make.food.otu.r")
  source("lib/Food_tree_scripts/make.fiber.otu.r")
  source("lib/Food_tree_scripts/make.dhydrt.otu.r")

# You can come back to the main directory by:
  setwd(main_wd)   

# ===============================================================================================================
# Load and prep data for generating food trees.
# ===============================================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ_formatfoods//")

# Load data.                
  QCed.foods <- read.table("VVKAJ_Items_f_id_s_m_QCed.txt", sep="\t", header=T, quote="", colClasses = "character") # quote="" added.
  head(QCed.foods) #FoodID has .0!!!!!!

# # ===============================================================================================================
# # Limit to just the foods reported in your study (use formatted dietrecords.txt as the input) 
# # ===============================================================================================================
# # Keep only the foods reported in your study. This is to make data compatible to create a food tree.
# FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt",
#                       food_records_fn  = "Food_D12_FC_QC_demo_QCed_males60to79.txt",  # output of filtering above.
#                       output_fn =        "Food_D12_FC_QC_demo_QCed_males60to79_red.txt")  
  
# ===============================================================================================================
# Limit to just the foods reported in your study (use formatted dietrecords.txt as the input) 
# ===============================================================================================================
# Keep only the foods reported in your study. This is to make data compatible to create a food tree.
  FilterDbByDietRecords(food_database_fn = "../Food_tree_eg/NHANESDatabase.txt",
                        food_records_fn  = "VVKAJ_Items_f_id_s_m_QCed.txt",  # output of filtering above.
                        output_fn =        "VVKAJ_Items_f_id_s_m_QCed_red.txt")
  
  #### Safety check, to be deleted.
  output_1 <- read.table("VVKAJ_Items_f_id_s_m_QCed_red.txt", sep="\t", header=T, quote="", colClasses="character") # quote="" added.
  head(output_1,2)
  tail(output_1,4) # FoodID has .0!!!!!!!!!!
  dim(output_1)
  
  View(FilterDbByDietRecords)
  head(QCed.foods$FoodCode)
  
  database <- read.table("../Food_tree_eg/NHANESDatabase.txt", sep="\t", header=T, quote="") # quote="" added.
  head(database)
  head(QCed.foods$FoodID)
  colnames(items)
  
  food_database_fn = "../Food_tree_eg/NHANESDatabase.txt"
  food_records_fn  = "VVKAJ_Items_f_id_s_m_QCed.txt"
  
  # the inside of the FilterByDietRecords function
  fdata <- read.table(food_database_fn, header=TRUE, sep="\t", colClasses="character", quote="", strip.white=T)
  head(fdata$FoodID)
  
  diet <- read.table(food_records_fn, header = TRUE, sep="\t", colClasses="character", quote="", strip.white=T)
  head(diet$FoodID)
  # .0 is missing. That is why there is nothing in the in the merged file. 
  
  valid_ids <- intersect(fdata$FoodID, diet$FoodID)
  
  # What about the NHANES input??
  nhanes = read.table("../NHANES/Laboratory_data/Food_D12_FC_QC_demo_QCed_males60to79.txt", 
                      header = TRUE, sep="\t", colClasses="character", quote="", strip.white=T  )  
  head(nhanes$FoodID) # has .0!!
  
  nhanes_nochar = read.table("../NHANES/Laboratory_data/Food_D12_FC_QC_demo_QCed_males60to79.txt", 
                      header = TRUE, sep="\t", quote="", strip.white=T  )  
  head(nhanes_nochar$FoodID) # has .0!!
  str(nhanes_nochar[, c("FoodCode", "FoodID")])
  # For some reason, for NHANES, FoodID is recognized as a character vector to begin with.  
  
  NHANESdatabase <- read.table('../Food_tree_eg/NHANESDatabase.txt', header=T, sep="\t", quote="", colClasses="character")
  NHANESdatabase <- read.delim('../Food_tree_eg/NHANESDatabase.txt', colClasses="character")
  head(NHANESdatabase)
  
  ####
  
# Use CheckDB function to check if any food reported in VVKAJ_Items_f_id_s_m_QCed_red.txt is missing in the 
# NHANES food database. # If there is, those will be written in the output file named xxx_missing.txt.
  check.db(food_database_fn = "../Food_tree_eg/NHANESDatabase.txt", 
           food_records_fn =  "VVKAJ_Items_f_id_s_m_QCed_red.txt",
           output_fn =        "VVKAJ_Items_f_id_s_m_QCed_red_missing.txt")

  # Load the output and check if the output contains anything? 
  mmm = read.table("VVKAJ_Items_f_id_s_m_QCed_red_missing.txt", sep="\t", header=T, quote="", colClasses="character")
  head(mmm)
  # Has item(s) ===> put this missing.txt file in addl_foods_fn argument of MakeFoodTree.
  # Empty       ===> put NULL in addl_foods_fn argument of MakeFoodTree.
  
  # Create food tree with the reduced dataset (only reported foods) classified at 
  # a desired level of classification (Lv. 1-6).
  # "NodeLabelsMCT.txt" has a list of food levels and names, which comes with the DietR package.
  MakeFoodTree(nodes_fn="../Food_tree_eg/NodeLabelsMCT.txt", 
               addl_foods_fn = NULL,
               num.levels = 3,
               food_database_fn =            "VVKAJ_Items_f_id_s_m_QCed_red.txt",  
               output_tree_fn =     "Foodtree/VVKAJ_Items_f_id_s_m_QCed_red_3Lv.tree.nwk", 
               output_taxonomy_fn = "Foodtree/VVKAJ_Items_f_id_s_m_QCed_red_3Lv.tax.txt"
  )
  #### it went through!!! Yayyyy 

# ===============================================================================================================
# Generate standard, grams of fiber, and dehydrated grams per kcal OTU tables to be used later.
# ===============================================================================================================
# Make the standard food otu table with data in gram weights of food.
# For the food_records_fn argument, you need to supply the items data which contains 'FoodAmt' column.
# It is the same file that was used for the "food_records_fn" argument in the FilterDbByDietRecords above. 

# It is OK to see see a warning message: 
# In write.table(dhydrt.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
#   appending column names to file
  MakeFoodOtu(food_records_fn=  "VVKAJ_Items_f_id_s_m_QCed.txt",  # same as food_records_fn in FilterDbByDietRecords 
              food_record_id =  "SampleID",               # Specify SampleID (User x Day)
              food_taxonomy_fn= "Foodtree/VVKAJ_Items_f_id_s_m_QCed_red_3Lv.tax.txt",  # Specify your taxonomy file produced by MakeFoodTree.
              output_fn =       "Foodtree/VVKAJ_Items_f_id_s_m_QCed_red_3Lv.food.otu.txt")  # Name your output otu file.
  
# Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "VVKAJ_Items_f_id_s_m_QCed.txt", 
               food_record_id=   "SampleID", 
               food_taxonomy_fn= "Foodtree/VVKAJ_Items_f_id_s_m_QCed_red_3Lv.tax.txt", 
               output_fn=        "Foodtree/VVKAJ_Items_f_id_s_m_QCed_red_3Lv.fiber.otu.txt")
  
# Make a food otu table as dehydrated grams per kcal.
  MakeDhydrtOtu(food_records_fn=  "VVKAJ_Items_f_id_s_m_QCed.txt", 
                food_record_id =  "SampleID", 
                food_taxonomy_fn= "Foodtree/VVKAJ_Items_f_id_s_m_QCed_red_3Lv.tax.txt", 
                output_fn =       "Foodtree/VVKAJ_Items_f_id_s_m_QCed_red_3Lv.dhydrt.otu.txt")
  
  #### Went through!!! Yayyyy
  #### Next, check if I can do ordination with these, and revise the ordination script accordingly. 
       # better to make a copy of ordination and use these newly formed food tree files....
  
  
  
  
  
        
#
#
##### OLD BELOW ######  
# ===============================================================================================================
# Prep data
# ===============================================================================================================

# Current ASA24 database does not have modcodes, so de-duplicate database file, 
# replace special characters with "_", and create a new FoodID out of foodcode and modcode.
# It leaves all other columns intact.
  FormatFoods(input_fn="eg_data/Food_tree_eg/all.food.desc.txt", output_fn="eg_data/Food_tree_eg/ASA24Database.txt")

# FoodCode and Main.food.description of additional foods not in ASA24. Format it for use.
  FormatFoods(input_fn="eg_data/Food_tree_eg/Soylent_codes.txt", 
              output_fn="eg_data/Food_tree_eg/Soylent_codes_formatted.txt")
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")
  
# Create a folder called "Foodtree" in the current working directory ("VVKAJ").
  
# Format your items data, and save the formatted items file to the "Foodtree" folder.
  FormatFoods(input_fn="VVKAJ_Items_f_id_s_m.txt", 
              output_fn="Foodtree/VVKAJ_Items_f_id_s_m_ff.txt", dedupe=F)

# ===============================================================================================================
# Generate a food tree with the whole ASA24 database. 
# ===============================================================================================================
  
# Move to "Food_tree_eg" directory.
  SpecifyDataDirectory(directory.name = "eg_data/Food_tree_eg/")
  
# Generate a tree with the whole ASA24 food database first. 
  # if there are missing foods, then create new files to add them in below under addl_foods
  MakeFoodTree(nodes_fn=        "NodeLabelsMCT.txt", 
               food_database_fn="ASA24Database.txt", 
               addl_foods_fn=  "Soylent_codes_formatted.txt", 
               num.levels = 4,  # How many levels of foods to be classified
               output_taxonomy_fn = "Food_tree_all_ASA24/ASA24_Lv4.taxonomy.txt",  # Name your output taxonomy file
               output_tree_fn=      "Food_tree_all_ASA24/ASA24_Lv4.tree.nwk"       # Name your output tree
               )

# ===============================================================================================================
# Generate a food tree with your own items dataset. 
# ===============================================================================================================
  
# Move to Foodtree folder in VVKAJ/Foodtree again.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Foodtree")
  
# Reduce the input dataset first - create a database with foods in your items file only.
# The food_database_fn is the whole ASA24 dataset. 
# The food_records_fn is the output of FormatFoods of your items above.
# The output_fn is the decreased database that only contains food items present in your data.
# Limit to just the foods reported in your study (formatted dietrecords.txt as the input)
  FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/ASA24Database.txt", 
                        food_records_fn  = "VVKAJ_Items_f_id_s_m_ff.txt",   # output of FormatFoods above.
                        output_fn        = "VVKAJ_Items_f_id_s_m_ff_database.txt")

# Make a food tree with the reduced data.
  MakeFoodTree(nodes_fn=         "../../Food_tree_eg/NodeLabelsMCT.txt", 
               food_database_fn= "VVKAJ_Items_f_id_s_m_ff_database.txt",    # output for FilterDbByDietRecords above.
               addl_foods_fn   = NULL,
               num.levels      = 4,
               output_taxonomy_fn = "VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tax.txt",
               output_tree_fn=      "VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tree.nwk" 
               )

# ===============================================================================================================
# Generate standard, grams of fiber, and dehydrated grams per kcal OTU tables to be used later.
# ===============================================================================================================
# Make the standard food otu table with data in gram weights of food.
# It is OK to see see a warning message: 
# In write.table(dhydrt.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
#   appending column names to file
  MakeFoodOtu(food_records_fn=  "VVKAJ_Items_f_id_s_m_ff.txt", 
              food_record_id =  "SampleID",               # Specify SampleID (User x Day)
              food_taxonomy_fn= "VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tax.txt",  # Specify your taxonomy file produced by MakeFoodTree.
              output_fn =       "VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.food.otu.txt")  # Name your output otu file.
  
# Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "VVKAJ_Items_f_id_s_m_ff.txt", 
               food_record_id=   "SampleID", 
               food_taxonomy_fn= "VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tax.txt", 
               output_fn=        "VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.fiber.otu.txt")
  
# Make a food otu table as dehydrated grams per kcal.
  MakeDhydrtOtu(food_records_fn=  "VVKAJ_Items_f_id_s_m_ff.txt", 
                food_record_id =  "SampleID", 
                food_taxonomy_fn= "VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tax.txt", 
                output_fn =       "VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.dhydrt.otu.txt")
  
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)  
