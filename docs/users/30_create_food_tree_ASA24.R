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
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# You can come back to the main directory by:
  setwd(main_wd)   
  
# Load source scripts
  source("lib/specify_data_dir.R")
  
  source("lib/Food_tree_scripts/newick.tree.r")
  source("lib/Food_tree_scripts/check.db.r")
  source("lib/Food_tree_scripts/format.foods.r")
  source("lib/Food_tree_scripts/filter.db.by.diet.records.r")
  source("lib/Food_tree_scripts/make.food.tree.r")
  source("lib/Food_tree_scripts/make.food.otu.r")
  source("lib/Food_tree_scripts/make.fiber.otu.r")
  source("lib/Food_tree_scripts/make.dhydrt.otu.r")

# ===============================================================================================================
# Prep data
# ===============================================================================================================

# Current ASA24 database doesn't have modcodes, so de-duplicate database file, 
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
  
# Move to Foodtree folder in VVKAJ again.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Foodtree")
  
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
