# ===============================================================================================================
# Generate a food tree from ASA24 data without using FilterDBByDiet, a clean version.
# Version 2
# Created on 02/16/2023 by Rie Sadohara
# ===============================================================================================================

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
  source("lib/Food_tree_scripts/make.food.tree.r")
  source("lib/Food_tree_scripts/make.food.otu.r")
  source("lib/Food_tree_scripts/make.fiber.otu.r")
  source("lib/Food_tree_scripts/make.dhydrt.otu.r")

# You can come back to the main directory by:
  setwd(main_wd)   

# Create a new folder called "Foodtree" in the "VVKAJ" folder.  
  
# ===============================================================================================================
# Generate a food tree from food items reported in your study.
# ===============================================================================================================
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")

# Create food tree with the reduced dataset (only reported foods) classified at 
# a desired level of classification.

  MakeFoodTree(nodes_fn="../Food_tree_eg/NodeLabelsMCT.txt", 
               food_database_fn =            "VVKAJ_Items_f_id_s_m_QCed.txt",  
               addl_foods_fn = NULL,
               num.levels = 4,
               output_tree_fn =     "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tree.nwk", 
               output_taxonomy_fn = "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tax.txt")  

  # food_database_fn:   Your food item data.
  # addl_foods_fn:      List of foods not present in the list can be added. NULL by default. 
  # output_tree_fn:     Name output tree file with .nwk at the end.
  # output_taxonomy_fn: Name output taxonomy file.
  
  
# ===============================================================================================================
# Generate standard, grams of fiber, and dehydrated grams per kcal OTU tables to be used later.
# ===============================================================================================================
# Make the standard food otu table with data in gram weights of food.
# For the food_records_fn argument, you need to supply the items data which contains 'FoodAmt' column.

# It is OK to see see a warning message: 
# In write.table(dhydrt.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
#   appending column names to file
  MakeFoodOtu(food_records_fn=  "VVKAJ_Items_f_id_s_m_QCed.txt",   
              food_record_id =  "SampleID",               
              food_taxonomy_fn= "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tax.txt",  
              output_fn =       "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.otu.txt")  
  
    # food_records_fn:  Same as food_records_fn in MakeFoodTree.
    # food_record_id:   Your SampleID (User x Day)
    # food_taxonomy_fn: Taxonomy file produced by MakeFoodTree.
    # output_fn:        Name output otu file.
  
# Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "VVKAJ_Items_f_id_s_m_QCed.txt", 
               food_record_id=   "SampleID", 
               food_taxonomy_fn= "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tax.txt", 
               output_fn=        "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.fiber.otu.txt")
  
# Make a food otu table as dehydrated grams per kcal.
  MakeDhydrtOtu(food_records_fn=  "VVKAJ_Items_f_id_s_m_QCed.txt", 
                food_record_id =  "SampleID", 
                food_taxonomy_fn= "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tax.txt", 
                output_fn =       "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.dhydrt.otu.txt")

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)  
  