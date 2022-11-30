# ===============================================================================================================
# Generate food tree out of GLU - males in their 60-79, with 55 subsamples of Prediabetic.
# Version 1
# Created on 11/16/2022 by Rie Sadohara
# ===============================================================================================================

### Updated these 2 lines below to reflect the item location changes.###
  # FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt" 
  # MakeFoodTree(nodes_fn="../../Food_tree_eg/NodeLabelsMCT.txt"

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/dietarry_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# Install the data.tree package necessary for newick.tree.r. 
  if (!require("data.tree", quietly = TRUE))install.packages("data.tree")
  
# ---------------------------------------------------------------------------------------------------------------
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
  
# You can come back to the main directory by:
  setwd(main_wd) 
  
# ===============================================================================================================
# Load and prep data for generating food trees 
# ===============================================================================================================

# Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data")

# Load the males60to79 people. Note this is a total data (1 row/person).
  glu_3_males60to79 <- read.table("QCtotal_d_glu_body_meta_demo_males60to79_2.txt", 
                               sep="\t", header=T)

# Make the individuals as a vector.
  selectedind <- glu_3_males60to79$SEQN

# Load the input file (all food record data) to be filtered.
  all.food.record <- read.table("../Food_D12_FC_cc_f.txt", sep="\t", header=T)
  
# Select only the individuals listed in 'selectedind'. 
  sel.food.record <- all.food.record[all.food.record$SEQN %in% selectedind, ]

# Confirm the two contains the same set of individuals. 
  identical(unique(sel.food.record$SEQN), selectedind)

# Save. This will be the input in the following procedures.
  write.table(sel.food.record, "Food_D12_FC_cc_f_males60to79_2.txt", 
              sep="\t", row.names=F, quote=F)  


# ===============================================================================================================
# Limit to just the foods reported in your study (formatted dietrecords.txt as the input) 
# ===============================================================================================================
# Keep only the foods reported in your study. This is already done, but need to run this 
# so that the data will be formatted in a compatible way to create food tree.
  FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt", 
                        food_records_fn  = "Food_D12_FC_cc_f_males60to79_2.txt",   # output of FormatFoods.  
                        output_fn =        "Food_D12_FC_cc_f_males60to79_2_red.txt")

# Use CheckDB function to ensure no food reported Food_D12_FC_cc_f.txt in is missing in the database.

# Check if there is any food item reported by people but are missing in the database. 
  check.db(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt", 
           food_records_fn =  "Food_D12_FC_cc_f_males60to79_2_red.txt", 
           output_fn =        "Food_D12_FC_cc_f_males60to79_2_red_missing.txt")

# Load the output and check if the output contains anything? 
  mmm = read.table("Food_D12_FC_cc_f_males60to79_2_red_missing.txt", sep="\t", header=T)
  head(mmm)
  # Has something ===> put this missing.txt file in addl_foods_fn argument of MakeFoodTree.
  # Empty         ===> put NULL in addl_foods_fn argument of MakeFoodTree.

# Create food tree with the reduced dataset (only reported foods) classified at 
# a desired level of classification (Lv. 1-6).
# "NodeLabelsMCT.txt" has a list of food levels and names, which comes with this package.
  MakeFoodTree(nodes_fn="../../Food_tree_eg/NodeLabelsMCT.txt", 
               addl_foods_fn = NULL,
               num.levels = 3,
               food_database_fn =   "Food_D12_FC_cc_f_males60to79_2_red.txt",  
               output_tree_fn =     "Foodtree/Food_D12_FC_cc_f_males60to79_2_red_Lv3.nwk", 
               output_taxonomy_fn = "Foodtree/Food_D12_FC_cc_f_males60to79_2_red_Lv3.taxonomy.txt"
  ) 


# --------------------------------------------------------------------------------------------------------------
# Generate OTU tables for downstream analyses; IT MAY TAKE SOME TIME.
# It is OK to see the following warning message:
# In write.table(fiber.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
# appending column names to file.

# Make the standard food otu table with data in gram weights of food.
  MakeFoodOtu(food_records_fn=  "Food_D12_FC_cc_f_males60to79_2.txt",  # need to supply data that have 'FoodAmt' before applying FilterDBByDietRecords.  
              food_record_id =  "SEQN",                          # The ID of your participants
              food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_males60to79_2_red_Lv3.taxonomy.txt",  # Your taxonomy file produced by MakeFoodTree.
              output_fn =       "Foodtree/Food_D12_FC_cc_f_males60to79_2_red_Lv3.food.otu.txt")  # Output otu file to be saved.

# Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "Food_D12_FC_cc_f_males60to79_2.txt", 
               food_record_id=   "SEQN", 
               food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_males60to79_2_red_Lv3.taxonomy.txt", 
               output_fn=        "Foodtree/Food_D12_FC_cc_f_males60to79_2_red_Lv3.fiber.otu.txt")

# Make a food otu table as dehydrated grams per kcal
  MakeDhydrtOtu(food_records_fn=  "Food_D12_FC_cc_f_males60to79_2.txt", 
                food_record_id =  "SEQN", 
                food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_males60to79_2_red_Lv3.taxonomy.txt", 
                output_fn =       "Foodtree/Food_D12_FC_cc_f_males60to79_2_red_Lv3.dhydrt.otu.txt")  

# ---------------------------------------------------------------------------------------------------------------

# Come back to the main directory.
  setwd(main_wd)
  
  

