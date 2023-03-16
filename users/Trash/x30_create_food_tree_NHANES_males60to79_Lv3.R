# ===============================================================================================================
# Generate food tree out of GLU - males in their 60-79, with 55 subsamples of Prediabetic.
# Version 2
# Created on 01/03/2023 by Rie Sadohara
# ===============================================================================================================

# First, we will load the QC-ed averaged totals of males 60-79 years old (n=237) and all food records (n=4,207). 
# Then, we will keep the food records of only the individuals in males 60-79 years old and generate a food tree
# with the filtered food data. 

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# Load the data.tree package necessary for newick.tree.r, and if it is not installed, install it. 
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
  totals_males60to79 <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79.txt", 
                                   sep="\t", header=T)

# Make the individuals as a vector.
  selectedind <- totals_males60to79$SEQN

# Load the input file (all food record data) to be filtered.
  all.food.record <- read.table("../Food_D12_FC_QC_demo_QCed.txt", sep="\t", header=T)
                                     
# Select only the individuals listed in 'selectedind'.
  sel.food.record <- all.food.record[all.food.record$SEQN %in% selectedind, ]

# Confirm the two contains the same set of individuals. 
  identical(unique(sel.food.record$SEQN), selectedind)

# Save. This will be the input for the following procedures.
  write.table(sel.food.record, "Food_D12_FC_QC_demo_QCed_males60to79.txt", 
              sep="\t", row.names=F, quote=F) 

# ===============================================================================================================
# Limit to just the foods reported in your study (use formatted dietrecords.txt as the input) 
# ===============================================================================================================
# Keep only the foods reported in your study. This is to make data compatible to create a food tree.
  FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt",
                        food_records_fn  = "Food_D12_FC_QC_demo_QCed_males60to79.txt",  # output of filtering above.
                        output_fn =        "Food_D12_FC_QC_demo_QCed_males60to79_red.txt")

# Use CheckDB function to check if any food reported in Food_D12_FC_QC_demo_QCed.txt is missing in the 
# NHANES food database. # If there is, those will be written in the output file named xxx_missing.txt.
  check.db(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt", 
           food_records_fn =  "Food_D12_FC_QC_demo_QCed_males60to79_red.txt",
           output_fn =        "Food_D12_FC_QC_demo_QCed_males60to79_red_missing.txt")

# Load the output and check if the output contains anything? 
  mmm = read.table("Food_D12_FC_QC_demo_QCed_males60to79_red_missing.txt", sep="\t", header=T)
  head(mmm)
  # Has item(s) ===> put this missing.txt file in addl_foods_fn argument of MakeFoodTree.
  # Empty       ===> put NULL in addl_foods_fn argument of MakeFoodTree.
  
# Create food tree with the reduced dataset (only reported foods) classified at 
# a desired level of classification (Lv. 1-6).
# "NodeLabelsMCT.txt" has a list of food levels and names, which comes with the DietR package.
  MakeFoodTree(nodes_fn="../../Food_tree_eg/NodeLabelsMCT.txt", 
               addl_foods_fn = NULL,
               num.levels = 3,
               food_database_fn =            "Food_D12_FC_QC_demo_QCed_males60to79_red.txt",  
               output_tree_fn =     "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv.nwk", 
               output_taxonomy_fn = "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv.tax.txt"
  )
  
# --------------------------------------------------------------------------------------------------------------
# Generate OTU tables for downstream analyses; IT MAY TAKE SOME TIME.
# It is OK to see the following warning message:
# In write.table(fiber.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
# appending column names to file.
  
# Make the standard food otu table with data in gram weights of food.
# For the food_records_fn argument, you need to supply 'sel.food.records' file that have 'FoodAmt' column.     
  MakeFoodOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_males60to79.txt",   
              food_record_id =  "SEQN",                              # The ID of your participants
              food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv.tax.txt",       # Your taxonomy file produced by MakeFoodTree.
              output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv.food.otu.txt")  # Output otu file to be saved.
  
# Make a food otu table with data in grams of fiber per food.
  MakeFiberOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_males60to79.txt", 
               food_record_id=   "SEQN", 
               food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv.tax.txt", 
               output_fn=        "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv.fiber.otu.txt")
  
# Make a food otu table as dehydrated grams per kcal.
  MakeDhydrtOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_males60to79.txt", 
                food_record_id =  "SEQN", 
                food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv.tax.txt", 
                output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv.dhydrt.otu.txt")  
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)
  
  
  
  