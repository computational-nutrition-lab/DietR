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
  setwd("~/GitHub/DietR")

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
  # glu_3_males60to79 <- read.table("QCtotal_d_glu_body_meta_demo_males60to79_2.txt", 
                               # sep="\t", header=T)
  totals_males60to79 <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79.txt", 
                                   sep="\t", header=T)

# Make the individuals as a vector.
  selectedind <- totals_males60to79$SEQN

# Load the input file (all food record data) to be filtered.
  # all.food.record <- read.table("../Food_D12_FC_cc_f.txt", sep="\t", header=T)
  # all.food.record <- read.table("../Food_D12_FC_cc_f_s.txt", sep="\t", header=T)
  # all.food.record <- read.table("../Food_D12_FC_QC_demo_QCed.txt", sep="\t", header=T)
  all.food.record <- read.table("../Food_D12_FC_QC_demo_QCed_w_FoodID.txt", sep="\t", header=T)

# Select only the individuals listed in 'selectedind'. 
  sel.food.record <- all.food.record[all.food.record$SEQN %in% selectedind, ]

# Confirm the two contains the same set of individuals. 
  identical(unique(sel.food.record$SEQN), selectedind)

# Save. This will be the input for the following procedures.
# write.table(sel.food.record, "Food_D12_FC_cc_f_males60to79_2.txt", # Deleted _2 ...  
  # write.table(sel.food.record, "Food_D12_FC_cc_f_s_males60to79.txt", 
  write.table(sel.food.record, "Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID.txt", 
              sep="\t", row.names=F, quote=F) 

# ===============================================================================================================
# Limit to just the foods reported in your study (formatted dietrecords.txt as the input) 
# ===============================================================================================================
# Keep only the foods reported in your study. This is to make data compatible to create a food tree.
  # FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt", 
  #                       food_records_fn  = "Food_D12_FC_cc_f_males60to79.txt",   # output of FormatFoods.  
  #                       output_fn =        "Food_D12_FC_cc_f_males60to79_red.txt")
  
  # FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt", 
  #                       food_records_fn  = "Food_D12_FC_cc_f_s_males60to79.txt",   # output of FormatFoods.  
  #                       output_fn =        "Food_D12_FC_cc_f_s_males60to79_red.txt")

  FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt",
                        food_records_fn  = "Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID.txt",   # (output of FormatFoods)(?).
                        output_fn =        "Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID_red.txt")
  
  ###
  reduced = read.delim("Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID_red.txt")
  reduced = read.delim("Food_D12_FC_cc_f_s_males60to79_red.txt")
  dim(reduced)
  head(reduced)
  length(unique(reduced$FoodCodes))
  length(unique(reduced$FoodID))
  
  records_QCed = read.delim('Food_D12_FC_QC_demo_QCed_males60to79.txt')
  records_s = read.delim('Food_D12_FC_cc_f_s_males60to79.txt')
  dim(records_QCed) #7603  151
  dim(records_s)    #7603  131
  write.table(colnames(records_QCed), 'clipboard', sep="\t", quote=F, row.names=F)
  write.table(colnames(records_s), 'clipboard', sep="\t", quote=F, row.names=F)
  colnames(records_s)    
  
  
  head(records)
  dim(output) # 1274    6
  head(output)
  length(unique(records$SEQN)) #1678
  head(unique(records$Food_code)[order(unique(records$Food_code))])
  length(unique(output$FoodID)) # 1274
  head(unique(output$FoodID))
  # So, FilterDbByDietRecords reduced the number of foods from 1678 to 1274.
  # Let's check what the function is doing.
  View(FilterDbByDietRecords)
  
  database = read.delim('../../Food_tree_eg/NHANESDatabase.txt')
  valid_ids <- intersect(database$FoodID, records$FoodID)
  
  Food_D12_FC_QC_demo_QCed <- read.delim('../Food_D12_FC_QC_demo_QCed.txt')
  dim(Food_D12_FC_QC_demo_QCed)  # 128578 x 151!
  length(unique(Food_D12_FC_QC_demo_QCed$SEQN)) # 4207, filtered after QC totals.
  length(unique(Food_D12_FC_QC_demo_QCed$)) # 4207, filtered after QC totals.

  ###
  
# Use CheckDB function to ensure no food reported Food_D12_FC_cc_f.txt in is missing in the database.

# Check if there is any food item reported by people but are missing in the database. 
  check.db(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt", 
           # food_records_fn =  "Food_D12_FC_cc_f_males60to79_red.txt", 
           # food_records_fn =  "Food_D12_FC_cc_f_s_males60to79_red.txt", 
           food_records_fn =  "Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID_red.txt",
           output_fn =        "Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID_red_missing.txt")

# Load the output and check if the output contains anything? 
  mmm = read.table("Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID_red_missing.txt", sep="\t", header=T)
  head(mmm)
  # Has something ===> put this missing.txt file in addl_foods_fn argument of MakeFoodTree.
  # Empty         ===> put NULL in addl_foods_fn argument of MakeFoodTree.

# Create food tree with the reduced dataset (only reported foods) classified at 
# a desired level of classification (Lv. 1-6).
# "NodeLabelsMCT.txt" has a list of food levels and names, which comes with this package.
  MakeFoodTree(nodes_fn="../../Food_tree_eg/NodeLabelsMCT.txt", 
               addl_foods_fn = NULL,
               num.levels = 3,
               food_database_fn =            "Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID_red.txt",  
               output_tree_fn =     "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID_red.nwk", 
               output_taxonomy_fn = "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_w_FoodID_red.taxonomy.txt"
  )


# --------------------------------------------------------------------------------------------------------------
# Generate OTU tables for downstream analyses; IT MAY TAKE SOME TIME.
# It is OK to see the following warning message:
# In write.table(fiber.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
# appending column names to file.

# Make the standard food otu table with data in gram weights of food.
# MakeFoodOtu(food_records_fn=  "Food_D12_FC_cc_f_males60to79.txt",  # need to supply the original data that have 'FoodAmt' before FilterDBByDietRecords.  
  MakeFoodOtu(food_records_fn=  "Food_D12_FC_cc_f_s_males60to79.txt",  # need to supply the original data that have 'FoodAmt' before FilterDBByDietRecords.  
              food_record_id =  "SEQN",                              # The ID of your participants
              food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_s_males60to79_red_Lv3.taxonomy.txt",  # Your taxonomy file produced by MakeFoodTree.
              output_fn =       "Foodtree/Food_D12_FC_cc_f_s_males60to79_red_Lv3.food.otu.txt")  # Output otu file to be saved.

# Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "Food_D12_FC_cc_f_s_males60to79.txt", 
               food_record_id=   "SEQN", 
               food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_s_males60to79_red_Lv3.taxonomy.txt", 
               output_fn=        "Foodtree/Food_D12_FC_cc_f_s_males60to79_red_Lv3.fiber.otu.txt")

# Make a food otu table as dehydrated grams per kcal
  MakeDhydrtOtu(food_records_fn=  "Food_D12_FC_cc_f_males60to79.txt", 
                food_record_id =  "SEQN", 
                food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_s_males60to79_red_Lv3.taxonomy.txt", 
                output_fn =       "Foodtree/Food_D12_FC_cc_f_s_males60to79_red_Lv3.dhydrt.otu.txt")  

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)
  
  

