# ===============================================================================================================
# Wasit 2.
# Generate IFC table.
# Version 1
# Created on 05/19/2023 by Rie Sadohara
# Replaced "OTU" with "IFC" on 06/28/2023 by Rie Sadohara
# Output as comments were updated. 
# ===============================================================================================================

# Set your working directory to the main directory.
# Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

  library(vegan)
  library(reshape2)
  library(data.tree)
  source("lib/specify_data_dir.R")
  source("lib/diversity_nth_tile.R") 

# Load source scripts to build foodtrees and OTU tables.
  source("lib/specify_data_dir.R")
  source("lib/Food_tree_scripts/newick.tree.r")
  source("lib/Food_tree_scripts/make.food.tree.r")
  source("lib/Food_tree_scripts/make.food.ifc.r")
  source("lib/Food_tree_scripts/make.fiber.ifc.r")
  source("lib/Food_tree_scripts/make.dhydrt.ifc.r")

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist2") 

# ===============================================================================================================
# Load totals that has now 3642 individuals.
# ===============================================================================================================

  totals <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3642.txt")
  
  # This should have 3,642 people.    
  nrow(totals)    
  
# Shannon's diversity is one of the measures to evaluate ecological diversity. We will use it to 
# evaluate diet diversity of each individual in the NHANES data.
  
# vegan::diversity calculates Shannon's diversity index one by one.
  
# As an input, we need to have a table with SEQN as rows and food items as columns. So, we will create 
# an IFC table first. 

# ===============================================================================================================
# Load food items and select only items with FoodCode starting with 4  
# ===============================================================================================================
    
# Load the food items data where QC-ed individuals were removed based on their totals data. 
  food <- read.delim("../../Food_D12_FC_QC_demo_QCed.txt", sep= "\t", header=T)
  
# Count the number of unique SEQNs. There should be 4164 people.   
  length(unique(food$SEQN)) 
  
# So need to only keep the individuals that are in totals (n=3642).
  food2<- food[ food$SEQN %in% totals$SEQN, ]

# Should be 3642 SEQNs in food2.
  length(unique(food2$SEQN))
  
# Here, we are interested in food items with their foodcode tarting from 4; nuts/seeds/legumes.  
# Select only the rows that contain those food items. 
  food4s <- subset(food2, Food_code > 39999999 & Food_code < 50000000)
  
  # Check that the subsetted data only contains 4xxxxxxxs.
  summary(food4s$Food_code) 
  
  # Check the summary of the subset data.
  paste(length(unique(food4s$SEQN)), "people consumed",
        nrow(food4s), "food items with duplicates.", 
        "There are", length(unique(food4s$Food_code)), "unique food items.")
  # "1823 people consumed 3464 food items with duplicates. There are 237 unique food items."
  # But for paper, I need to do this AFTER removing an outlier...
  
  # Save as a txt file.
  write.table(food4s, "Food_D12_FC_QC_demo_QCed_n3642_4s.txt", sep= "\t", row.names=F,quote=F)
  
# ===============================================================================================================
# Generate an IFC table
# ===============================================================================================================
  
# To calculate diversity of 4xxxxxxxs for each SEQN, we need to create an IFC table.
  
# ---------------------------------------------------------------------------------------------------------------
# Generate a foodtree and IFC table.
  
  MakeFoodTree(nodes_fn="../../../Food_tree_eg/NodeLabelsMCT.txt", 
               addl_foods_fn = NULL,
               num.levels = 3,
               food_database_fn =            "Food_D12_FC_QC_demo_QCed_n3642_4s.txt",  
               output_tree_fn =     "Foodtree/Food_D12_FC_QC_demo_QCed_n3642_4s_3Lv.nwk", 
               output_taxonomy_fn = "Foodtree/Food_D12_FC_QC_demo_QCed_n3642_4s_3Lv.tax.txt"
  )
  
# --------------------------------------------------------------------------------------------------------------
# Generate IFC tables for downstream analyses; IT MAY TAKE SOME TIME.
# It is OK to see the following warning message:
# In write.table(fiber.ifc, output_fn, sep = "\t", quote = F, append = TRUE) :
# appending column names to file.
  MakeFoodIfc(food_records_fn=  "Food_D12_FC_QC_demo_QCed_n3642_4s.txt",   
              food_record_id =  "SEQN",                              # The ID of your participants
              food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_n3642_4s_3Lv.tax.txt",       # Your taxonomy file produced by MakeFoodTree.
              output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_n3642_4s_3Lv.food.ifc.txt")  # Output ifc file to be saved.
  
# Load the generated IFC table.
  ifc <- read.delim("Foodtree/Food_D12_FC_QC_demo_QCed_n3642_4s_3Lv.food.ifc.txt")
  
# It should have the dimension of number of unique foods x (1 food column + number of people + 1 taxonomy column). 
# 237 x 1825, in this case.
  dim(ifc)  
  
  # The column names have "X." at the beginning. We will take care of it later. 
  ifc[1:4, 1:4]
  
# ---------------------------------------------------------------------------------------------------------------
  

  
  