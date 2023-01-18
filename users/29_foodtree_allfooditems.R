# ===============================================================================================================
# Generate a food tree with all food items in the FNDDS database. 
# Plus, your own addition such as Soylent.
# This code used to be part of 30_create_food_tree_ASA24.R, but the first part with the entire database has been
# separated to this script.
# Version 1
# Created on 01/18/2023 by Rie Sadohara
# ===============================================================================================================

# This script explains how to:
# 1. format food list containing "Main.food.description" and "ModCode" by using the FormatFood function.   
# 2. build a food tree with the entire list of food items in "all.food.desc.txt".
# 3. visualize the food tree you have created in step 2.

# This brief script is to serve as an example of formatting and generating food trees with your own dataset. 


# Set your working directory as the main directory (dietary_patterns)
Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# Load the packages/scripts necessary for tree building.
  
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

# ---------------------------------------------------------------------------------------------------------------
# Load the packages/scripts necessary for tree visualization.
  
# If you have not downloaded and installed the ggtree package yet: 
# You can do so by first installing BiocManager (if you have not done so):
  if (!require("BiocManager", quietly = TRUE))install.packages("BiocManager")
  
# Then, use BiocManager to install the "ggtree" package.
  BiocManager::install("ggtree")
  
# Load the functions necessary to set directories.
  source("lib/specify_data_dir.R")
  source("lib/viz_food_tree.r")
  
# You can come back to the main directory by:
  setwd(main_wd) 

# ===============================================================================================================
# Load and prepare items data.
# ===============================================================================================================
 
# Move to "Food_tree_eg" directory.
  SpecifyDataDirectory(directory.name = "eg_data/Food_tree_eg/")
  
# Food trees show the classification of each food item entered in your dietary data. For details, please read
# Johnson et al., 2021.

# Create a new folder called "Foodtree" in the "VVKAJ" folder.
# Prepare input data
# Current ASA24 database doesn't have modcodes, so de-duplicate database file, 
# replace special characters with "_", and create a new FoodID out of foodcode and modcode.  
# It will leave all other columns intact.
  FormatFoods(input_fn= "all.food.desc.txt", 
              output_fn="ASA24Database.txt")

# ---------------------------------------------------------------------------------------------------------------
# You can also create a list of FoodCode and Main.food.description of additional foods not in ASA24 that you 
# would like to include in the analysis. As an example, Soylent_codes.txt has two columns: FoodCode 
# and Main.food.description.  You can generate food codes for the food items unique to your dataset. 
# Be sure to use a nine-digit number that is not already in the all.food.desc.txt.

# Format this soylent_codes.txt for use - replace special characters with "_". There are no special characters in 
# this file, so Main.food.description will result in the same as Old.Main.food.description with this particular file.

  FormatFoods(input_fn= "Soylent_codes.txt", 
              output_fn="Soylent_codes_formatted.txt")

# ---------------------------------------------------------------------------------------------------------------

# # After preparing those files, specify the directory where the items data is.
#   SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")

# # Format your items data, and save the formatted items file to the "Foodtree" folder.  Similarly, the special characters in Main.food.description column in the items file will be replaced with "_".
#   FormatFoods(input_fn= "VVKAJ_Items_f_id_s_m.txt", 
#               output_fn="Foodtree/VVKAJ_Items_f_id_s_m_ff.txt", dedupe=F)

# ===============================================================================================================
# Generate a food tree with the whole ASA24 food database
# ===============================================================================================================

# Create a folder called "Food_tree_all_ASA24" within "Food_tree_eg" folder to save the output.
  
# Generate a tree with the whole ASA24 food database first as a reference.
# The file specified by the addl_foods argument will be added to ASA24Database.txt.

  MakeFoodTree(nodes_fn=         "NodeLabelsMCT.txt", 
               food_database_fn= "ASA24Database.txt", 
               addl_foods_fn=    "Soylent_codes_formatted.txt", 
               num.levels= 3,
               output_taxonomy_fn="Food_tree_all_ASA24/ASA24_3Lv.taxonomy.txt",  
               output_tree_fn=    "Food_tree_all_ASA24/ASA24_3Lv.tree.nwk")

# The nodes_fn argument specifies the food level (node) information for each fCood item.
# NodeLabelsMCT.txt has the classification level of each food items and its Main.food.description.  
# The classification level will be the basis of hierarchical food tree generation. 

  # The food_database_fn specifies the whole ASA24 database to use. 
  # The addl_foods_fn specifies additional foods that are not in ASA24 database but you would like to add; 
  # soylent_codes in this case.  If none, enter "NULL" instead.
  # The num.levels means the number of food levels (1 - 5) to save.
  # The output_taxonomy_fn specifies the output taxonomy file (to be used later) name. Should end with ".txt".
  # The output_tree_fn specifies the output tree file name. Should end with ".nwk".
  
# ===============================================================================================================
# Visualize your food tree
# ===============================================================================================================
  
# Load the generated food tree. This will load the .nwk file and save it as a tree object called "tree".
# It is OK to see a message saying: 
# Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
# Also defined by 'tidytree'  
  tree <- read.tree("Food_tree_all_ASA24/ASA24_3Lv.tree.nwk")
  tree
  
# Prepare node labels of L1 for plotting. It assumes that the tree file has 9 L1 levels.
  PrepFoodTreePlots(input.tree=tree)
  
# Create a color-coded and annotated food tree with 9 L1 levels.
# Choose either 'circular' or 'radial' for layout.
# It is OK to see some warning messages about Coordinate system and scale for 'y' already being present.
  VizFoodTree(input.tree=tree, layout="radial")
  
# Look at the color-coded and annotated food tree, saved as annotated_tree.
  annotated_tree
  
# Save the tree as a PDF file. 
  ggsave("Food_tree_all_ASA24/ASA24_3Lv.tree.png", 
         annotated_tree, device="png", width=6, height=6, units="in", dpi=300)
  
  
  
  
  
  


