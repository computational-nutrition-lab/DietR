# ===============================================================================================================
# Generate a food tree with all food items in the FNDDS database. 
# Plus, your own addition such as Soylent.
# This code used to be part of 30_create_food_tree_ASA24.R, but the first part with the entire database has been
# separated to this script.
# Version 1
# Created on 01/18/2023 by Rie Sadohara
# 06/26/2023 replaced "OTU" with "IFC" by Rie Sadohara
# ===============================================================================================================

# This brief script is to serve as an example of formatting and generating food trees with your own dataset. 

# This script demonstrates how to:
# 1. Format food list containing "Main.food.description" and "ModCode" by using the FormatFood function.   
# 2. Build a food tree with the entire list of food items in "all.food.desc.txt".
# 3. Visualize the food tree you have created in step 2.

# The functions in Food_tree_scripts folder expects that the input files are tab-delimited txt file with 
# no special characters that impede correct loading such as:
#  "
#  '
#  #
#  &
# The use of the FormatFoods function in 02_load_clean_ASA24.R script has already dealt with it, but
# It is helpful to know the assumptions which the functions you are going to use were built on.

# Set your working directory as the main directory (dietary_patterns)
Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# Load the packages/scripts necessary for tree building.
  if (!require("reshape2", quietly = TRUE))install.packages("reshape2")

# Load the data.tree package necessary for newick.tree.r, and if it is not installed, install it. 
  if (!require("data.tree", quietly = TRUE))install.packages("data.tree")

# Load functions necessary for foodtree building.
  source("lib/specify_data_dir.R")
  source("lib/Food_tree_scripts/newick.tree.r")
  source("lib/Food_tree_scripts/check.db.r")
  source("lib/Food_tree_scripts/format.foods_2.r")
  source("lib/Food_tree_scripts/filter.db.by.diet.records.r")
  source("lib/Food_tree_scripts/make.food.tree.r") # This needs 'newick.tree.r' already loaded.
  source("lib/Food_tree_scripts/make.food.ifc.r")
  source("lib/Food_tree_scripts/make.fiber.ifc.r")
  source("lib/Food_tree_scripts/make.dhydrt.ifc.r")

# ---------------------------------------------------------------------------------------------------------------
# Load the packages/scripts necessary for tree visualization.
  
# If you have not downloaded and installed the ggtree package yet: 
# You can do so by first installing BiocManager (if you have not done so):
  if (!require("BiocManager", quietly = TRUE))install.packages("BiocManager")
  
# Then, use BiocManager to install the "ggtree" package.
  # BiocManager::install("ggtree")
  
# Load the functions necessary to visualize foodtrees.
  library(ggtree)
  source("lib/viz_food_tree.r")
  
# You can come back to the main directory by:
  setwd(main_wd) 

# ===============================================================================================================
# Load and prepare items data.
# ===============================================================================================================
 
# Food trees show the classification of each food item entered in your dietary data. For details, please read
# Johnson et al., 2021.
  
# Move to "Food_tree_eg" directory.
  SpecifyDataDirectory(directory.name = "eg_data/Food_tree_eg/")

# Replace special characters such as quotation marks, "%", with "_", and create a new FoodID out of foodcode 
# and modcode connected with a period.  The FormatFoods function will leave all other columns intact.
  FormatFoods(input_fn= "all.food.desc.txt", 
              output_fn="all.food.desc_formatted.txt")

# ---------------------------------------------------------------------------------------------------------------
# You can also create a list of FoodCode and Main.food.description of additional foods not in ASA24 that you 
# would like to include in the analysis. As an example, Soylent_codes.txt has two columns: FoodCode 
# and Main.food.description.  You can generate food codes for the food items unique to your dataset. 
# Be sure to use a nine-digit number that is not already in the all.food.desc.txt.

# Format this soylent_codes.txt for use - replace special characters with "_". There are no special characters in 
# this file, so Main.food.description will result in the same as Old.Main.food.description with this particular file.

  FormatFoods(input_fn= "Soylent_codes.txt", 
              output_fn="Soylent_codes_formatted.txt")


# ===============================================================================================================
# Generate a food tree with the whole ASA24 food database
# ===============================================================================================================

# Create a folder called "Food_tree_all_ASA24" within "Food_tree_eg" folder to save the output.
  
# Generate a tree with the whole ASA24 food database first as a reference.
# The file specified by the addl_foods argument will be added to that specified by food_database_fn.
# NodeLabelsMCT.txt has the full classification level of each food items and its Main.food.description.  
# The classification level (num_levels) will be the basis of hierarchical food tree generation. 

    MakeFoodTree(nodes_fn=         "NodeLabelsMCT.txt", 
               food_database_fn= "all.food.desc_formatted.txt", 
               addl_foods_fn=    "Soylent_codes_formatted.txt", 
               num_levels= 4,
               output_tree_fn=    "Food_tree_all_ASA24/ASA24_4Lv.tree.nwk",
               output_taxonomy_fn="Food_tree_all_ASA24/ASA24_4Lv.tax.txt")  

  # nodes_fn:           the food level (node) information for each food item.
  # food_database_fn:   the whole ASA24 database to use. 
  # addl_foods_fn:      additional foods that are not in ASA24 database but you would like to add; 
  #                     soylent_codes in this case.  If none, enter "NULL" instead.
  # num_levels:         the number of food levels (1 - 5) to save.
  # output_tree_fn:     the output tree file name. Should end with ".nwk".
  # output_taxonomy_fn: the output taxonomy file (to be used later) name.
  
# ===============================================================================================================
# Visualize your food tree
# ===============================================================================================================
  
# Load the generated food tree. This will load the .nwk file and save it as a tree object called "tree".
# It is OK to see a message saying: 
# Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
# Also defined by 'tidytree'  
  tree <- read.tree("Food_tree_all_ASA24/ASA24_4Lv.tree.nwk")
  tree
  
# Prepare node labels of L1 for plotting. It assumes that the tree file has 9 L1 levels.
  PrepFoodTreePlots(input.tree=tree)
  
# Create a color-coded and annotated food tree with nine L1 levels.
# Choose either 'circular' or 'radial' for layout.
# It is OK to see some warning messages about Coordinate system and scale for 'y' already being present.
  VizFoodTree(input.tree=tree, layout="circular")

#####
  input.tree=tree
  layout = "radial"
  ggtree()
    ggtree(input.tree, ladderize=F, layout = layout) 
      # geom_text(aes(label=node), hjust= -0.3) +
      geom_hilight(   node=L1nodenum[1],  fill=L1hilightcolors[1]) +  # Milk products
      geom_cladelabel(node=L1nodenum[1], color=  L1labelcolors[1], label=L1nodelabels[1], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[2],  fill=L1hilightcolors[2]) +  # Meat & fish
      geom_cladelabel(node=L1nodenum[2], color=  L1labelcolors[2], label=L1nodelabels[2], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[3],  fill=L1hilightcolors[3]) +  # Eggs
      geom_cladelabel(node=L1nodenum[3], color=  L1labelcolors[3], label=L1nodelabels[3], offset=0.5, geom="label", fill='white', hjust=0.5) +  
      geom_hilight(   node=L1nodenum[4],  fill=L1hilightcolors[4]) +  # Legumes, nuts & seeds
      geom_cladelabel(node=L1nodenum[4], color=  L1labelcolors[4], label=L1nodelabels[4], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[5],  fill=L1hilightcolors[5]) +  # Grain products
      geom_cladelabel(node=L1nodenum[5], color=  L1labelcolors[5], label=L1nodelabels[5], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[6],  fill=L1hilightcolors[6]) +  # Fruits
      geom_cladelabel(node=L1nodenum[6], color=  L1labelcolors[6], label=L1nodelabels[6], offset=0.5, geom="label", fill='white', hjust=0.5) +
      geom_hilight(   node=L1nodenum[7],  fill=L1hilightcolors[7]) +  # Vegetables
      geom_cladelabel(node=L1nodenum[7], color=  L1labelcolors[7], label=L1nodelabels[7], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[8],  fill=L1hilightcolors[8]) +  # Fats & oils
      geom_cladelabel(node=L1nodenum[8], color=  L1labelcolors[8], label=L1nodelabels[8], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[9],  fill=L1hilightcolors[9]) +  # Sweets & beverages
      geom_cladelabel(node=L1nodenum[9], color=  L1labelcolors[9], label=L1nodelabels[9], offset=0.5, geom="label", fill='white', hjust=0.5) 
    
    # Widen the opening of the tree  
    tree_an_hi_o <- open_tree(tree_an_hi, 10)
    
    # Rotate the tree so that the root (break) will come to the bottom
    tree_an_hi_o_rt <- rotate_tree(tree_an_hi_o, 275) # 270 + 10*0.5 
    
    # Rename the tree with a better name.
    annotated_tree <<- tree_an_hi_o_rt
  
#####
    
# Look at the color-coded and annotated food tree, saved as annotated_tree.
  annotated_tree
  
# Save the tree as a PDF file. 
  ggsave("Food_tree_all_ASA24/ASA24_4Lv.tree.pdf",
         annotated_tree, device="pdf", width=6, height=6, units="in", dpi=300)
  
# ---------------------------------------------------------------------------------------------------------------
# You can come back to the main directory by:
  setwd(main_wd) 


