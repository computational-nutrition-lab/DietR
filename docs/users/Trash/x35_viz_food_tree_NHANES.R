# ===============================================================================================================
# Generate food tree out of GLU - males in their 50s
# Version 1
# Created on 08/29/2022 by Rie Sadohara
# ===============================================================================================================

# READY TO BE COPIED TO TUTORIAL =====COPIED ON 08/30/2022====

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# Load source scripts
  source("lib/specify_data_dir.R")
  source("lib/viz_food_tree.r")

# You can come back to the main directory by:
  setwd(main_wd)
  
# ===============================================================================================================
# Visualize food tree.
# ===============================================================================================================
  
# Specify where your data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data/Foodtree")

# Load your tree object.
  tree <- read.tree("Food_D12_FC_cc_f_males50s_red_Lv4.nwk")

# It is OK to see an error that says:
# Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
# Also defined by 'tidytree'

# Prepare the tree data for visualization.
  PrepFoodTreePlots(input.tree=tree)

# Create a color-coded and annotated food tree with 9 L1 levels.
# Choose either 'circular' or 'radial' for layout.
# It is OK to see some warning messages about Coordinate system and scale for 'y' already being present.
  VizFoodTree(input.tree=tree, layout="radial")  

# Take a look at the tree.
  annotated_tree

# Save.
  ggsave("Food_D12_FC_cc_f_males50s_red_Lv4_viz.pdf", annotated_tree,
         device="pdf", width=5.2, height=5)

  
# --------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)
  
  