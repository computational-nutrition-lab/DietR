# ===============================================================================================================
# Generate food tree out of totals with cholesterol data with food tree level 3.
# Version 1
# Created on 01/27/2023 by Rie Sadohara
# ===============================================================================================================

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# Load source scripts
  source("lib/specify_data_dir.R")
  source("lib/viz_food_tree.r")

# You can come back to the main directory by:
  setwd(main_wd)
  
# ===============================================================================================================
# Visualize your food tree.
# ===============================================================================================================
  
# Specify where your data is.
  SpecifyDataDirectory("eg_data/NHANES/PF/Foodtree/")

# Load your tree object.
  tree <- read.tree("Food_D12_FC_QC_demo_QCed_leg_3Lv.nwk")

# It is OK to see an error that says:
# Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
# Also defined by 'tidytree'
  ggtree(tree, layout = "circular", ladderize = T)
  
  View(ggtree)
  
# Cannot display the names, but can use an online tool for visualization. 
# anyway .nwk is all that's needed...
    
# Prepare the tree data for visualization.
  PrepFoodTreePlots(input.tree = tree)
  
#### The functions below assumes that there are L1-L9 levels of food items, so cannot process
  # this dataset with only a subset of legume-containing foods. 
  
# Create a color-coded and annotated food tree with 9 L1 levels.
# Choose either 'circular' or 'radial' for layout.
# It is OK to see some warning messages that say: 
  # Coordinate system already present. Adding new coordinate system, which will replace the existing one.
  # Scale for 'y' is already present. Adding another scale for 'y', which will replace the existing scale.
  VizFoodTree(input.tree=tree, layout="radial")  

# Take a look at the tree.
  annotated_tree

# Save.
  ggsave("Food_D12_FC_QC_demo_QCed_Leg_red_3Lv_viz.png", annotated_tree,
         device="png", width=5.2, height=5)

# --------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)
  
  