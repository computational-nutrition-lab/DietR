# ===============================================================================================================
# Plot a food tree generated with dietary data.
# Version 2
# Created on 08/30/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# You can come back to the main directory by:
  setwd(main_wd)

# If you have not downloaded and installed the ggtree package yet: 
# You can do so by first installing BiocManager (if you have not done so):
  if (!require("BiocManager", quietly = TRUE))install.packages("BiocManager")
  
# Then, use BiocManager to install the "ggtree" package.
  BiocManager::install("ggtree")

# Load the functions necessary to set directories.
  source("lib/specify_data_dir.R")
  source("lib/viz_food_tree.r")
  
# ---------------------------------------------------------------------------------------------------------------
# Go to the "Foodtree" directory where the tree files are saved. 
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Foodtree")
  
# Load the generated food tree. This will load the .nwk file and save it as a tree object called "tree".
# It is OK to see a message saying: 
# Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
# Also defined by 'tidytree'  
  tree <- read.tree("VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tree.nwk")
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
  ggsave("VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tree.pdf", 
         annotated_tree, device="pdf", width=6, height=6, units="in", dpi=300)
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)  
  
  