# ===============================================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 2 - cleaner veresion with just 'Users' plot.
# Created on 03/25/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# If you have not downloaded and installed the phyloseq package yet: 
# You can do so by first installing BiocManager (if you have not done so):
  if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
  
# Then download and install the phyloseq package.
  BiocManager::install("phyloseq")

# ---------------------------------------------------------------------------------------------------------------
# load the necessary packages.
  library(phyloseq)
  library(ggtree)

# Load necessary functions and ggplot formatting themes
  source("lib/specify_data_dir.R")
  source("lib/ordination.R")
  source("lib/ggplot2themes.R")
  source("lib/plot.axis.1to4.by.factor.R")

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("lib/distinct100colors.rda")  

# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Create a phyloseq object for ordination.
# ===============================================================================================================
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")
  
# Load the necessary files for creating a phyloseq object.  
  
# Food
  # Load food OTU table - this is our food OTU data
  food <- read.delim("Foodtree/VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.food.otu.txt", row.names = 1)
  
  # "food" is a matrix of Food descriptions (rows) x SampleID (columns).
  head(food)
  
  # Format the food file and create a otu_table called OTU.
  PrepFood(data= food)
  
# Taxonomy (tax)
  tax <- read.delim("Foodtree/VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tax.txt")
  
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data= tax)
  
# Sample
  meta <- read.table( "ind_metadata_UserxDay.txt", sep="\t", header=T)
  
  # Format the metadata file and save it as 'SAMPLES'. 
  PrepMeta(data= meta)

# Food tree
  foodtree <- read_tree("Foodtree/VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tree.nwk")
  # It is OK to see a message saying that
    # "Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
    # Also defined by 'tidytree'".
  
  # Format food tree and save it as 'TREE'. 
  PrepTree(data=foodtree)
  # It is OK to see the same message as the previous line. 

# ---------------------------------------------------------------------------------------------------------------
# Make a phyloseq object with OTU, TAX, samples, and foodtree.
  phyfoods <- phyloseq(OTU, TAX, SAMPLES, TREE)
  # It is OK to see a message (or multiple of them) saying that
    # Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
    # Also defined by 'tidytree'.

# Check your metadata
  # Show the sample names and ensure they are vvkaj.00xxx. 
  sample_names(phyfoods)
  
# Show metadata. 
  head(sample_data(phyfoods), n=3)
  
# Check the level 1 foods in your food tree. There should be nine categories.
  L1s = tax_table(phyfoods)[, "L1"]
  as.vector(unique(L1s))

# ===============================================================================================================
# Use your phyloseq object and perform ordination 
# ===============================================================================================================

# Change to the folder called "Ordination" in your "VVKAJ" folder.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Ordination/")
  
# Perform Principal Coordinate Analysis (PCoA) with weighted unifrac distance of your food data.
# Ordination by UNweighted unifrac distances can be done by having the "weighted" argument as FALSE. 
# This may take a few minutes depending on your data size.
# e.g. a large phyloseq object (7.9 MB) takes ~ 1 min. 
  ordinated <- phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=TRUE) 

# # Save the percent variance explained by the axes as a vector to use in plots.  
#   eigen_percent <- ordinated$values$Relative_eig

# Save the percent variance explained as a txt file.
  Eigen(eigen.input = ordinated$values$Relative_eig, 
        output.fn="4Lv_ord_WEIGHTED_eigen_percent.txt")

# Merge the first n axes to the metadata and save it as a txt file. 
# This will be used for plotting ordination results.
  MergeAxesAndMetadata(ord.object=ordinated, number.of.axes=10, meta.data= meta, 
                       output.fn= "4Lv_ord_WEIGHTED_meta_users.txt")

  
# ===============================================================================================================
# Plot your ordination results 
# ===============================================================================================================
  
# Read in the eigenvalues for axis labels of biplots.
  eigen_loaded <- read.table("4Lv_ord_WEIGHTED_eigen_percent.txt", header=T)
  
# Make a vector that contains the variance explained.
  eigen_loaded_vec <- eigen_loaded[, 2]
  
# Read in the metadata and users' Axis values. 
  meta_usersdf <- read.table("4Lv_ord_WEIGHTED_meta_users.txt", header=T)    
  
# Take a look at meta_usersdf that has been loaded. 
  head(meta_usersdf, 3)
  
# ---------------------------------------------------------------------------------------------------------------
# Save Axes 1 & 2, 1 & 3, 2 & 3, 3 & 4, 2 & 4 biplots with and without ellipses with specified confidence interval.
# The results are saved with filenames with the specified "prefix_AxisXY.pdf" or "prefix_AxisXY_ellipses.pdf".
# You need to supply the same number of colors in the order of the factor level to be used. 
# dot.colors are for datapoints, and ellipses.colors are for ellipses outlines. 
  PlotAxis1to4ByFactor(axis.meta.df    = meta_usersdf, 
                       factor.to.color = "Diet", 
                       eigen.vector    = eigen_loaded_vec,
                       dot.colors      = distinct100colors, 
                       ellipses.colors = distinct100colors,  
                       ellipses.cflevel = 0.95,
                       out.prefix = "4Lv_ord_WEIGHTED_diet"
  )  
  
# ---------------------------------------------------------------------------------------------------------------
# Some of the Diet groups seem to form distinct clusters. Use beta-diversity and adonis tests 
# to see if they are actually distinct from one another.

  
# Generate a weighted unifrac distance matrix.
  dist_matrix <- phyloseq::distance(phyfoods, method = "wunifrac")
  
# Dispersion test and plot
# vegan::betadisper computes centeroids and distance of each datapoint from it. 
  dispr <- vegan::betadisper(d=dist_matrix, phyloseq::sample_data(phyfoods)$Diet)
  
# Show the centroids and dispersion of each group. 
  plot(dispr)
  
# Use dispr to do a permutation test for homogeneity of multivariate dispersion
  vegan::permutest(dispr, perm=5000)
  # If p>0.05, the dispersion of each group are not different, and the assumption for adonis is met.
  
# Use adonis to test whether there is a difference between groups' composition. 
# i.e., composition among groups (food they consumed) is similar or not.
  vegan::adonis(dist_matrix ~ phyloseq::sample_data(phyfoods)$Diet, permutations = 5000)
  
# If overall adonis is significant, which is true in this case,  
# you can run pairwise adonis to see which group pairs are different.
  pairwise.adonis(dist_matrix, phyloseq::sample_data(phyfoods)$Diet, perm = 5000)    

  
# ===============================================================================================================
# Save unifrac distance (unweighted or weighted) matrices
# ===============================================================================================================
# Generate and save a weighted unifrac distance matrix of "Samples". 
  WeightedUnifracDis(input.phyloseq.obj = phyfoods, 
                     output.fn = "4Lv_WEIGHTED_uni_dis.txt")        
  
# Generate and save an unweighted unifrac distance matrix of "Samples". 
  UnweightedUnifracDis(input.phyloseq.obj = phyfoods, 
                       output.fn = "4Lv_UNweighted_uni_dis.txt")        
  
##### TUTORIAL UPDATED! 12/05/2022.
  
#     
# # ===============================================================================================================
# # NEW PAGE, NEW SCRIPT
# # Plot your ordination results 
# # ===============================================================================================================
# 
# # Set your working directory to the main directory.
#   Session --> Set working directory --> Choose directory.
#   setwd("~/GitHub/DietR")
#   
# # Name your main directory for future use. 
#   main_wd <- file.path(getwd())
# 
# # You can come back to the main directory by:
#   setwd(main_wd)
#   
# # ---------------------------------------------------------------------------------------------------------------
# # # If you have not downloaded and installed the phyloseq package yet: 
# # # You can do so by first installing BiocManager (if you have not done so):
# #   if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
# #   
# # # Then download and install the phyloseq package.
# #   BiocManager::install("phyloseq")
# # # ---------------------------------------------------------------------------------------------------------------
#   
# # load the necessary packages.
#   # library(phyloseq)
#   library(ggplot2)
#   # library(ggtree)
#   
# # Load necessary functions and ggplot formatting themes
#   source("lib/specify_data_dir.R")
#   # source("lib/ordination.R")
#   source("lib/ggplot2themes.R")
#   
# # Load the distinct 100 colors for use.   
#   distinct100colors <- readRDS("lib/distinct100colors.rda")
#   
# # # ---------------------------------------------------------------------------------------------------------------
# # Change to the folder called "Unifrac" in your "VVKAJ" folder.
#   SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Unifrac/")
#   
# # Read in the metadata and users' Axis values. 
# # meta_usersdf_loaded <- read.table("results/ordinated_weighted_axes_meta_MCT.txt", header=T)
#   meta_usersdf <- read.table("4Lv_ordinated_Weighted_meta_users.txt", header=T)
# 
# # Take a look at meta_usersdf_loaded. 
#   head(meta_usersdf,3)
# 
# # Read in the eigenvalues for axis labels of biplots.
#   eigen_loaded <- read.table("4Lv_ordinated_weighted_eigen_percent.txt", header=T)
#   
# # Plot Axis 1 and Axis 2 to show the separation of samples colored by UserName, gender, timing, etc. as in the metadata.
# # By UserName.
#   by_user <- ggplot(meta_usersdf, aes(x=Axis.1, y=Axis.2, color=UserName)) +
#           geom_point(aes(color=UserName), size=2) + 
#           scale_color_manual(values = distinct100colors) + # OR use viridis theme.
#           # scale_color_viridis_d() +
#           xlab( paste("Axis.1 (", paste(round(eigen_loaded[1,2]*100, 1)), "%)", sep="") ) +
#           ylab( paste("Axis.2 (", paste(round(eigen_loaded[2,2]*100, 1)), "%)", sep="") ) +
#           no_grid + space_axes + theme(aspect.ratio = 1)
#   by_user
#   
# # Save by_user plot as a PDF. 
#   ggsave("4Lv_ordinated_Weighted_Axis12_users.pdf", by_user,
#          device="pdf", height=6, width=6, unit="in", dpi=300)
#   
# # Add lines to connect samples in the order in which they appear in the data using geom_path. 
# # [NOTE] geom_line connects in the order of the variable (small to large) on the x axis, 
# # so it could be misleading.
#   by_user_pathconnected <- by_user + geom_path(aes(color = UserName))  
#   by_user_pathconnected
#   
#   # Save pathconnected as a PDF. 
#   ggsave("4Lv_ordinated_Weighted_Axis12_users_pathconnected.pdf", 
#          by_user_pathconnected, device="pdf", height=6, width=6, unit="in", dpi=300)
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Color individuals by diet (Vegan, Vegetarian, Keto, American, and Japanese).
#   by_diet <- ggplot(meta_usersdf, aes(x=Axis.1, y=Axis.2, color=Diet)) +
#     geom_point(aes(color=Diet), size=2) + 
#     scale_color_manual(values = distinct100colors) + # OR use viridis theme.
#     # scale_color_viridis_d() +
#     xlab( paste("Axis.1 (", paste(round(eigen_loaded[1,2]*100, 1)), "%)", sep="") ) +
#     ylab( paste("Axis.2 (", paste(round(eigen_loaded[2,2]*100, 1)), "%)", sep="") ) +
#     no_grid + space_axes + theme(aspect.ratio = 1)
#   by_diet
#   
# # You can add ellipses at a desired confidence level. 
#   by_diet_ellipses <- by_diet + stat_ellipse(level=0.95) 
#   by_diet_ellipses
#   
# # Save by_user_diet_ellipses as a PDF. 
#   ggsave("4Lv_ordinated_Weighted_Axis12_diet_ellipses.pdf", by_diet_ellipses, 
#          device="pdf", height=6, width=6, unit="in", dpi=300)
# 
# # Make polygons by diet.
#   by_diet_polygons <- by_diet + geom_polygon(aes(fill = Diet)) + 
#     geom_point(aes(color = Diet), size=2) + 
#     scale_fill_manual(values = distinct100colors)
#   by_diet_polygons
#   
#   # Save polygons as a PDF. 
#   ggsave("4Lv_ordinated_Weighted_Axis12_diet_polygons.pdf", 
#          by_diet_polygons, device="pdf", height=6, width=6, unit="in", dpi=300)
# 
# 
# # ===============================================================================================================
# # Change colors for specific user(s) 
# # ===============================================================================================================
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # [A] Highlight one sample with others being grey.  
#   # Specify which user to highlight; e.g. VVKAJ101.
#   select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 
# 
#   panelA <- 
#     by_user + geom_point(size=2, color="grey") +  
#     geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=2) 
#   panelA
#   
# # Save the panel as a PDF. 
#   ggsave("4Lv_ordinated_Weighted_Axis12_users_VVKAJ101_grey.pdf", 
#          panelA, device="pdf", height=6, width=6, unit="in", dpi=300)
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # [B] Highlight multiple samples with others being grey.
# # Specify colors for each samples to be highlighted in the scale_color_manual argument.
#   select_points <- subset(meta_usersdf, UserName=="VVKAJ101" | UserName=="VVKAJ106" )
# 
#   panelB <- 
#     by_user + geom_point(data=select_points, aes(x=Axis.1, y=Axis.2, color=as.factor(UserName))) +
#     scale_color_manual(values = c("VVKAJ101"="red", "VVKAJ106"="blue")) 
#   # It is OK to see a message: "Scale for 'colour' is already present. 
#   # Adding another scale for 'colour', which will replace the existing scale."
#   panelB
#   
# # Save the panel as a PDF. 
#   ggsave("4Lv_ordinated_Weighted_Axis12_users_VVKAJ101_106_grey.pdf", 
#          panelB, device="pdf", height=6, width=6, unit="in", dpi=300)
#   
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # [C] Highlight one sample; other points will retain their original colors. 
#   select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 
# 
# # Changing the shape sizes might help find the dots. Note that points may be overlapping
#   panelC <- 
#     by_user + geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) 
#   panelC
#   
# # Save the panel as a PDF. 
#   ggsave("4Lv_ordinated_Weighted_Axis12_users_VVKAJ101_color.pdf", 
#          panelC, device="pdf", height=6, width=6, unit="in", dpi=300)
#   
#     
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # [D] Highlight multiple samples; other points will retain their original colors. 
#   select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 
#   select_point_2 <- subset(meta_usersdf, UserName=="VVKAJ106") 
# 
#   panelD <- 
#     by_user + 
#     geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) +
#     geom_point(data=select_point_2, aes(x=Axis.1, y=Axis.2), color="green", size=4) 
#   panelD
#   
# # Save the panel as a PDF. 
#   ggsave("4Lv_ordinated_Weighted_Axis12_users_VVKAJ101_106_color.png", 
#          panelD, device="png", height=6, width=6, unit="in", dpi=300)
#   
#   
