# ===============================================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 2 - cleaner veresion with just 'Users' plot.
# Created on 03/25/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# You can come back to the main directory by:
  setwd(main_wd)

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
  library(ggplot2)

# Load necessary functions and ggplot formatting themes
  source("lib/specify_data_dir.R")
  source("lib/ordination.R")
  source("lib/ggplot2themes.R")

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("lib/distinct100colors.rda")
  
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
    # Also defined by 'tidytree'"
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
  # Show the sample names. 
  sample_names(phyfoods)  
  
  # Show metadata. 
  head(sample_data(phyfoods), n=3)
  
  # Show only the columns of metadata. 
  sample_variables(phyfoods)

# Check the level 1 foods in your food tree 
  L1s = tax_table(phyfoods)[, "L1"]
  as.vector(unique(L1s))

# ===============================================================================================================
# Use your phyloseq object and perform ordination 
# ===============================================================================================================

# Perform Principal Coordinate Analysis (PCoA) with weighted unifrac distance of your food data.
# This may take a few minutes depending on your data size.
# e.g. a large phyloseq object (7.9 MB) takes ~ 1 min. 
  ordinated <- phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=TRUE) 

# Save the percent variance explained by the axes as a vector to use in plots.  
  eigen_percent <- ordinated$values$Relative_eig

# Save the percent variance explained as a txt file.
  # Eigen(eigen.input = eigen_percent, output.fn="results/ordinated_weighted_eigen_percent_mct_4Lv.txt")
  Eigen(eigen.input = eigen_percent, output.fn="Unifrac/4Lv_ordinated_weighted_eigen_percent.txt")

# ===============================================================================================================
# Save unifrac distance (unweighted or weighted) matrices. 
# ===============================================================================================================

# Generate and save an unweighted unifrac distance matrix of "Samples". 
  UnweightedUnifracDis(input.phyloseq.obj = phyfoods, 
                       output.fn = "Unifrac/4Lv_UNweighted_uni_dis.txt")        

# ---------------------------------------------------------------------------------------------------------------
# Generate and save an weighted unifrac distance matrix of "Samples". 
  WeightedUnifracDis(input.phyloseq.obj = phyfoods, 
                     output.fn = "Unifrac/4Lv_WEIGHTED_uni_dis.txt")        


# ===============================================================================================================
# Plot your ordination results 
# ===============================================================================================================

# Change to the folder called "Unifrac" in your "VVKAJ" folder.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Unifrac/")
  
# Merge the first n axes to the metadata and save it as a txt file. 
# The merged dataframe, 'meta_usersdf', will be used for plotting.
  MergeAxesAndMetadata(ord.object=ordinated, number.of.axes=10, meta.data= meta, 
                       # output.fn= "results/ordinated_weighted_axes_meta_MCT.txt")
                       output.fn= "4Lv_ordinated_Weighted_meta_users.txt")

# Read in the metadata and users' Axis values. 
# meta_usersdf_loaded <- read.table("results/ordinated_weighted_axes_meta_MCT.txt", header=T)
  meta_usersdf_loaded <- read.table("4Lv_ordinated_Weighted_meta_users.txt", header=T)

# Take a look at meta_usersdf_loaded. 
  head(meta_usersdf_loaded,3)

# Plot Axis 1 and Axis 2 to show the separation of samples colored by UserName, gender, timing, etc. as in the metadata.
  p1 <- ggplot(meta_usersdf, aes(x=Axis.1, y=Axis.2, color=UserName)) +
          geom_point(aes(color=UserName), size=2) + 
          scale_color_manual(values = distinct100colors) + # OR use viridis theme.
          # scale_color_viridis_d() +
          xlab( paste("Axis.1 (", paste(round(eigen_percent[1]*100, 1)), "%)", sep="") ) +
          ylab( paste("Axis.2 (", paste(round(eigen_percent[2]*100, 1)), "%)", sep="") ) +
          no_grid + space_axes + theme(aspect.ratio = 1)
  p1
  
# Save p1 as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_p1.pdf", 
         p1, device="pdf", height=6, width=6, unit="in", dpi=300)

# You can add ellipses at a desired confidence level; but with this 
# example data, there are too few samples per user to draw them. 
  ellipses <- p1 + stat_ellipse(level=0.95) 
  ellipses
  
# Save ellipses as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_ellipses.pdf", 
         ellipses, device="pdf", height=6, width=6, unit="in", dpi=300)
  

# Add lines to connect samples in the order in which they appear in the data. 
# Note that geom_line option, not geom_path, connects in the order of the variable 
# on the x axis, so it could be misleading.
  pathconnected <- p1 + geom_path(aes(color = UserName))  
  pathconnected
  
# Save pathconnected as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_pathconnected.pdf", 
         pathconnected, device="pdf", height=6, width=6, unit="in", dpi=300)

# Make a polygon by UserName
# Could be messy with overlapping clusters and/or too many samples.
  polygons <- p1 + geom_polygon(aes(fill = UserName)) + 
    geom_point(aes(color=UserName), size=2) + 
    scale_fill_manual(values=distinct100colors)
  polygons
  
# Save polygons as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_polygons.pdf", 
         polygons, device="pdf", height=6, width=6, unit="in", dpi=300)
  
      
# ===============================================================================================================
# Change colors for specific user(s) 
# ===============================================================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [A] Highlight one sample with others being grey.  
  # Specify which user to highlight; e.g. VVKAJ101.
  select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 

  panelA <- 
    p1 + geom_point(size=2, color="grey") +  
    geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=2) 
  panelA
  
# Save the panel as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_p1_VVKAJ101_grey.pdf", 
         panelA, device="pdf", height=6, width=6, unit="in", dpi=300)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [B] Highlight multiple samples with others being grey.
# Specify colors for each samples to be highlighted in the scale_color_manual argument.
  select_points <- subset(meta_usersdf, UserName=="VVKAJ101" | UserName=="VVKAJ106" )

  panelB <- 
    p1 + geom_point(data=select_points, aes(x=Axis.1, y=Axis.2, color=as.factor(UserName))) +
    scale_color_manual(values = c("VVKAJ101"="red", "VVKAJ106"="blue")) 
  # It is OK to see a message: "Scale for 'colour' is already present. 
  # Adding another scale for 'colour', which will replace the existing scale."
  panelB
  
# Save the panel as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_p1_VVKAJ101_106_grey.pdf", 
         panelB, device="pdf", height=6, width=6, unit="in", dpi=300)
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [C] Highlight one sample; other points will retain their original colors. 
  select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 

# Changing the shape sizes might help find the dots. Note that points may be overlapping
  panelC <- 
    p1 + geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) 
  panelC
  
# Save the panel as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_p1_VVKAJ101_color.pdf", 
         panelC, device="pdf", height=6, width=6, unit="in", dpi=300)
  
    
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [D] Highlight multiple samples; other points will retain their original colors. 
  select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 
  select_point_2 <- subset(meta_usersdf, UserName=="VVKAJ106") 

  panelD <- 
    p1 + geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) +
         geom_point(data=select_point_2, aes(x=Axis.1, y=Axis.2), color="green", size=4) 
  panelD
  
# Save the panel as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_p1_VVKAJ101_106_color.png", 
         panelD, device="png", height=6, width=6, unit="in", dpi=300)
  
  

# ===============================================================================================================
# Use other ordination methods 
# ===============================================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Perform Principal Coordinate Analysis (PCoA) with UNweighted unifrac distance of your food data.
# This may take a few minutes depending on your data size.
# e.g. takes ~ 1 min to process a 7.9-MB phyloseq object . 
  ordinated_2 = phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=FALSE)  
  
# Use the same code above for creating plots, but now with ordinated_2 for the ord.object argument.
  

  
  
  
  
  
 

  
  
  