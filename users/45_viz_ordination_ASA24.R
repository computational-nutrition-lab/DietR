# ===============================================================================================================
# Visualize ordination results.
# Version 3 - cleaner version with just 'Users' plot.
# Created on 10/03/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")
  
# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# load the necessary packages.
  library(ggplot2)
  
# Load necessary functions and ggplot formatting themes
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")
  
# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Load ordination results
# ===============================================================================================================
  
# Change to the folder called "Unifrac" in your "VVKAJ" folder.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Unifrac/")
  
# Read in the metadata and users' Axis values. 
# meta_usersdf_loaded <- read.table("results/ordinated_weighted_axes_meta_MCT.txt", header=T)
  meta_usersdf <- read.table("4Lv_ordinated_Weighted_meta_users.txt", header=T)

# Take a look at meta_usersdf_loaded. 
  head(meta_usersdf,3)

# Read in the eigenvalues for axis labels of biplots.
  eigen_loaded <- read.table("4Lv_ordinated_Weighted_eigen_percent.txt", header=T)
  
# ---------------------------------------------------------------------------------------------------------------
# Plot Axis 1 and Axis 2 to show the separation of samples colored by UserName, gender, timing, etc. as in the metadata.

# By UserName.
  by_user <- ggplot(meta_usersdf, aes(x=Axis.1, y=Axis.2, color=UserName)) +
          geom_point(aes(color=UserName), size=2) + 
          scale_color_manual(values = distinct100colors) + # OR use viridis theme.
          # scale_color_viridis_d() +
          xlab( paste("Axis.1 (", paste(round(eigen_loaded[1,2]*100, 1)), "%)", sep="") ) +
          ylab( paste("Axis.2 (", paste(round(eigen_loaded[2,2]*100, 1)), "%)", sep="") ) +
          no_grid + space_axes + theme(aspect.ratio = 1)
  by_user
  
# Save by_user plot as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_users.pdf", by_user,
         device="pdf", height=6, width=6, unit="in", dpi=300)
  
# Add lines to connect samples in the order in which they appear in the data using geom_path. 
# [NOTE] geom_line connects in the order of the variable (small to large) on the x axis, 
# so it could be misleading.
  by_user_pathconnected <- by_user + geom_path(aes(color = UserName))  
  by_user_pathconnected
  
  # Save by_user_pathconnected as a PDF.
  ggsave("4Lv_ordinated_Weighted_Axis12_users_pathconnected.pdf", 
         by_user_pathconnected, device="pdf", height=6, width=6, unit="in", dpi=300)
  
# ---------------------------------------------------------------------------------------------------------------
# Color individuals by diet (Vegan, Vegetarian, Keto, American, and Japanese).
  by_diet <- ggplot(meta_usersdf, aes(x=Axis.1, y=Axis.2, color=Diet)) +
    geom_point(aes(color=Diet), size=2) + 
    scale_color_manual(values = distinct100colors) + # OR use viridis theme.
    # scale_color_viridis_d() +
    xlab( paste("Axis.1 (", paste(round(eigen_loaded[1,2]*100, 1)), "%)", sep="") ) +
    ylab( paste("Axis.2 (", paste(round(eigen_loaded[2,2]*100, 1)), "%)", sep="") ) +
    no_grid + space_axes + theme(aspect.ratio = 1)
  by_diet
  
# You can add ellipses at a desired confidence level. 
  by_diet_ellipses <- by_diet + stat_ellipse(level=0.95) 
  by_diet_ellipses
  
# Save by_user_diet_ellipses as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_diet_ellipses.pdf", by_diet_ellipses, 
         device="pdf", height=6, width=6, unit="in", dpi=300)

# Make polygons by diet.
  by_diet_polygons <- by_diet + geom_polygon(aes(fill = Diet)) + 
    geom_point(aes(color = Diet), size=2) + 
    scale_fill_manual(values = distinct100colors)
  by_diet_polygons
  
  # Save by_diet_polygons as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_diet_polygons.pdf", 
         by_diet_polygons, device="pdf", height=6, width=6, unit="in", dpi=300)


# ===============================================================================================================
# Change colors for specific user(s) 
# ===============================================================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [A] Highlight one sample with others being grey.  
  # Specify which user to highlight; e.g. VVKAJ101.
  select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 

  panelA <- 
    by_user + geom_point(size=2, color="grey") +  
    geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=2) 
  panelA
  
# Save the panel as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_users_VVKAJ101_grey.pdf", 
         panelA, device="pdf", height=6, width=6, unit="in", dpi=300)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [B] Highlight multiple samples with others being grey.
# Specify colors for each samples to be highlighted in the scale_color_manual argument.
  select_points <- subset(meta_usersdf, UserName=="VVKAJ101" | UserName=="VVKAJ106" )

  panelB <- 
    by_user + geom_point(data=select_points, aes(x=Axis.1, y=Axis.2, color=as.factor(UserName))) +
    scale_color_manual(values = c("VVKAJ101"="red", "VVKAJ106"="blue")) 
  # It is OK to see a message: "Scale for 'colour' is already present. 
  # Adding another scale for 'colour', which will replace the existing scale."
  panelB
  
# Save the panel as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_users_VVKAJ101_106_grey.pdf", 
         panelB, device="pdf", height=6, width=6, unit="in", dpi=300)
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [C] Highlight one sample; other points will retain their original colors. 
  select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 

# Changing the shape sizes might help find the dots. Note that points may be overlapping
  panelC <- 
    by_user + geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) 
  panelC
  
# Save the panel as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_users_VVKAJ101_color.pdf", 
         panelC, device="pdf", height=6, width=6, unit="in", dpi=300)
  
    
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [D] Highlight multiple samples; other points will retain their original colors. 
  select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 
  select_point_2 <- subset(meta_usersdf, UserName=="VVKAJ106") 

  panelD <- 
    by_user + 
    geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) +
    geom_point(data=select_point_2, aes(x=Axis.1, y=Axis.2), color="green", size=4) 
  panelD
  
# Save the panel as a PDF. 
  ggsave("4Lv_ordinated_Weighted_Axis12_users_VVKAJ101_106_color.png", 
         panelD, device="png", height=6, width=6, unit="in", dpi=300)
  

  