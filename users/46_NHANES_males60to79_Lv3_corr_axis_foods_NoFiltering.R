# ===============================================================================================================
# Generate a heatmap of correlation between food categories and ordination Axes.  
# Version 2
# Created on 02/16/2023 by Rie Sadohara
# The create_corr_frame function credit: Mo Hutti. 
# ===============================================================================================================

# Set your working directory to the main directory.
Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR/")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# load the necessary packages and the source code.
  library(ggplot2)
  source("lib/specify_data_dir.R")
  source("lib/corr.axes.foods.R")
  source("lib/ggplot2themes.R")

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# You can come back to the main directory by:
  setwd(main_wd)

# Set working directory.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data/Ordination/")

# ===============================================================================================================
# Analyze correlation between ordination axes values and foods 
# ===============================================================================================================
  
# From sorted food OTU table, generate a table of total amount of food consumed by all the individuals, 
# and a table with correlation coefficients, p-values, and q-values with desired threshold between 
# food items and Axes that were saved in the ordination section. 
# Be careful about not to confuse WEIGHTED and UNweighted unifrac distances.
  
# WEIGHTED unifrac distance results.
  CorrAxesFood(food.otu_soted = "../Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.food.otu_sortedbysample.txt", 
               AmountSums.out.fn =          "Food_D12_FC_QC_demo_QCed_males60to79_3Lv_AmountSums.txt",
               qval.threshold = 0.05,
               meta.users =            "Food_D12_FC_QC_demo_QCed_males60to79_3Lv_ord_WEIGHTED_meta_users.txt",
               corr.axes.foods.outfn = "Food_D12_FC_QC_demo_QCed_males60to79_3Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")
  
  # food.otu_soted:     xxx.food.otu.sorted.txt file, saved in the ordination section.
  # AmountSums.out.fn:  output filename to be saved which has the total consumption amount of each food.
  # qval.threshold:     q-value threshold to call a correlation significant.
  # meta.users:         xxx.meta_users.txt file, waved in the ordination section.
  # corr.axes.foods.outfn: output filename to be saved which has the correlation between foods and Axes.
  
  # UNweighted unifrac distance ordination results.
  # xxx_AmountSums.txt will be generated again, but its content will be the same regardless of which distance method
  # (weighted or unweighted unifrac or else) was used, as long as the food.otu_sorted is the same.
  
  CorrAxesFood(food.otu_soted = "../Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.food.otu_sortedbysample.txt",
               AmountSums.out.fn = "Food_D12_FC_QC_demo_QCed_males60to79_3Lv_AmountSums.txt",
               qval.threshold = 0.05,
               meta.users =            "Food_D12_FC_QC_demo_QCed_males60to79_3Lv_ord_UNweighted_meta_users.txt",
               corr.axes.foods.outfn = "Food_D12_FC_QC_demo_QCed_males60to79_3Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt")
  
# ---------------------------------------------------------------------------------------------------------------
# Load the output.
  # WEIGHTED         
  dat <- read.delim("VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")
  
  # Show only food items that are significantly correlated with one of the axes.
  subset(dat, Significance=="*")

  # It is also possible to view each axis separately.
  # Select Axis 1 rows
  dat_1 <- subset(dat, Axis=="Axis.1")
  head(dat_1[order(dat_1$qval), ], 10)
  
  # Select Axis 2 rows and sort by qval.  
  dat_2 <- subset(dat, Axis=="Axis.2")
  head(dat_2[order(dat_2$qval), ], 10)
  
  # Select Axis 3 rows and sort by qval.  
  dat_3 <- subset(dat, Axis=="Axis.3")
  head(dat_3[order(dat_3$qval), ], 10)
  
  # Select Axis 4 rows and sort by qval.  
  dat_4 <- subset(dat, Axis=="Axis.4")
  head(dat_4[order(dat_4$qval), ], 10)
  
  
  # UNweighted can be viewed in the same way.         
  dat <- read.delim("Food_D12_FC_QC_demo_QCed_males60to79_3Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt")
  
  # Show only food items that are significantly correlated with one of the axes.
  subset(dat, Significance=="*")
  

  