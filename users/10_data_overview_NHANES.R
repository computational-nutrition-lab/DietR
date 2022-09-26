# ===============================================================================================================
# Take an overview of ASA24 items and totals data.
# Version 1
# Created on 06/22/2022 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Set working directory 
# ===============================================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/dietary_patterns/")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow and generate plots.
  source("lib/specify_data_dir.R")  
  source("lib/data_overview.R")  
  source("lib/ggplot2themes.R")  

# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Load and analyze (QC-ed) NHANES food items data
# ===============================================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  

# Load your items data to be analyzed.
# "_FC_cc_f_d" stands for: "Food category variables added", "column names changed", 
# "food formatted", and "demographic data merged".  
  food12f_d <- read.table("Food_D12_FC_cc_f_d.txt", sep="\t", header=T) 
  
# ---------------------------------------------------------------------------------------------------------------
# Summary statistics
  
# Summary statistics of one variable

# View min, quantiles, mean, etc. for a variable in your dataset. 
  summary(food12f_d$KCAL)

# Summary statistics of all the variables
# Calculate the minimum, 1st quantile, median, mean, 3rd quantile, max, and standard deviation
# for each variable in the input dataframe and save as a .txt file. 
  SummaryStats(inputdf = food12f_d, 
               outfn = "Food_D12_FC_cc_f_d_summ.txt")
# *** NOTE that these are individual items, not by user or day. 

# ---------------------------------------------------------------------------------------------------------------
# Boxplot
# Generate a boxplot to view data distribution.

# Group by metadata variables and generate a boxplot. 
# According to the documentation of the NHANES 2015-2016 
# (https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.htm),
# the column for gender is RIAGENDR.   
  
# Convert RIAGENDR into a factor, so that it will be a categorical variable.
  food12f_d$RIAGENDR <- factor(food12f_d$RIAGENDR, levels= c('1', '2'))
  
# Generate a boxplot of KCAL by gender.
  gender_kcal <- ggplot(food12f_d, aes(x=RIAGENDR, y=KCAL, group=RIAGENDR)) +
    geom_boxplot() + no_grid + space_axes + labs(x="Gender") +
    scale_x_discrete(labels = c("Male", "Female")) 
  gender_kcal

# Save it as a .pdf file.
  ggsave("Food_D12_FC_cc_f_d_gender_kcal.pdf", 
         gender_kcal, device="pdf", width=5.3, height=4.5)

# ---------------------------------------------------------------------------------------------------------------
# Scatterplot
  
# Scaterplot of two numeric variables: TFAT and KCAL. 
  TFAT_KCAL <- ggplot(food12f_d, aes(x=TFAT, y=KCAL)) +
    geom_point() + no_grid + space_axes 
  TFAT_KCAL

# Save it as a .pdf file.
  ggsave("Food_D12_FC_cc_f_d_TFAT_KCAL.pdf", 
         TFAT_KCAL, device="pdf", width=5.3, height=4.5)
  
  
# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient.
  cor.test(x=food12f_d$TFAT, y=food12f_d$KCAL, method="pearson")

  
# ===============================================================================================================
# Load and analyze (QC-ed) NHANES totals data
# ===============================================================================================================
  
# Specify the directory where the data is, if you have not done so yet.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  
  
# Load your QC-ed totals with demograhpic data to be analyzed.
  QCtotals_d <- read.table("Total_D12_FC_QC_mean_QC_d.txt", sep="\t", header=T)
  
# Note that each row is a total dietary intake of each user on each day. 
  head(QCtotals_d, 2)

# ---------------------------------------------------------------------------------------------------------------
# Summary statistics
  
# Summary statistics of one variable
  summary(QCtotals_d$KCAL)
  
# Calculate the min, quantiles, mean, etc. for a variable in your dataset
# in the same way we did with the items.   
  SummaryStats(inputdf = QCtotals_d, 
               outfn = "Total_D12_FC_QC_mean_QC_d_summ.txt")

# ---------------------------------------------------------------------------------------------------------------
# Boxplot
# Generate a boxplot to view data distribution.

# Convert RIAGENDR into a factor, so that it will be a categorical variable.
  QCtotals_d$RIAGENDR <- factor(QCtotals_d$RIAGENDR, levels= c('1', '2'))
  
# Boxplot of KCAL by gender (RIAGENDR). "t" stands for "total"
  gender_KCAL_t <- ggplot(QCtotals_d, aes(x= RIAGENDR, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes + labs(x="Gender") +
    scale_x_discrete(labels = c("Male", "Female"))
  gender_KCAL_t
  
# Save it as a .pdf file.
  ggsave("Total_D12_FC_QC_mean_QC_d_gender_KCAL.pdf", 
         gender_KCAL_t, device="pdf", width=5.3, height=4.5)
  

# Boxplot of KCAL by gender, with each datapoint.  Note that geom_boxplot must have outlier.shape = NA 
# when plotted with geom_jitter. Otherwise, outlier points will be duplicated and will be misleading. 
  gender_KCAL_t_dots <- ggplot(QCtotals_d, aes(x= RIAGENDR, y= KCAL)) +
    geom_boxplot(outlier.shape = NA) + no_grid + space_axes + labs(x="Gender") +
    geom_jitter(width=0.3, color="grey60", alpha=0.5) +
    scale_x_discrete(labels = c("Male", "Female"))
  gender_KCAL_t_dots
  
# Save it as a .pdf file.
  ggsave("Total_D12_FC_QC_mean_QC_d_KCAL_dots.pdf", 
         gender_KCAL_t_dots, device="pdf", width=5.3, height=4.5)
  
  
# ---------------------------------------------------------------------------------------------------------------
# Scatterplot
  
# Scaterplot of two variables. 
  TFAT_KCAL_t <- ggplot(QCtotals_d, aes(x=TFAT, y=KCAL)) +
    geom_point(color="grey30", alpha=0.5) + no_grid + space_axes
  TFAT_KCAL_t
  
# Save it as a .pdf file.
  ggsave("Total_D12_FC_QC_mean_QC_d_TFAT_KCAL.pdf", 
         TFAT_KCAL_t, device="pdf", width=5.3, height=4.5)

# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient
  cor.test(x=QCtotals_d$TFAT, y=QCtotals_d$KCAL, method="pearson")

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)
  
  
