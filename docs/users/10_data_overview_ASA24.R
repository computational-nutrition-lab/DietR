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
# Load and analyze (QC-ed) ASA24 items data
# ===============================================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ")  

# Load your items data to be analyzed.
# "_f_id_s_m" stands for: "food names formatted", "SampleID added", "selected individuals", 
# and "metadata merged".  
  items_f_id_s_m <- read.table("VVKAJ_Items_f_id_s_m.txt", sep="\t", header=T)
  head(items_f_id_s_m)

# ---------------------------------------------------------------------------------------------------------------
# Summary statistics
  
# Summary statistics of one variable

# View min, quantiles, mean, etc. for a variable in your dataset. 
  summary(items_f_id_s_m$KCAL)

# Summary statistics of all the variables
# Calculate the minimum, 1st quantile, median, mean, 3rd quantile, max, and standard deviation
# for each variable in the input dataframe and save as a .txt file. 
  SummaryStats(inputdf = items_f_id_s_m, 
               outfn = "VVKAJ_Items_f_id_s_m_summ.txt")
# *** NOTE that these are individual items, not by user or day. 

# ---------------------------------------------------------------------------------------------------------------
# Boxplot
# Generate a boxplot to view data distribution.
  
# Boxplot of KCAL by users. And 
  users_kcal <- ggplot(items_f_id_s_m, aes(x=UserName, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes + rotate_X_labels
  users_kcal
  
# Save it as a .pdf file.
  ggsave("VVKAJ_Items_f_id_s_m_users_kcal.pdf", users_kcal, device="pdf")
  
# Similarly, generate a boxplot of KCAL by gender.
  gender_kcal <- ggplot(items_f_id_s_m, aes(x=Gender, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes 
  gender_kcal

# Save it as a .pdf file.
  ggsave("VVKAJ_Items_f_id_s_m_gender_kcal.pdf", gender_kcal, device="pdf")
  
  
# ---------------------------------------------------------------------------------------------------------------
# Scatterplot
  
# Scaterplot of two numeric variables: TFAT and KCAL. 
  TFAT_KCAL <- ggplot(items_f_id_s_m, aes(x=TFAT, y=KCAL)) +
    geom_point() + no_grid + space_axes 
  TFAT_KCAL

# Save it as a .pdf file.
  ggsave("VVKAJ_Items_f_id_s_m_TFAT_KCAL.pdf", TFAT_KCAL, device="pdf")
  
  
# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient
  cor.test(x=items_f_id_s_m$TFAT, y=items_f_id_s_m$KCAL, method="pearson")

  
# ===============================================================================================================
# Load and analyze (QC-ed) ASA24 totals data
# ===============================================================================================================
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")  
  
# Load your QC-ed totals data to be analyzed.
  tot_m_QCed <- read.table("VVKAJ_Tot_m_QCed.txt", sep="\t", header=T)
  
# Note that each row is a total dietary intake of each user on each day. 
  head(tot_m_QCed)

# ---------------------------------------------------------------------------------------------------------------
# Summary statistics
  
# Summary statistics of one variable
  summary(tot_m_QCed$KCAL)
  
# Calculate the min, quantiles, mean, etc. for a variable in your dataset
# in the same way we did with the items.   
  SummaryStats(inputdf = tot_m_QCed, 
               outfn = "VVKAJ_Tot_m_QCed_summ.txt")

# ---------------------------------------------------------------------------------------------------------------
# Boxplot
# Generate a boxplot to view data distribution.

# Boxplot of KCAL by users. This is a variation of the days, and note that
# some users may have less number of days due to the QC process or missing data. 
  users_kcal_t <- ggplot(tot_m_QCed, aes(x=UserName, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes + rotate_X_labels
  users_kcal_t
  
# Save it as a .pdf file.
  ggsave("VVKAJ_Tot_m_QCed_users_KCAL.pdf", users_kcal_t, device="pdf")
  
# Boxplot of KCAL by gender.
  gender_KCAL_t <- ggplot(tot_m_QCed, aes(x=Gender, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes 
  gender_KCAL_t
  
# Save it as a .pdf file.
  ggsave("VVKAJ_Tot_m_QCed_gender_KCAL.pdf", gender_KCAL_t, device="pdf")
  

# Boxplot of KCAL by gender, with each datapoint.  Note that geom_boxplot must have outlier.shape = NA 
# when plotted with geom_jitter. Otherwise, outlier points will be duplicated and will be misleading. 
  gender_KCAL_t_dots <- ggplot(tot_m_QCed, aes(x=Gender, y=KCAL)) +
    geom_boxplot(outlier.shape = NA) + no_grid + space_axes +
    geom_jitter(width=0.3)
  gender_KCAL_t_dots
  
# Save it as a .pdf file.
  ggsave("VVKAJ_Tot_m_QCed_gender_KCAL_dots.pdf", gender_KCAL_t_dots, device="pdf")
  
  
# ---------------------------------------------------------------------------------------------------------------
# Scatterplot
  
# Scaterplot of two variables. 
  TFAT_KCAL_t <- ggplot(tot_m_QCed, aes(x=TFAT, y=KCAL)) +
    geom_point() + no_grid + space_axes
  TFAT_KCAL_t
  
# Save it as a .pdf file.
  ggsave("VVKAJ_Tot_m_QCed_TFAT_KCAL.pdf", TFAT_KCAL_t, device="pdf")

# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient
  cor.test(x=tot_m_QCed$TFAT, y=tot_m_QCed$KCAL, method="pearson")

# ---------------------------------------------------------------------------------------------------------------
# Lineplot 
  
# Prepare your totals dataset for line plot - insert NA to missing combinations of UserName and RecallNo (day), 
# and separate rows into NA's and no NAs. 
  PrepLinePlot(inputdf= tot_m_QCed, day="RecallNo", username="UserName", 
               all.fn=              "VVKAJ_Tot_m_QCed_wNA.txt",
               full.days.only.fn=   "VVKAJ_Tot_m_QCed_fullonly.txt",
               partial.days.only.fn="VVKAJ_Tot_m_QCed_partialonly.txt")
  
# Load the files.
  tot_m_QCed_w_NA <-        read.table("VVKAJ_Tot_m_QCed_wNA.txt", sep="\t", header=T)
  tot_m_QCed_fullonly <-    read.table("VVKAJ_Tot_m_QCed_fullonly.txt", sep="\t", header=T)
  tot_m_QCed_partialonly <- read.table("VVKAJ_Tot_m_QCed_partialonly.txt", sep="\t", header=T)

# Make RecallNo (day) as a factor so that it can be used as a variable on the X (or Y) axes.
  tot_m_QCed$RecallNo <-             as.factor(tot_m_QCed$RecallNo)
  tot_m_QCed_w_NA$RecallNo <-        as.factor(tot_m_QCed_w_NA$RecallNo)
  tot_m_QCed_fullonly$RecallNo <-    as.factor(tot_m_QCed_fullonly$RecallNo)
  tot_m_QCed_partialonly$RecallNo <- as.factor(tot_m_QCed_partialonly$RecallNo)

# Plot points and lines separately.  Specify your "x" and "y" twice.
# The geom_line function only connects the data of individuals with all days of data.
  lineplot_1 <- ggplot() + 
    geom_point(tot_m_QCed,          mapping = aes(x=RecallNo, y=KCAL, group=UserName, color=UserName)) +
    geom_line( tot_m_QCed_fullonly, mapping = aes(x=RecallNo, y=KCAL, group=UserName, color=UserName), 
              linetype="dashed") + no_grid +
    scale_color_manual(values = distinct100colors) 
  lineplot_1

# Save it as a .pdf file.
  ggsave("VVKAJ_Tot_m_QCed_KCAL_lineplot.pdf", lineplot_1, device="pdf")
  
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory before you start running another script.
  setwd(main_wd)
  
  
