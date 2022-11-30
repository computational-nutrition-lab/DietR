# ===============================================================================================================
# Look at the proportion of Normal, Prediabetic, and Diabetic in each age group. 
# Version 1
# Created on 11/21/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR/")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())  

# Load necessary functions.
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
  source("lib/percent_kcal.R") # to use AddGenderAgeGroups function.  

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")

# ---------------------------------------------------------------------------------------------------------------
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/Laboratory_data")  

  
# ===============================================================================================================
# Load the mean totals and add "Gender", "AgeGroup", and "Age_Gender".
# ===============================================================================================================
  
# Load the data of those to be used in the diabetes status analysis. 
  glu_2 <- read.delim( file="QCtotal_d_ga_body_meta_glu_comp_2.txt", sep= "\t", header=T)
  
# Make GLU_index as a factor for plotting.
  glu_2$GLU_index <- factor(glu_2$GLU_index, levels = c("Normal", "Prediabetic", "Diabetic"))

# Add gender and age_groups to glu_2. The output is named "totals_out".
  AddGenderAgeGroups(input= glu_2, age.col="RIDAGEYR", gender.col="RIAGENDR")
  
# Rename the output.
  glu_2 <- totals_out
    
# Ensure that glu_2 now has Gender, AgeGroup, and Gender_Age columns.
  head(glu_2[, c("Gender", "AgeGroup", "Gender_Age")])


# ===============================================================================================================
# Build a stacked bar chart of diabetics by age and gender - FEMALE
# ===============================================================================================================

# Select females 
  glu_2_females <- subset(glu_2, Gender == "F") 
  
# Check the dimension of the selected data - 851 rows.
  nrow(glu_2_females)
  
# Create a table with the count of each AgeGroup and GLU_index combination.
# The variable to be counted can be any one with no missing data; therefore, "SEQN" is selected here.
  longtable_f <- aggregate(SEQN ~ AgeGroup + GLU_index,                                    
                           data = glu_2_females,   # Change the input dataset here.
                           FUN = length)
  longtable_f 

# Make a short table with GLU_index levels in each column in order to compute their proportions. 
  shorttable_f <- reshape2::dcast(longtable_f, AgeGroup ~ GLU_index, sum)
  shorttable_f
  
# Calculate the sum of all the levels of GLU_index.
  shorttable_f$sum <- rowSums(shorttable_f[, 2:4])
  
# Calculate the proportion of each - to be used as percentages.
  shorttable_f$Normal_pr      <- shorttable_f$Normal/shorttable_f$sum         
  shorttable_f$Prediabetic_pr <- shorttable_f$Prediabetic/shorttable_f$sum
  shorttable_f$Diabetic_pr    <- shorttable_f$Diabetic/shorttable_f$sum
  
# Calculate the sum of the proportions - should be all 1.
  shorttable_f$sum_pr <- rowSums(shorttable_f[, 6:8])
  shorttable_f
  
# Take the proportions and make it into a long table again for plotting.
  longtable_pr_f <- reshape2::melt(shorttable_f[, c(1, 6:8)])
  longtable_pr_f
  
# Generate a stacked barchart for females: GLU_index_pr_F. 
  GLU_index_pr_F <- ggplot(longtable_pr_f, aes(x= AgeGroup, y= value, fill= variable)) +
    geom_bar(position = "fill", stat = "identity",color='black',width=0.9) + 
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("steelblue2", "lightgoldenrod1", "lightpink1") ) +
    geom_text(aes(label = paste0( round(value*100, 0),"%")), 
              position = position_stack(vjust = 0.5), size = 5) +
    rotate_X_labels + space_axes + no_grid +
    labs(x="Age", y="")
  GLU_index_pr_F

# Save the plot.
  ggsave("QCtotal_d_ga_body_meta_glu_comp_2_AgeGroup_GLU_index_female.pdf", 
         GLU_index_pr_F, device="pdf", width=7, height=4.5, unit="in", dpi=300)

# ===============================================================================================================
# Build a stacked bar chart of diabetics by age and gender - MALE
# ===============================================================================================================
# Repeat the same operation and tables for females will be created. 
  
# Select males 
  glu_2_males <- subset(glu_2, Gender == "M") 
  
# Check the number of rows of the selected data - 774 rows.
  nrow(glu_2_males)
  
# Create a table with the count of each AgeGroup and GLU_index combination.
# The variable to be counted can be any one with no missing data; therefore, "SEQN" is selected here.
  longtable_m <- aggregate(SEQN ~ AgeGroup + GLU_index,                                    
                         data = glu_2_males,
                         FUN = length)
  longtable_m 
  
# Make a short table with GLU_index levels in each column in order to compute their proportions. 
  shorttable_m <- reshape2::dcast(longtable_m, AgeGroup ~ GLU_index, sum)
  shorttable_m
  
# Calculate the sum of all the levels of GLU_index.
  shorttable_m$sum <- rowSums(shorttable_m[, 2:4])
  
# Calculate the proportion of each - to be used as percentages.
  shorttable_m$Normal_pr      <- shorttable_m$Normal/shorttable_m$sum         
  shorttable_m$Prediabetic_pr <- shorttable_m$Prediabetic/shorttable_m$sum
  shorttable_m$Diabetic_pr    <- shorttable_m$Diabetic/shorttable_m$sum
  
# Calculate the sum of the proportions - should be all 1.
  shorttable_m$sum_pr <- rowSums(shorttable_m[, 6:8])
  shorttable_m
  
# Take the proportions and make it into a long table again for plotting.
  longtable_pr_m <- reshape2::melt(shorttable_m[, c(1, 6:8)])
  longtable_pr_m
  
# Plot it with percentage labels.
  GLU_index_pr_M <- ggplot(longtable_pr_m, aes(x= AgeGroup, y= value, fill= variable)) +
    geom_bar(position = "fill", stat = "identity",color='black',width=0.9) + 
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("steelblue2", "lightgoldenrod1", "lightpink1") ) +
    geom_text(aes(label = paste0( round(value*100, 0),"%")), 
              position = position_stack(vjust = 0.5), size = 5) +
    rotate_X_labels + space_axes + no_grid +
    labs(x="Age Group", y="")
  GLU_index_pr_M
  
# Save the plot.
  ggsave("QCtotal_d_ga_body_meta_glu_comp_2_AgeGroup_GLU_index_male.pdf", 
         GLU_index_pr_M, device="pdf", width=7, height=4.5, unit="in", dpi=300)
  
  
# By looking at the distribution, males in 60s and over has the highest percentages of Diabetic individuals.
# We will use this gender-age group to analyze their diets further. 

  