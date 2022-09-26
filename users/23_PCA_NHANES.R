# ===============================================================================================================
# Perform PCA analysis with NHANES males in their 50s data.
# Version 1
# Created on 08/29/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Load necessary packages and source scripts
  library(ggplot2)
  library(ggfortify)
  
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")
  source("lib/PCA.R")
  
# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# PCA with nutrients and body weight
# ===============================================================================================================

# Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data")

# Your input data should be a data frame with variables with non-zero variance. 
  pca_input <- read.table("males50s_QCtotal_d_glu_body_meta_demo_Nut_rv.txt", 
                          sep="\t", header=T)

# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
  head(pca_input, 2)
  
# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x= pca_input, scale= TRUE)   

# Specify the directory (folder) to save the results.
  res_dir_Nut = "males50s_Nut_PCA" 

# Specify the prefix of filenames to be saved. 
  res_prefix_Nut = "males50s_Nut"

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
  OutputPCA(pca.data= pca_input, pca.result= scaled_pca, 
             out.dir= res_dir_Nut, out.prefix= res_prefix_Nut)
  
# Now, output .pdf and .txt files should be found in your folder specified as. res_prefix_Nut.

# Combine the input (totals before processing) with all the variables and the PC results. 
# Input is your items/totals input file before any prep for clustering, from which you derived the input for the PCA.
  SaveInputAndPCs(input="QCtotal_d_glu_body_meta_demo_males50s.txt", pca.results = scaled_pca, 
                  out.dir= res_dir_Nut, out.prefix= res_prefix_Nut)

# [NOTE] Even though the input file has both nutrients (Nut) and food categories (Cat) data,  
# PCA was done with only either Nut or Cat, not both.

# ---------------------------------------------------------------------------------------------------------------
# Create a biplot with Normal, Prediabetes, and Diabetes people color-coded.   

# Load the input & PC info.
  Nut_PCs <- read.table("males50s_Nut_PCA/males50s_Nut_PCs.txt", sep="\t", header=T)

# Change GLU_index to a factor so that factors will be displayed in order.
  Nut_PCs$GLU_index <- factor(Nut_PCs$GLU_index, levels= c("Normal", "Prediabetic", "Diabetic"))

# Ellipses.
  ell <- ggplot(data= Nut_PCs, aes(x=PC1, y=PC2, color= GLU_index)) +
    geom_point(aes(color=GLU_index), size=3 ) + 
    theme_bw(base_size = 12) + no_grid + theme(aspect.ratio = 1) +
    scale_color_manual( values= c("steelblue3", "gold3", "hotpink")) +
    stat_ellipse(level=0.95)
  ell
  
# Save as a .pdf.
  ggsave("males50s_Nut_PCA/males50s_Nut_PCA_by_GLU_index_PC12_ell.pdf", ell, 
         device="pdf", width=7, height=6.5)

# ---------------------------------------------------------------------------------------------------------------
# Load the glu_3_males50s data. (The original data before filtering variables)
  glu_3_males50s <- read.table("QCtotal_d_glu_body_meta_demo_males50s.txt", 
                               sep="\t", header=T)
  colnames(glu_3_males50s)
  
# Change GLU_index to a factor so that factors will be displayed in order.
  glu_3_males50s$GLU_index <- factor(glu_3_males50s$GLU_index, 
                                     levels= c("Normal", "Prediabetic", "Diabetic"))

# Use the autoplot function. Specify which PC in the x and y arguments.
  food_Nut_PCA <- autoplot(scaled_pca, x=1, y=2,    
                           loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
                           data = glu_3_males50s,  size= 3 ) +   # data is the original input, not after selecting specific variables.  
    geom_point(size = 3, alpha = 1, na.rm = T, shape = 21,  aes(fill= GLU_index)) +
    theme_bw(base_size = 12) + theme(aspect.ratio = 1) +  
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual( values= c("steelblue3", "yellow", "hotpink")) 
  food_Nut_PCA

  ggsave("males50s_Nut_PCA/males50s_Nut_PCA_by_GLU_index_PC12.pdf", food_Nut_PCA, 
         device="pdf", width=7, height=6.5)

 
# ===============================================================================================================
# PCA with food categories and body weight
# ===============================================================================================================

# Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data")
  
# Your input data should be a data frame with variables with non-zero variance. 
  pca_input <- read.table("males50s_QCtotal_d_glu_body_meta_demo_Cat_rv.txt", 
                          sep="\t", header=T)

# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)

# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x= pca_input, scale= TRUE)   

# Specify the directory (folder) to save the results.
  res_dir_Cat = "males50s_Cat_PCA" 

# Specify the prefix of filenames to be saved. 
  res_prefix_Cat = "males50s_Cat"

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
  OutputPCA(pca.data=pca_input, pca.result=scaled_pca, 
             out.dir= res_dir_Cat, out.prefix= res_prefix_Cat)

# Combine the input (totals before processing) with all the variables and the PC results. 
# Input is your items/totals input file before any prep for clustering, from which you derived the input for the PCA.
  SaveInputAndPCs(input="QCtotal_d_glu_body_meta_demo_males50s.txt", pca.results = scaled_pca, 
                  out.dir= res_dir_Cat, out.prefix= res_prefix_Cat)
# [NOTE] Even though the input file has both nutrients (Nut) and food categories (Cat) data,  
# PCA was done with only either Nut or Cat, not both.

# ---------------------------------------------------------------------------------------------------------------
# Create a biplot with Normal, Prediabetes, and Diabetes people color-coded.   

# Load the input & PC info.
  Cat_PCs <- read.table("males50s_Cat_PCA/males50s_Cat_PCs.txt", sep="\t", header=T)

# Change GLU_index to a factor so that the levels will be displayed in order.
  Cat_PCs$GLU_index <- factor(Cat_PCs$GLU_index, levels= c("Normal", "Prediabetic", "Diabetic"))

# Ellipses. Specify which PCs to plot. Specify which PC in the x and y arguments.
  ell <- ggplot(data= Cat_PCs, aes(x=PC1, y=PC2, color= GLU_index)) +
    geom_point(aes(color=GLU_index), size=3 ) + 
    theme_bw(base_size = 12) + no_grid + space_axes + theme(aspect.ratio = 1) +
    scale_color_manual( values= c("steelblue3", "gold3", "hotpink")) +
    stat_ellipse(level=0.95)
  ell
  
# Save as a .pdf.
  ggsave("males50s_Cat_PCA/males50s_Cat_PCA_by_GLU_index_PC12_ell.pdf", ell, 
        device="pdf", width=7, height=6.5)

# ---------------------------------------------------------------------------------------------------------------
# Load the glu_3_males50s data. 
  glu_3_males50s <- read.table("QCtotal_d_glu_body_meta_demo_males50s.txt", 
                               sep="\t", header=T)
  
# Change GLU_index to a factor so that factors will be displayed in order.
  glu_3_males50s$GLU_index <- factor(glu_3_males50s$GLU_index, 
                                     levels= c("Normal", "Prediabetic", "Diabetic"))  
  
# Use the autoplot function. Specify which PC in the x and y arguments.
  food_Cat_PCA <- autoplot(scaled_pca, x=1, y=2,  
                           loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
                           data = glu_3_males50s,  size= 3 ) +            # The original data before filtering.
    geom_point(size = 3, alpha = 1, na.rm = T, shape = 21,  aes(fill= GLU_index)) +
    theme_bw(base_size = 12) + theme(aspect.ratio = 1) +  
    theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual( values= c("steelblue3", "yellow", "hotpink")) 
  food_Cat_PCA

  ggsave("males50s_Cat_PCA/males50s_Cat_PCA_by_GLU_index_PC12.pdf", food_Cat_PCA, 
         device="pdf", width=7, height=6.5)

# ---------------------------------------------------------------------------------------------------------------
  # Come back to the main directory.
  setwd(main_wd)  
  
  