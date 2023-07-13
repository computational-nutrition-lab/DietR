# ===============================================================================================================
# Take random 55 samples from Prediabetic
# |
# Generate food tree, OUT table, tax table.
# |
# Generate phyloseq object
# |
# Ordination and save p-values of 
# adonis::bdisp, 
# adonis::vegan,
# pairwise.adonis. 
# 
# Version 1
# Created on 11/17/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
Session --> Set working directory --> Choose directory.
setwd("~/GitHub/dietarry_patterns")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# Functions needed for subsetting ------------
library(SASxport)
source("lib/specify_data_dir.R")
source("lib/load_clean_NHANES.R")
source("lib/prep_data_for_clustering.R")
source("lib/ggplot2themes.R")

# Load the distinct 100 colors for use.
distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")

# Functions needed for foodtree making ------------
# Load source scripts
source("lib/specify_data_dir.R")

source("lib/Food_tree_scripts/newick.tree.r")
source("lib/Food_tree_scripts/check.db.r")
source("lib/Food_tree_scripts/format.foods.r")
source("lib/Food_tree_scripts/filter.db.by.diet.records.r")
source("lib/Food_tree_scripts/make.food.tree.r")
source("lib/Food_tree_scripts/make.food.otu.r")
source("lib/Food_tree_scripts/make.fiber.otu.r")
source("lib/Food_tree_scripts/make.dhydrt.otu.r")

# Functions needed for ordination ------------
library(vegan)
library(phyloseq)
library(ggplot2)
library(ggtree)
library(SASxport)
library(pairwiseAdonis)
source("lib/specify_data_dir.R")
source("lib/ordination.R")
source("lib/ggplot2themes.R")
# source("lib/prep_for_adonis_pairwise.R")



# ===============================================================================================================
# ===============================================================================================================
# Load NHANES15-16totals with demographic data
# ===============================================================================================================
# Load necessary packages.
# library(SASxport)
# 
# # Set your working directory to the main directory.
# Session --> Set working directory --> Choose directory.
# setwd("~/GitHub/dietarry_patterns")
# 
# # Name your main directory for future use.
# main_wd <- file.path(getwd())
# 
# # Load necessary functions.
# source("lib/specify_data_dir.R")
# source("lib/load_clean_NHANES.R")
# source("lib/prep_data_for_clustering.R")
# source("lib/ggplot2themes.R")
# 
# # Load the distinct 100 colors for use.
# distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Load the data of those to be used in the diabetes status analysis. 
# glu <- read.delim( file="Laboratory_data/QCtotal_d_glu_body_meta.txt", sep= "\t", header= T )
# 
# table(glu$GLU_index)
# 
# # Make GLU_index as a factor for plotting.
# glu$GLU_index <- factor(glu$GLU_index, levels = c("Normal", "Prediabetic", "Diabetic"))
# 
# # Exclude those who are following special diets.   
# # Look at the number of individuals who are following any specific diet (DRQSDIET==1).
# table(glu$DRQSDIET)
# 
# # DRQSDIET==1 is following a special diet, so select only rows with DRQSDIET==2. 
# glu_2 <- subset(glu, DRQSDIET == 2)
# 
# # How many people remained? -- 1625 remained.
# table(glu_2$DRQSDIET)
# 
# # Check the sample size of each category.
# table(glu_2$GLU_index)
# 
# # Normal Prediabetic    Diabetic 
# # 684         730         211  
# 
# # ===============================================================================================================
# # Select only MEN 60-79 yo, so that the samples are more uniform and smaller.
# # ===============================================================================================================
# 
# # Age - no missing data, and spread pretty evenly. 
# summary(glu_2$RIDAGEYR)
# hist(glu_2$RIDAGEYR)
# 
# # Gender - no missing data. 1: male, 2: female.
# table(glu_2$RIAGENDR)     
# 
# # Select males in their 50s
# glu_2_males <- subset(glu_2, RIAGENDR == 1) 
# glu_2_males60to79 <- subset(glu_2_males, RIDAGEYR >= 60 & RIDAGEYR <= 79) 
# 
# # Check the dimension of the selected data - 151 rows.
# dim(glu_2_males60to79)
# 
# # Ensure the ages of the selected subpopulation are between 50-59.  
# table(glu_2_males60to79$RIDAGEYR)
# 
# # Look at the distribution of GLU_index among the selected subpopulation.
# table(glu_2_males60to79$GLU_index)
# 
# # Normal Prediabetic    Diabetic 
# # 47         127          63 
# 
# source("~/GitHub/R_Toolbox/twoway_table.R")
# TwoWayTableWithTotals(data=glu_2_males60to79, var1 = "RIDAGEYR", var2 = "GLU_index", 
#                       sort.by.var2.total = F, 
#                       outfn = "Laboratory_data/QCtotal_d_glu_body_meta_demo_males60to79_age_GLU.csv")
# twoway_table
# twoway = table(glu_2_males60to79$`RIDAGEYR`, glu_2_males60to79$GLU_index)



# ===============================================================================================================
# Load data already filtered for males aged 60-79. 
# ===============================================================================================================

glu_2_males60to79 = read.table("Laboratory_data/QCtotal_d_glu_body_meta_demo_males60to79.txt",  
                 sep="\t", header=T)

table(glu_2_males60to79$GLU_index)

# Normal Prediabetic    Diabetic 
# 47         127          63  

# ===============================================================================================================
# Take a random sample of Prediabetic. (just one time of sampling)
# ===============================================================================================================
# # Subset Prediabetics/  
# Prediabetics <- subset(glu_2_males60to79, GLU_index=="Prediabetic")
# 
# # scramble.a <- dtA.feats[sample(nrow(dtA.feats)),] 
# scramble.Predia <- Prediabetics[sample(nrow(Prediabetics)), ] 
# # this is shuffling all the rows in Prediabetics.
# 
# Prediabetics[1:5, 1:5]
# scramble.Predia[1:5, 1:5]
# 
# # Take the first 55 Prediabetic rows.
# Predia_55 <- scramble.Predia[1:55, ]
# 
# # Take normal and diabetic to merge with Predia_55. 
# Normal <- subset(glu_2_males60to79, GLU_index=="Normal")
# Dia <-    subset(glu_2_males60to79, GLU_index=="Diabetic")
# 
# # Merge 
# glu_2_males60to79Pred55 <- rbind(Normal, Predia_55, Dia) 
# 
# # Check. 
# table(glu_2_males60to79Pred55$GLU_index)
# 
# #  glu_2_males60to79Pred55 has 165 rows (samples) and 263 columns. 
# dim(glu_2_males60to79Pred55)
# 
# # Save glu_2_males60to79Pred55 as a txt file.
# write.table(glu_2_males60to79Pred55, "Laboratory_data/QCtotal_d_glu_body_meta_demo_males60to79Pred55.txt", 
#             sep="\t", row.names = F, quote = F)
# 
# read_n165 <- read.table("Laboratory_data/QCtotal_d_glu_body_meta_demo_males60to79Pred55.txt", 
#                         sep="\t", header = F)

# ----------------------------------------------------------------------------------------------------------------  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")

# ===============================================================================================================
# Bootstrapping to take random samples of Prediabetic in a loop.
# ===============================================================================================================
# Make tables to store results for WEIGHTED.
  counttable_w           <- data.frame(number=seq(1:100), Dia_vs_Nor=NA, Dia_vs_Pre=NA, Pre_vs_Nor=NA) 
  pvalue_table_w         <- data.frame(number= seq(1:100), dist.p=NA, adonis.p=NA)
  pairwise_adonis_list_w <- list()

# # Make tables to store results for UNweighted.
#   counttable_u           <- data.frame(number=seq(1:2), Dia_vs_Nor=NA, Dia_vs_Pre=NA, Pre_vs_Nor=NA) 
#   pvalue_table_u         <- data.frame(number= seq(1:2), dist.p=NA, adonis.p=NA)
#   pairwise_adonis_list_u <- list()

for(i in 1:100){
  
  # Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  
  
  ### Subset ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Subset Prediabetics/  
  Prediabetics <- subset(glu_2_males60to79, GLU_index=="Prediabetic")
  
  # scramble.a <- dtA.feats[sample(nrow(dtA.feats)),] 
  scramble.Predia <- Prediabetics[sample(nrow(Prediabetics)), ] 
  # this is shuffling all the rows in Prediabetics.
  
  # Take the first 55 rows in shuffled Prediabetic data.
  Predia_55 <- scramble.Predia[1:55, ]
  
  # Take normal and diabetic to merge with Predia_55. 
  Normal <- subset(glu_2_males60to79, GLU_index=="Normal")
  Dia <-    subset(glu_2_males60to79, GLU_index=="Diabetic")
  
  # Merge 
  glu_2_males60to79Pred55 <- rbind(Normal, Predia_55, Dia) 
  
  # Save glu_2_males60to79Pred55 as a txt file.
  write.table(glu_2_males60to79Pred55, "Laboratory_data/QCtotal_d_glu_body_meta_demo_males60to79Pred55.txt", 
              sep="\t", row.names = F, quote = F)
  
  read_Pred55 <<- read.table("Laboratory_data/QCtotal_d_glu_body_meta_demo_males60to79Pred55.txt", 
                            sep="\t", header = T)
  
  ### Make food tree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data")

  # Make the individuals as a vector.
  selectedind <- read_Pred55$SEQN
  
  # Load the input file (all food record data) to be filtered.
  all.food.record <- read.table("../Food_D12_FC_cc_f.txt", sep="\t", header=T)
  
  # Select only the individuals listed in 'selectedind'. 
  sel.food.record <- all.food.record[all.food.record$SEQN %in% selectedind, ]
  
    # Because the data is shuffled, the order will not match, but the same names are there.
    # # Confirm the two contains the same set of individuals. 
    # identical(unique(sel.food.record$SEQN), selectedind)
  
  # Save. This will be the input in the following procedures.
  write.table(sel.food.record, "Food_D12_FC_cc_f_males60to79Pred55.txt", 
              sep="\t", row.names=F, quote=F)  
  
  
  # ===============================================================================================================
  # Limit to just the foods reported in your study (formatted dietrecords.txt as the input)
  # ===============================================================================================================
  # Keep only the foods reported in your study. This is already done, but need to run this
  # so that the data will be formatted in a compatible way to create food tree.
  FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt",
                        food_records_fn  = "Food_D12_FC_cc_f_males60to79Pred55.txt",   # output of FormatFoods.
                        output_fn =        "Food_D12_FC_cc_f_males60to79Pred55_red.txt")

  # Use CheckDB function to ensure no food reported Food_D12_FC_cc_f.txt in is missing in the database.

  # Check if there is any food item reported by people but are missing in the database.
  check.db(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt",
           food_records_fn =  "Food_D12_FC_cc_f_males60to79Pred55_red.txt",
           output_fn =        "Food_D12_FC_cc_f_males60to79Pred55_red_missing.txt")

  # Load the output and check if the output contains anything?
  mmm = read.table("Food_D12_FC_cc_f_males60to79Pred55_red_missing.txt", sep="\t", header=T)
  print(head(mmm))

  # Has something ===> put this missing.txt file in addl_foods_fn argument of MakeFoodTree.
  # Empty         ===> put NULL in addl_foods_fn argument of MakeFoodTree.

  # Create food tree with the reduced dataset (only reported foods) classified at
  # a desired level of classification (Lv. 1-6).
  # "NodeLabelsMCT.txt" has a list of food levels and names, which comes with this package.
  MakeFoodTree(nodes_fn="../../Food_tree_eg/NodeLabelsMCT.txt",
               addl_foods_fn = NULL,
               num.levels = 3,
               food_database_fn =   "Food_D12_FC_cc_f_males60to79Pred55_red.txt",
               output_tree_fn =     "Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.nwk",
               output_taxonomy_fn = "Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.taxonomy.txt"
  )


  # --------------------------------------------------------------------------------------------------------------
  # Generate OTU tables for downstream analyses; IT MAY TAKE SOME TIME.
  # It is OK to see the following warning message:
  # In write.table(fiber.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
  # appending column names to file.
  # 
  # Make the standard food otu table with data in gram weights of food.
  MakeFoodOtu(food_records_fn=  "Food_D12_FC_cc_f_males60to79Pred55.txt",  # need to supply data that have 'FoodAmt' before applying FilterDBByDietRecords.
              food_record_id =  "SEQN",                          # The ID of your participants
              food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.taxonomy.txt",  # Your taxonomy file produced by MakeFoodTree.
              output_fn =       "Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.food.otu.txt")  # Output otu file to be saved.

  # Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "Food_D12_FC_cc_f_males60to79Pred55.txt",
               food_record_id=   "SEQN",
               food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.taxonomy.txt",
               output_fn=        "Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.fiber.otu.txt")

  # Make a food otu table as dehydrated grams per kcal
  MakeDhydrtOtu(food_records_fn=  "Food_D12_FC_cc_f_males60to79Pred55.txt",
                food_record_id =  "SEQN",
                food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.taxonomy.txt",
                output_fn =       "Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.dhydrt.otu.txt")

  # test.read <-   read.table("Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.taxonomy.txt", sep="\t", header=T)

  ### Prep data for making a phyloseq object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Food - copied from "40_ordination_NHANES_males60to79_2_Lv3_Euclidean_sortfood.R".
  # Load food OTU table - this is our food OTU data
  food_raw <- read.delim("Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.dhydrt.otu.txt", row.names=1)
  head(food_raw, 1)  # food_raw has SEQN in a different order (X86435 X86515 X86817 X86831 ...).
  
  # Sort the columns (SEQN)...
  # The taxonomy is at the end of food - the nth column.
  taxonomycolumn <- length(colnames(food_raw))
  
  # order the column names of food, except the last column which has taxonomy.
  sortedcolnames_order <- order(colnames(food_raw)[ 1 : taxonomycolumn-1 ])
  
  # Sort the SEQNs but do not touch the taxonomy column at the end. 
  food <- food_raw[, c(sortedcolnames_order, taxonomycolumn) ] 
  
  # # Take a look at the food file.
  # # The column name of "food" is SEQN preceded with an 'X'.
  # food[1:8, 1:8]
  
  # Format the food file and create an otu_table called OTU.
  PrepFood(data = food)
  
  # Taxonomy (tax)
  # Load taxonomy file generated by the MakeFoodTree function.   
  tax <- read.delim("Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.taxonomy.txt")
  
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data=tax)
  
  # Sample
  # Load the demographics data.
  demog <- read.xport("../Raw_data/DEMO_I.XPT")  # demog has SEQN in small-large order.
  
  # Load a dataset that has the "GLU_index" information. 
  glu <- read.delim( file="QCtotal_d_glu_body_meta.txt", sep= "\t", header= T )
  
  # head(glu$SEQN) # This also has SEQN in small-large order.
  
  # Take out only the SEQN and GLU_index.
  SEQN_GLU <- glu[, c("SEQN", "GLU_index")]
  # head(SEQN_GLU)     # This also has SEQN in small-large order.
  
  # Add GLU_index to metadata. 
  demog_glu <- merge(x=SEQN_GLU, y=demog, all.x=TRUE, by="SEQN")
  # head(demog_glu[, c("SEQN", "GLU_index")])  # This also has SEQN in small-large order.
  
  # Now, it has GLU_index.
  # head(demog_glu, 2)
  
  # Put 'X' in front of the SEQN and define it as rownames.    
  rownames(demog_glu) <- paste("X", demog_glu$SEQN, sep="") 
  
  # Prep metadata for generating a phyloseq object.  
  PrepMeta_NHANES(data= demog_glu)
  # View(PrepMeta_NHANES)
  # head(SAMPLES)                    # This IS in the small-large order. 
  # head(phyloseq::sample_data(phyfoods)) # this IS also in the small-large order.
  
  # Food tree
  # Load foodtree file generated by the MakeFoodTree function.   
  foodtree <- read_tree("Foodtree/Food_D12_FC_cc_f_males60to79Pred55_red_Lv3.nwk")
  # It is OK to see a message that says:
  # "Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
  # Also defined by 'tidytree'"
  
  # Format the food tree and save it as 'TREE'. 
  PrepTree(data=foodtree)
  # It is OK to see the same message as the previous line. 
  
  
  # Make a phyloseq object with OTU, TAX, samples, and foodtree by using the phyloseq function.
  phyfoods <- phyloseq(OTU, TAX, SAMPLES, TREE)
  # It is OK to see the same message as the previous line. 
  
  ### Make a phyloseq object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Make a phyloseq object with OTU, TAX, samples, and foodtree by using the phyloseq function.
  phyfoods <- phyloseq(OTU, TAX, SAMPLES, TREE)
  # It is OK to see the same message as the previous line. 
  
  # Change to the folder called "Ordination" in your "Ordination" folder.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/Laboratory_data/Ordination/")
  
  # ===============================================================================================================
  # Use your phyloseq object and perform ordination - WEIGHTED unifrac distance
  # ===============================================================================================================

  # Perform Principal Coordinate Analysis (PCoA) with WEIGHTED unifrac distance of your food data.
  # This may take a few minutes depending on your data size.
  # e.g. a large phyloseq object (7.9 MB) could take a few minutes.
  ordinated_w <- phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=TRUE)

  # Save the percent variance explained by the axes as a vector to use in plots.
  eigen_percent_w <- ordinated_w$values$Relative_eig

  # Save the percent variance explained as a txt file.
  Eigen(eigen.input = eigen_percent_w,
        output.fn="Food_D12_FC_cc_f_males60to79Pred55_red_Lv3_ord_WEIGHTED_eigen.txt")

  # Merge the first n axes to the metadata and save it as a txt file.
  # The merged dataframe, 'meta_usersdf', will be used for plotting.
  MergeAxesAndMetadata_NHANES(ord.object= ordinated_w, number.of.axes= 10, meta.data= demog_glu,
                              output.fn= "Food_D12_FC_cc_f_males60to79Pred55_red_Lv3_ord_WEIGHTED_meta_users.txt")
  # View(MergeAxesAndMetadata_NHANES)

  # Load the output again for plotting.
  loaded_glu_w <- read.table("Food_D12_FC_cc_f_males60to79Pred55_red_Lv3_ord_WEIGHTED_meta_users.txt",
                             sep="\t", header=T)

  # Convert the GLU_index as a factor to plot it in order.
  loaded_glu_w$GLU_index <- factor(loaded_glu_w$GLU_index, levels= c("Normal", "Prediabetic", "Diabetic"))
  print(table(loaded_glu_w$GLU_index))

  ### beta-dispersion test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # The GLU_index groups look different. Use beta-diversity and adonis tests to see
  # if they are they actually distinct from one another.

  # Generate a weighted unifrac distance matrix.
  dist_matrix_w <- phyloseq::distance(phyfoods, method = "wunifrac") # weighted

  # Dispersion test and plot
  # vegan::betadisper computes centeroids and distance of each datapoint from it.
  dispr_w <- vegan::betadisper(d=dist_matrix_w, phyloseq::sample_data(phyfoods)$GLU_index)

  # ---------------------------------------------------------------------------------------------------------------
  # # Can show the centroids and dispersion of each group.
  # plot(dispr_w)

  # # Or show the distance to centroid of each datapoint.
  # boxplot(dispr_w, xlab = "")

  # Use dispr to do a permutation test for homogeneity of multivariate dispersion
  perm_res <- vegan::permutest(dispr_w, perm=5000)

  # Save the p-value for Groups
  perm_res_w_p <- perm_res$tab$`Pr(>F)`[1]

  pvalue_table_w[i, 2] <- perm_res_w_p

  ### adonis test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  adonis_res <- vegan::adonis(dist_matrix_w ~ phyloseq::sample_data(phyfoods)$GLU_index, permutations = 5000)

  # Check
  head(dist_matrix_w)

  adonis_res_w_p <- adonis_res$aov.tab$`Pr(>F)`[1]

  # Save it to pvalue_table.
  pvalue_table_w[i, 3] <- adonis_res_w_p

  # If overall adonis is significant, you can run pairwise adonis to see which group pairs are different.
  if(adonis_res_w_p < 0.05){

    pairwise_adonis_res <- pairwise.adonis(dist_matrix_w, phyloseq::sample_data(phyfoods)$GLU_index, perm = 5000,
                                           p.adjust.m = "fdr")

    # Sort it in the order of pairs.
    pairwise_adonis_res_sorted <- pairwise_adonis_res[order(pairwise_adonis_res$pairs) , ]

    # Take out the p-value for each combination.
    Dia_vs_Nor_p <- pairwise_adonis_res_sorted[1, 6]
    Dia_vs_Pre_p <- pairwise_adonis_res_sorted[2, 6]
    Pre_vs_Nor_p <- pairwise_adonis_res_sorted[3, 6]

    # Put 1 if p-value for each comparison is less than 0.05, and put 0 if not.
    if(Dia_vs_Nor_p < 0.05){counttable_w[i, "Dia_vs_Nor"] <- 1}else{counttable_w[i, "Dia_vs_Nor"] <- 0 }
    if(Dia_vs_Pre_p < 0.05){counttable_w[i, "Dia_vs_Pre"] <- 1}else{counttable_w[i, "Dia_vs_Pre"] <- 0 }
    if(Dia_vs_Pre_p < 0.05){counttable_w[i, "Pre_vs_Nor"] <- 1}else{counttable_w[i, "Pre_vs_Nor"] <- 0 }

    # Save it to mylist.
    pairwise_adonis_list_w[[i]] <- pairwise_adonis_res_sorted

  }else{  # if adonis p-value >= 0.05, put NAs.

    # Put 1 if the p-value for each comparison is less than 0.05, and put 0 if not.
    counttable_w[i, "Dia_vs_Nor"] <- NA
    counttable_w[i, "Dia_vs_Pre"] <- NA
    counttable_w[i, "Pre_vs_Nor"] <- NA

    # Save NA to the list.
    pairwise_adonis_list_w[[i]] <- NA

  }

  # Add average at the last row of the p-value table.
  pvalue_table_w_ave <- rbind(pvalue_table_w, c("Mean", colMeans(pvalue_table_w[, -1], na.rm = T)))

  # Add total at the last row in counttable.
  counttable_w_sum <- rbind(counttable_w, c("Total", colSums(counttable_w[, -1], na.rm = T)))

  print(pvalue_table_w_ave)
  print(pairwise_adonis_list_w)
  print(counttable_w_sum)

  print(paste0( i, " was done."))
  
  # # ===============================================================================================================
  # # ===============================================================================================================
  # # Use your phyloseq object and perform ordination - UNweighted unifrac distance
  # # ===============================================================================================================
  # # ===============================================================================================================
  # 
  # # Perform Principal Coordinate Analysis (PCoA) with UNweighted unifrac distance of your food data.
  # # This may take a few minutes depending on your data size.
  # # e.g. a large phyloseq object (7.9 MB) could take a few minutes.
  # ordinated_u <- phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted= FALSE)
  # 
  # # Save the percent variance explained by the axes as a vector to use in plots.  
  # eigen_percent_u <- ordinated_u$values$Relative_eig
  # 
  # # Save the percent variance explained as a txt file.
  # Eigen(eigen.input = eigen_percent_u, 
  #       output.fn="Food_D12_FC_cc_f_males60to79Pred55_red_Lv3_ord_UNweighted_eigen.txt")
  # 
  # # Merge the first n axes to the metadata and save it as a txt file. 
  # # The merged dataframe, 'meta_usersdf', will be used for plotting.
  # MergeAxesAndMetadata_NHANES(ord.object= ordinated_u, number.of.axes= 10, meta.data= demog_glu, 
  #                             output.fn= "Food_D12_FC_cc_f_males60to79Pred55_red_Lv3_ord_UNweighted_meta_users.txt")
  # # View(MergeAxesAndMetadata_NHANES)
  # 
  # # Load the output again for plotting.
  # loaded_glu_u <- read.table("Food_D12_FC_cc_f_males60to79Pred55_red_Lv3_ord_UNweighted_meta_users.txt",
  #                            sep="\t", header=T)
  # 
  # # Convert the GLU_index as a factor to plot it in order.
  # loaded_glu_u$GLU_index <- factor(loaded_glu_u$GLU_index, levels= c("Normal", "Prediabetic", "Diabetic"))
  # print(table(loaded_glu_u$GLU_index))
  # 
  # ### beta-dispersion test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # # The GLU_index groups look different. Use beta-diversity and adonis tests to see
  # # if they are they actually distinct from one another.
  # 
  # # Generate a weighted unifrac distance matrix.
  # dist_matrix_u <- phyloseq::distance(phyfoods, method = "unifrac") # UNweighted
  # 
  # # Dispersion test and plot
  # # vegan::betadisper computes centeroids and distance of each datapoint from it. 
  # dispr_u <- vegan::betadisper(d=dist_matrix_u, phyloseq::sample_data(phyfoods)$GLU_index)
  # 
  # # ---------------------------------------------------------------------------------------------------------------
  # # # Can show the centroids and dispersion of each group. 
  # # plot(dispr_u)
  # 
  # # # Or show the distance to centroid of each datapoint.
  # # boxplot(dispr_u, xlab = "")
  # 
  # # Use dispr to do a permutation test for homogeneity of multivariate dispersion
  # perm_res <- vegan::permutest(dispr_u, perm=5000)
  # 
  # # Save the p-value for Groups
  # perm_res_u_p <- perm_res$tab$`Pr(>F)`[1] 
  # 
  # pvalue_table_u[i, 2] <- perm_res_u_p
  # 
  # ### adonis test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # adonis_res <- vegan::adonis(dist_matrix_u ~ phyloseq::sample_data(phyfoods)$GLU_index, permutations = 5000)
  # 
  # # Check
  # head(dist_matrix_u)
  # 
  # adonis_res_u_p <- adonis_res$aov.tab$`Pr(>F)`[1]
  # 
  # # Save it to pvalue_table.
  # pvalue_table_u[i, 3] <- adonis_res_u_p
  # 
  # # If overall adonis is significant, you can run pairwise adonis to see which group pairs are different.
  # if(adonis_res_u_p < 0.05){
  # 
  #   pairwise_adonis_res <- pairwise.adonis(dist_matrix_u, phyloseq::sample_data(phyfoods)$GLU_index, perm = 5000,
  #                                          p.adjust.m = "fdr")    
  #   
  #   # Sort it in the order of pairs.
  #   pairwise_adonis_res_sorted <- pairwise_adonis_res[order(pairwise_adonis_res$pairs) , ]
  #   
  #   # Take out the p-value for each combination.
  #   Dia_vs_Nor_p <- pairwise_adonis_res_sorted[1, 6]
  #   Dia_vs_Pre_p <- pairwise_adonis_res_sorted[2, 6]
  #   Pre_vs_Nor_p <- pairwise_adonis_res_sorted[3, 6]
  #   
  #   # Put 1 if p-value for each comparison is less than 0.05, and put 0 if not. 
  #   if(Dia_vs_Nor_p < 0.05){counttable_u[i, "Dia_vs_Nor"] <- 1}else{counttable_u[i, "Dia_vs_Nor"] <- 0 }
  #   if(Dia_vs_Pre_p < 0.05){counttable_u[i, "Dia_vs_Pre"] <- 1}else{counttable_u[i, "Dia_vs_Pre"] <- 0 }
  #   if(Dia_vs_Pre_p < 0.05){counttable_u[i, "Pre_vs_Nor"] <- 1}else{counttable_u[i, "Pre_vs_Nor"] <- 0 }
  #   
  #   # Save it to mylist.
  #   pairwise_adonis_list_u[[i]] <- pairwise_adonis_res_sorted
  # 
  # }else{  # if adonis p-value >= 0.05, put NAs. 
  # 
  #   # Put 1 if the p-value for each comparison is less than 0.05, and put 0 if not. 
  #   counttable_u[i, "Dia_vs_Nor"] <- NA 
  #   counttable_u[i, "Dia_vs_Pre"] <- NA 
  #   counttable_u[i, "Pre_vs_Nor"] <- NA
  #   
  #   # Save it to the list.
  #   pairwise_adonis_list_u[[i]] <- NA
  #   
  # }
  # 
  # # Add average at the last row of the p-value table.
  # pvalue_table_u_ave <- rbind(pvalue_table_u, c("Mean", colMeans(pvalue_table_u[, -1], na.rm = T))) 
  # 
  # # Add total at the last row in counttable.
  # counttable_u_sum <- rbind(counttable_u, c("Total", colSums(counttable_u[, -1], na.rm = T))) 
  # 
  # print(pvalue_table_u_ave)
  # print(pairwise_adonis_list_u)
  # print(counttable_u_sum)
  # 
  # print(paste0( i, " was done."))
  
}
  
write.table(pvalue_table_w_ave, "clipboard", sep="\t", quote=F, row.names = F)
write.table(counttable_w_sum, "clipboard", sep="\t", quote=F, row.names = F)

