# ===============================================================================================================
# Quartiles of pulse consumers, depending on the diversity (Shannons?) of their legume consumption patterns.
# Version 1
# Created on 02/09/2023 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Divide NHANES participants into four groups 
# ===============================================================================================================
 
# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
  source("lib/add_gender_and_age.R") # to use AddGenderAgeGroups function.  
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  
  
# ---------------------------------------------------------------------------------------------------------------
#   
# Load averaged totals data, nutrition & food categories with demographic, gender-age, body measurements, and 
# metadata. From line 118 in 04_add_meta_GLU_index_NHANES.R.
  
  totals <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta.txt")
  dim(totals)  # 4207 people.  
  head(totals, 2)
  
#   # Age
#   hist(totals$RIDAGEYR)
#   # Gender
#   table(totals$Gender, useNA="ifany")
#   # BMI
#   hist(totals$BMXBMI)
#   # Diets 
#   table(totals$DRQSDIET, useNA="ifany")
#   # 1    2 
#   # 674 3533 
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Take a look at the columns that starts with "PF" (protein foods)
# 
#   # df[, grepl(x=names(df), pattern="^x")]
#   PF <- totals[, grepl(x=names(totals), pattern="^PF_")]
#   dim(PF)  
#   colnames(PF)  
#   write.table(head(PF, 10), "clipboard", sep="\t", row.names=F, quote=F)  
# # PF_TOTAL is the sum of all the PF_XXX foods EXCEPT PF_LEGUMES!!!
# # Probably because legumes could be counted as Veg or protein foods, but not both.
# # So it's giving researchers choices whether to include legumes as veg or protein, probably.
# 
# # 'PF_MPS_TOTAL' is the sum of Meat, Poultry, Seafood.. 
#   diet_mps <- totals[, c("DRQSDIET", "PF_MPS_TOTAL")]
#   boxplot(x=diet_mps$DRQSDIET, diet_mps$PF_MPS_TOTAL)
#   # Those folloing special diets have very low 'PF_MPS_TOTAL'.
#   
#   summary(PF$PF_LEGUMES*28.3495)
#   hist(PF$PF_LEGUMES)   # 0-15. PF_Legumes are oz. equivalent.
#   hist(PF$PF_LEGUMES*28.3495)   # 1 oz. = 28.3495 grams.
#   summary(PF$PF_MPS_TOTAL)       # 0-31 oz.
#   hist(PF$PF_MPS_TOTAL*28.3495) 
#   
# # ---------------------------------------------------------------------------------------------------------------
# # PF_TOTAL_LEG <- PF_TOTAL + PF_LEGUME
#   totals$PF_TOTAL_LEG <- totals$PF_TOTAL + totals$PF_LEGUME
# # PF_LEG_perTOTAL is the percentage of legumes in all the protein foods.   
#   totals$PF_LEG_perTOTAL <- totals$PF_LEGUME / totals$PF_TOTAL_LEG *100 
#   
#   hist(totals$PF_LEG_perTOTAL)
#   
#   plot(totals$PF_LEGUMES, totals$PF_LEG_perTOTAL)
#   cor.test(totals$PF_LEGUMES, totals$PF_LEG_perTOTAL)
#   
#   hist(totals$PF_TOTAL_LEG)  
#   summary(totals$PF_LEG_perTOTAL)  
#   # There is 1 NA...
#   totals[ is.na(totals$PF_LEG_perTOTAL), ] 
#   # No protein intake, that is why. (But this person consumed dairy foods.)
# 
# # Compare those who are following special diets and who are not.  
#   boxplot(x= totals$DRQSDIET, totals$PF_LEG_perTOTAL)
# # Those who are following a diet (DRQSDIET=1) have low PF_LEG_perTOTAL... hmmm   
  
# ---------------------------------------------------------------------------------------------------------------
# Measure the diet diversity of each individual.
# Tutorial: https://www.programmingr.com/shannon-diversity-index-the-diversity-function-in-r/
  library(vegan)
  # vegan::diversity calculates Shannon's diversity index one by one.
  # Need to have a table with SEQN as rows and food items as columns... food table?
  
  # Load the food items data where QC-ed indivisuals were removed based on their totals data. should be 4207 people. 
  food <- read.delim("Food_D12_FC_QC_demo_QCed.txt", sep= "\t", header=T)
  dim(food)
  length(unique(food$SEQN)) # 4207 people.
  
  # pick up only the rows that contain food with its foodcode starting from 4. 
  str(food)
  summary(food$Food_code)
  
  food4s <- subset(food, Food_code > 39999999 & Food_code < 50000000)
  summary(food4s$Food_code) # OK, only contains 4xxxxxxxs.
  length(unique(food4s$Food_code)) # only 243 unique food items!
  length(unique(food4s$SEQN))      # 2108 people.
  
  # How much duplicates are there? 
  fourtable <- as.data.frame(table(food4s$Food_code))
  head(fourtable[ order(fourtable$Freq, decreasing = T), ], 10)
      # Var1 Freq
  # 42202000  323
  # 41420300  191
  # 42101130  121
  # 41104020  111
  # 41104011   97
  # 42116050   83
  # 41201020   77
  # 43104000   72
  # 42111200   67
  # 42111100   64
  
  # Some foods are consumed so many times!!
  hist(fourtable$Freq)
  
  write.table(food4s, "Food_D12_FC_QC_demo_QCed_4s.txt", sep= "\t", row.names=F,quote=F)

# ===============================================================================================================
# ===============================================================================================================
  
# ---------------------------------------------------------------------------------------------------------------
# To calcualte diversity of 4xxxxxxxs for each SEQN, Need to create a food OTU table.
# then need to make a foodtree and a tax table.
  
# From 53_PF_Pulses_NHANES_create_food_tree_Leg1-3_clean.R
  # Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")
  
  # Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
  # ---------------------------------------------------------------------------------------------------------------
  # Load source scripts
  source("lib/specify_data_dir.R")
  
  source("lib/Food_tree_scripts/newick.tree.r")
  # source("lib/Food_tree_scripts/check.db.r")
  source("lib/Food_tree_scripts/format.foods.r")
  # source("lib/Food_tree_scripts/filter.db.by.diet.records.r")
  source("lib/Food_tree_scripts/make.food.tree.r")
  source("lib/Food_tree_scripts/make.food.otu.r")
  source("lib/Food_tree_scripts/make.fiber.otu.r")
  source("lib/Food_tree_scripts/make.dhydrt.otu.r")
  
  # You can come back to the main directory by:
  setwd(main_wd)   
  
# ===============================================================================================================
# Load and prep data for generating food trees 
# ===============================================================================================================
  
  # Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/PF")
  
  MakeFoodTree(nodes_fn="../../Food_tree_eg/NodeLabelsMCT.txt", 
               addl_foods_fn = NULL,
               num.levels = 3,
               food_database_fn =            "Food_D12_FC_QC_demo_QCed_4s.txt",  
               output_tree_fn =     "Foodtree/Food_D12_FC_QC_demo_QCed_4s_3Lv.nwk", 
               output_taxonomy_fn = "Foodtree/Food_D12_FC_QC_demo_QCed_4s_3Lv.tax.txt"
  )
  
  # --------------------------------------------------------------------------------------------------------------
  # Generate OTU tables for downstream analyses; IT MAY TAKE SOME TIME.
  # It is OK to see the following warning message:
  # In write.table(fiber.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
  # appending column names to file.
  MakeFoodOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_4s.txt",   
              food_record_id =  "SEQN",                              # The ID of your participants
              food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_4s_3Lv.tax.txt",       # Your taxonomy file produced by MakeFoodTree.
              output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_4s_3Lv.food.otu.txt")  # Output otu file to be saved.
  
  otu <- read.delim("Foodtree/Food_D12_FC_QC_demo_QCed_4s_3Lv.food.otu.txt")
  dim(otu) # 243 x 2110 
  head(colnames(otu)) # X.FOODID, X87496, X88725, ...
  tail(colnames(otu)) # X92316, ..., taxonomy.
  otu[1:4, 1:4]
  # OTU done!
  
# ===============================================================================================================
# calcualate diversity of 4xxxxxxxs for each SEQN
# ===============================================================================================================
  
# Take out the foodID (description) and taxonomy from otu.
  otu2 <- otu[, 2: (ncol(otu)-1) ]
  head(colnames(otu2)) # no FoodID, OK.
  tail(colnames(otu2)) # no taxonomy, OK.
  otu2[1:2,1:2]

# transpose so that the SEQN will come to   
 otu2t <- as.data.frame(t(otu2)) 
 dim(otu2t)
 length(otu$X.FOODID)
 
 # Add taxonomy as the column names of otu2t. 
 colnames(otu2t) <- otu$X.FOODID
 head(otu2t,2)  
 
 head(rowSums(otu2t))    # total consumption by SEQN.
 summary(rowSums(otu2t)) # consumption by SEQN. no zero values, OK.
 # Each row of otu2t is SEQN. So, diversity needs to be calculated per each row.

 head(colSums(otu2t))    # total consumption by item.
 summary(colSums(otu2t)) # consumption per item. no zero values. OK. 
 
 # Calculate diversity.
 ?diversity
 D = diversity(otu2t[1, ], 'shannon') # first row of otu2t = first SEQN.
 D = diversity(otu2[, 1], 'shannon') # first column of otu2 = first SEQN; before transforming.
 D # D values are the same, so all is good. 

 
   # If calculate Shannon diversity by hand, it returns error when your data contains zero values.
   # vegan::dievrsity must be doing a clever trick to avoid NaN, as there are many zero values in OTU tables. 
   # pop = c(0, 100, 500, 200, 600, 500, 400, 700, 900, 800)
   # pop = otu2t[, 1]
   # n = pop
   # N = sum(pop)
   # p = n/N
   # H = -sum(p*log(p))  # Shannon diversity by hand. It works if all values you have are non-zero.
   # H 

# Do a loop to calculate shannon diversity for all SEQN.
 otu2t[1:2, 1:2] # SEQN as rows and foods as columns.
  
# make a table to save results. 
  SEQNdiv <- as.data.frame(matrix(nrow = nrow(otu2t) , ncol = 4))
  colnames(SEQNdiv) <- c("SEQN", "Shannon", "Simpson", "Invsimpson")
  head(SEQNdiv)
  
 for( i in 1: nrow(otu2t) ){
  
   # print(i)
   # print(otu2t[i, 1:2])
   
   # calculate D for each row(SEQN) with different methods.
   SEQNdiv[i, 1] <- rownames(otu2t)[i]
   SEQNdiv[i, 2] <- diversity(otu2t[i, ], 'shannon')
   SEQNdiv[i, 3] <- diversity(otu2t[i, ], 'simpson')
   SEQNdiv[i, 4] <- diversity(otu2t[i, ], 'invsimpson')

   # print(head(SEQNdiv))
   
 } 

  head(SEQNdiv)  
  table(is.na(SEQNdiv))  # no NAs. Good.
  summary(SEQNdiv$Shannon)
  nrow( subset(SEQNdiv, Shannon==0 ))
  summary(SEQNdiv$Simpson)
  nrow( subset(SEQNdiv, Simpson==0 ))
  summary(SEQNdiv$Invsimpson)
  nrow( subset(SEQNdiv, Invsimpson==1 ))
  
  par(mfrow = c(2, 2))
  hist(SEQNdiv$Shannon, main="Shannon diversity", xlab="", breaks=10)
  hist(SEQNdiv$Simpson, main="Simpson diversity", xlab="", breaks=10)
  hist(SEQNdiv$Invsimpson, main="Invsimpson diversity", xlab="", breaks=10)
  par(mfrow = c(1, 1))
  # some have 0 diversity?? --> If a row has only one non-zero values, then diversity will be zero. e.g. c(20,0,0,0,0,0,0) 
  # None of them are normally distributed because of a lot of zero diversity values.

# ---------------------------------------------------------------------------------------------------------------
# Anyway, group individuals based on their diversity values.
# Remove the "X" in the SEQNdiv$SEQN for merging. 
  SEQNdiv$SEQN_noX <- gsub(SEQNdiv$SEQN, pattern = "X", replacement = "") 
  head(SEQNdiv)
  # Bring SEQN_noX to the first column. 
  SEQNdiv_2 <- SEQNdiv[, c(5,2,3,4,1)] 
  head(SEQNdiv_2)
  # Rename the columns.
  colnames(SEQNdiv_2)[5] <- "XSEQN"   
  colnames(SEQNdiv_2)[1] <- "SEQN"  # numbers only; to be used for merging.   
    
  length(unique(totals$SEQN))  # 4207
  length(unique(SEQNdiv$SEQN)) # 2108 because only those who consumed 4xxxxxxx were selected. 
  
# First, need to add the diversity values to totals. Only take the rows present in both datasets.
  totals_div <- merge(totals, SEQNdiv_2, by='SEQN')
  head(totals_div)
  
# ---------------------------------------------------------------------------------------------------------------
# dplyr has quantile function... 
  library(tidyverse)
  # temp <- temp %>% mutate(quartile = ntile(value, 4))
  totals_div_2 <-  totals_div %>% mutate(quartile = ntile(Shannon, 4)) # percentage
  # quartile column is added. 
  head(totals_div_2)
  table(totals_div_2$quartile)

  for(i in 1:4){
    print(i)
    quai <- subset(totals_div_2, quartile == i)
    print(summary(quai$Shannon))
  }  
  # Group 1 and 2 are all zero.. as expected.  

# ---------------------------------------------------------------------------------------------------------------
# Select only those who have diversity > 0 first.
  totals_div_3 <- subset(totals_div_2, Shannon > 0)
  
  nrow(totals_div_3) /  nrow(totals_div_2)  * 100
  # 39% of the individuals have diversity > 0.
  
  # Split the nonzero diversity group into 2-tiles.
  # totals_leg <-  totals_leg %>% mutate(leg_tritile = ntile(PF_LEG_perTOTAL, 3))
  totals_div_3 <-  totals_div_3 %>% mutate(halves = ntile(Shannon, 2))
  table( totals_div_3$halves)
  plot(totals_div_3$halves, totals_div_3$Shannon)
  
  for(i in 1:2){
    print(i)
    selectedi <- subset(totals_div_3, halves == i)
    print(summary(selectedi$Shannon))
  }  
  
# ---------------------------------------------------------------------------------------------------------------
  # Mark them as
  # None (50%) ... Did not report any foods with Food ID of 4xxxxxxxx. Shannon's diversity = NA.
  # Sha0 (30%) ... Reported 1 food with Food ID of 4xxxxxxx.           Shannon's diversity = 0. 
  # Sha1 (10%) ... Reported >1 foods with Food ID of 4xxxxxxx. lower.  Shannon's diversity > 1. 
  # Sha2 (10%) ... Reported >1 foods with Food ID of 4xxxxxxx. upper.  Shannon's diversity > 1. 
  
# Define Div1 and Div2. 
  totals_div_3$DivGroup <- 
    ifelse(
      totals_div_3$halves == 1,
      totals_div_3$DivGroup <- 'Div1',
      totals_div_3$DivGroup <- 'Div2'
    )
  
  table(totals_div_3$DivGroup)
  head(totals_div_3[1:10, c("DivGroup", "halves")]  )
  
  # Take only the SEQN and DivGroup
  SEQN_Div12 <- totals_div_3[, c("SEQN", "DivGroup")]
  
# Define Div0.
  totals_div_4 <- subset(totals_div_2, Shannon == 0)
  
  totals_div_4$DivGroup <- 'Div0'
  
  # Take only the SEQN and DivGroup
  SEQN_Div0 <- totals_div_4[, c("SEQN", "DivGroup")]
  
# Define DivNA.
  # Define "Not in" function.  By default it's not existent.
  `%!in%` <- Negate(`%in%`)
  
  totals_not_in_SEQNdiv_2 <- totals[totals$SEQN %!in% SEQNdiv_2$SEQN, ]  
  dim(totals_not_in_SEQNdiv_2) #2099, good!
   
  # This should return zero rows, because they have no rows in common. 
  merge(totals_not_in_SEQNdiv_2, SEQNdiv_2, by="SEQN")
  
  totals_not_in_SEQNdiv_2$DivGroup <- 'DivNA'
  
  # Take only the SEQN and DivGroup
  SEQN_DivNA <- totals_not_in_SEQNdiv_2[, c("SEQN", "DivGroup")]
  
# Combine all the rows for merging.
  SEQN_Div_NA_012 <-    rbind(SEQN_DivNA, SEQN_Div0, SEQN_Div12)
  length(unique(SEQN_Div_NA_012$SEQN))

# Check
  table(SEQN_Div_NA_012$DivGroup, useNA = "ifany")
  
# Merge this with the totals.
  totals_divgroup <- merge(totals, SEQN_Div_NA_012, by="SEQN")
  
# Now, all the rows have DivGroups. 
  table(totals_divgroup$DivGroup, useNA = "ifany")

# Change DivGroup into a factor.
  totals_divgroup$DivGroup_f <- factor(totals_divgroup$DivGroup, 
                                       levels = c('DivNA', 'Div0', 'Div1', 'Div2') )
  
  plot(totals_divgroup$DivGroup_f, totals_divgroup$PF_LEG_perTOTAL)
  plot(totals_divgroup$DivGroup_f, totals_divgroup$PF_LEGUMES)
  # as expected. Good. 
  
# So up to here, the individuals in totals were grouped into 4 groups depending on their consumption of 
# 4xxxxxxx foods (or the lack thereof). # The totals_divgroup has DivGroup_f column. 
  table(totals_divgroup$DivGroup_f, useNA = "ifany")
  # DivNA  Div0  Div1  Div2 
  # 2099  1295   407   406 
  
  write.table(totals_divgroup, "Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup.txt",
              sep="\t", row.names=F, quote=F)
  

# ---------------------------------------------------------------------------------------------------------------
  
#  
  
  