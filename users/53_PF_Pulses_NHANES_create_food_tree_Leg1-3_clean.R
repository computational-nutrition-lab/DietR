# ===============================================================================================================
# Generate food tree out of PF_LEGUME data from Leg1= - Leg3 groups.
# Put the items directly into MakeFoodTree function.  
# Version 1
# Created on 01/31/2023 by Rie Sadohara
# ===============================================================================================================

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# Load the data.tree package necessary for newick.tree.r, and if it is not installed, install it. 
  if (!require("data.tree", quietly = TRUE))install.packages("data.tree")
  
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

# # Load the males60to79 people. Note this is a total data (1 row/person).
#   # totals_males60to79 <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79.txt", 
#   #                                  sep="\t", header=T)
# 
#   totals <- read.delim("../Total_D12_FC_QC_mean_QC_demo_ga_body_meta_Leg.txt")
#   head(totals, 1)
#   dim(totals)
#   length(unique(totals$SEQN)) # 4207
#   
# # Make the individuals as a vector.
#   selectedind <- totals$SEQN # 4207
# 

# Load the input file (all food record data) to be filtered.
  all.food.record <- read.table("../Food_D12_FC_QC_demo_QCed.txt", sep="\t", header=T)
  length(unique(all.food.record$SEQN)) # 4207
  
######## NEWLY ADDED ##########  
# load the items in the FPED 15-16 database with 1 or more oz./100 g PF_LEGUME.
  morethanone <- read.delim("../FPED/PFED_1516_PF_LEGUMES_more_than_1.txt")
  nrow(morethanone) # only 110 items.
  head(morethanone[100:112, 1:2])

# Filter all.food.record for only the food that is in morethanone by FOODCODE.
  head(all.food.record)  
  length(unique(all.food.record$SEQN))      # 4,207 
  length(unique(all.food.record$Food_code)) # 5,063 food items in all.food.record of 4207 people.
  
  # Keep only the legume-containing food items consumed by 4207 people. 
  all.food.record_leg <- all.food.record[ all.food.record$Food_code %in% morethanone$FOODCODE ,  ]
  
  # How many PF_LEGUME food items in all.food.record_leg?
  length(unique(all.food.record_leg$Food_code))  # 58 food items.
  length(unique(all.food.record_leg$SEQN))       # 703 people.
  table(all.food.record_leg$Food_code)
  
  # # Need to re-define selectedind.
  # selectedind <- unique(all.food.record_leg$SEQN)
  # length(selectedind)
  
  # # Take a look
  # all.food.record_leg[1:5, c("Food_code", "Main.food.description", "FoodAmt", "PF_LEGUMES") ]
  # colnames(all.food.record_leg)
  
  # Save. This will be the input for the following procedures.
  write.table(all.food.record_leg, "Food_D12_FC_QC_demo_QCed_leg.txt", 
              sep="\t", row.names=F, quote=F) 
  
######## TILL HERE ##########  
                                       
# Select only the individuals listed in 'selectedind'.
  # sel.food.record <- all.food.record[all.food.record$SEQN %in% selectedind, ]

# Confirm the two contains the same set of individuals. 
  # identical(unique(sel.food.record$SEQN), selectedind)

# Save. This will be the input for the following procedures.
  # write.table(sel.food.record, "Food_D12_FC_QC_demo_QCed_Leg_morethanone.txt", 
  #             sep="\t", row.names=F, quote=F) 

# ===============================================================================================================
# Limit to just the foods reported in your study (use formatted dietrecords.txt as the input) 
# ===============================================================================================================
# # Keep only the foods reported in your study. This is to make data compatible to create a food tree.
#   FilterDbByDietRecords(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt",
#                         food_records_fn  = "Food_D12_FC_QC_demo_QCed_Leg_morethanone.txt",  # output of filtering above.
#                         output_fn =        "Food_D12_FC_QC_demo_QCed_Leg_morethanone_red.txt")
# 
#   ####
#   NHANESdatabase <- read.delim("../../Food_tree_eg/NHANESDatabase.txt")
#   dim(NHANESdatabase) # 8429 food items
#   head(NHANESdatabase)
#   food_records_fn  <- read.delim("Food_D12_FC_QC_demo_QCed_Leg_morethanone.txt", quote="", colClasses='character')  # output of filtering above.
#   dim(food_records_fn) # 900 x 177
#   
#   length(unique(food_records_fn$SEQN)) # 703 
#   length(unique(food_records_fn$Food_code)) # 58
#   
#   merged <- merge(food_records_fn[, c('FoodID', "SEQN")], NHANESdatabase[, c("FoodID", "Main.food.description")], all.x=T)
#   as.data.frame(table(merged$Main.food.description, useNA = 'always'))
#   # some foods are missing in merged. that means some foods in food_records_fn were not in NHANESdatabase.
#   length(unique(merged$SEQN)) # 703 
#   length(unique(merged$FoodID)) # 58
#   # merged, allx=T done correctly. 
# 
#   output_fn = read.delim("Food_D12_FC_QC_demo_QCed_Leg_morethanone_red.txt", quote="", colClasses="character")
#   dim(output_fn) # 45 x 6 food items
#   head(output_fn, 1)
#   colnames(output_fn)
#   # So after reducing, only 45 items exist in the xxx_red.txt. ????
#   # Asked Abby --> NHANESdatabase.txt is not needed, it was used for her research that included non-NHANES foods. 
#   # So, I do not need to FilterDbByDietRecords.
#   # But need to make a txt that has 6 columns of: "FoodCodes", "drxfcsd", "Main.food.description", 
#   # "Old.Main.food.description", "ModCode", "FoodID". 
#   which(colnames(food_records_fn) == "FoodID")
#   head(food_records_fn$FoodID) # "FoodCodes" doesn't exist in  food_records_fn.  

  # # ftc means food tree columns.
  # PrepFoodtreeInput(food_records_fn = "Food_D12_FC_QC_demo_QCed_leg.txt",
  #                   out_fn          = "Food_D12_FC_QC_demo_QCed_leg_ftc.txt")
  
  # prep_red = read.delim("Food_D12_FC_QC_demo_QCed_leg_ftc.txt", quote="", colClasses="character")
  # dim(prep_red) # 900 x 5 food items
  # head(prep_red, 6)
  # max(as.numeric(prep_red$ModCode)) # ModCode are all zero.
  
# # Use CheckDB function to check if any food reported in Food_D12_FC_QC_demo_QCed.txt is missing in the 
#   # NHANES food database. # If there is, those will be written in the output file named xxx_missing.txt.
#   check.db(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt", 
#            food_records_fn =  "Food_D12_FC_QC_demo_QCed_Leg_morethanone_red.txt",
#            output_fn =        "Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_missing.txt")
#   View(check.db)
  
  # check.db looks for food items found in food_records_fn but not in food_database_fn.
  # it is not needed for now, because we only took food items in NHANES and FPED and no new 
  # food items have been added..
  
 
 # This should be enough for making food tree.
  MakeFoodTree(nodes_fn="../../Food_tree_eg/NodeLabelsMCT.txt", 
               addl_foods_fn = NULL,
               num.levels = 3,
               food_database_fn =            "Food_D12_FC_QC_demo_QCed_leg.txt",  
               output_tree_fn =     "Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.nwk", 
               output_taxonomy_fn = "Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.tax.txt"
  )
  
  tax <- read.delim("Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.tax.txt")
  dim(tax) # 58 x 3, good.
  colnames(tax)
  head(tax, 1)  
  
# --------------------------------------------------------------------------------------------------------------
# Generate OTU tables for downstream analyses; IT MAY TAKE SOME TIME.
# It is OK to see the following warning message:
# In write.table(fiber.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
# appending column names to file.
  MakeFoodOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_leg.txt",   
              food_record_id =  "SEQN",                              # The ID of your participants
              food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.tax.txt",       # Your taxonomy file produced by MakeFoodTree.
              output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.food.otu.txt")  # Output otu file to be saved.
  
  otu <- read.delim("Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.food.otu.txt")
  dim(otu) # 58 x 705 
  colnames(otu) # X.FOODID, X87496, X88725, ..., taxonomy.
  
  # Make a food otu table with data in grams of fiber per food.
  MakeFiberOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_leg.txt", 
               food_record_id=   "SEQN", 
               food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.tax.txt", 
               output_fn=        "Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.fiber.otu.txt")
  
  # Make a food otu table as dehydrated grams per kcal.
  MakeDhydrtOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_leg.txt", 
                food_record_id =  "SEQN", 
                food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.tax.txt", 
                output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_leg_3Lv.dhydrt.otu.txt")  
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)
  

#   
#   
# # Use CheckDB function to check if any food reported in Food_D12_FC_QC_demo_QCed.txt is missing in the 
# # NHANES food database. # If there is, those will be written in the output file named xxx_missing.txt.
#   check.db(food_database_fn = "../../Food_tree_eg/NHANESDatabase.txt", 
#            food_records_fn =  "Food_D12_FC_QC_demo_QCed_Leg_morethanone_red.txt",
#            output_fn =        "Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_missing.txt")
# 
# # Load the output and check if the output contains anything?
#   mmm = read.table("Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_missing.txt", sep="\t", header=T)
#   head(mmm)
#   # Has item(s) ===> put this missing.txt file in addl_foods_fn argument of MakeFoodTree.
#   # Empty       ===> put NULL in addl_foods_fn argument of MakeFoodTree.
#   
# # Create food tree with the reduced dataset (only reported foods) classified at 
# # a desired level of classification (Lv. 1-6).
# # "NodeLabelsMCT.txt" has a list of food levels and names, which comes with the DietR package.
#   MakeFoodTree(nodes_fn="../../Food_tree_eg/NodeLabelsMCT.txt", 
#                addl_foods_fn = NULL,
#                num.levels = 3,
#                food_database_fn =            "Food_D12_FC_QC_demo_QCed_Leg_morethanone_red.txt",  
#                output_tree_fn =     "Foodtree/Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_3Lv.nwk", 
#                output_taxonomy_fn = "Foodtree/Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_3Lv.tax.txt"
#   )
  
# # --------------------------------------------------------------------------------------------------------------
# # Generate OTU tables for downstream analyses; IT MAY TAKE SOME TIME.
# # It is OK to see the following warning message:
# # In write.table(fiber.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
# # appending column names to file.
#   
# # Make the standard food otu table with data in gram weights of food.
# # For the food_records_fn argument, you need to supply 'sel.food.records' file that have 'FoodAmt' column.     
#   MakeFoodOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_Leg_morethanone.txt",   
#               food_record_id =  "SEQN",                              # The ID of your participants
#               food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_3Lv.tax.txt",       # Your taxonomy file produced by MakeFoodTree.
#               output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_3Lv.food.otu.txt")  # Output otu file to be saved.
#   
# # Make a food otu table with data in grams of fiber per food.
#   MakeFiberOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_Leg_morethanone.txt", 
#                food_record_id=   "SEQN", 
#                food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_3Lv.tax.txt", 
#                output_fn=        "Foodtree/Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_3Lv.fiber.otu.txt")
#   
# # Make a food otu table as dehydrated grams per kcal.
#   MakeDhydrtOtu(food_records_fn=  "Food_D12_FC_QC_demo_QCed_Leg_morethanone.txt", 
#                 food_record_id =  "SEQN", 
#                 food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_3Lv.tax.txt", 
#                 output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_Leg_morethanone_red_3Lv.dhydrt.otu.txt")  
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Come back to the main directory.
#   setwd(main_wd)
#   
#   
#   
#   