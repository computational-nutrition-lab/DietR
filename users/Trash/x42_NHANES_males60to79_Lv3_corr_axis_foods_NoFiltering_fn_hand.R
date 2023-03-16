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
  

# By hand  
# ===============================================================================================================
# Load ordination data
# ===============================================================================================================

# Run this code below after making your phyloseq object and doing ordination. 

# Load the food file again that was used to build the phyfoods object. 
  food1 <- read.delim('../Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.food.otu_sortedbysample.txt')   
  
# SEQN-sorted food
  # dim(food1) # 58 foods x {703 people + 1 taxonomy columns}
  dim(food1) # 1678 foods x {237 people + 1 taxonomy columns}
  food1[1:3, 1:3]
  
# samples (individuals) needs to be the rownames. So, transform it.
# Remove the taxonomy column from 'food'.
  food2 <- food1[, !colnames(food1) == "taxonomy"] 
  colnames(food2)
  dim(food2) # 1678 x 237

# Transpose food so that rows will be the SEQN (for which a distance matrix is calculated)
  food3 <- as.data.frame(t(food2))
  head(colnames(food3))    # columns have foods
  head(rownames(food3))    # rows have individuals
  dim(food3) 

# Sort individuals (rows) in order. **IMPORTANT!** 
  food3_s <- food3[order(rownames(food3)), ]
  head(rownames(food3_s), 10)       # individuals should be in order.
  dim(food3_s) # 237 people x 1678 foods
# food3_s is x. 

# Load the SEQN and Axis values.
  loaded_leg_w <- read.table("Food_D12_FC_QC_demo_QCed_males60to79_3Lv_ord_WEIGHTED_meta_users.txt",
                             sep="\t", header=T)  

  # loaded_leg_u <- read.table("Food_D12_FC_QC_demo_QCed_males60to79_3Lv_ord_UNweighted_meta_users.txt",
  #                            sep="\t", header=T)  
  
# loaded_leg_u has vectors (Axis values) of each SEQN.
  head(loaded_leg_w) 
  head(loaded_leg_u) 
    # Add rownames: X83732 etc. This will stay even after selecting only Axis. columns. 
    # rownames(loaded_leg_u) <- paste("X", loaded_leg_u$SEQN, sep="")
     rownames(loaded_leg_w) <- loaded_leg_w$Row.names
     rownames(loaded_leg_u) <- loaded_leg_u$Row.names
    
    # pick up only columns whose names start with "Axis.".
    loaded_leg_w_Axisonly <- SubsetByFirstChaInCol(input.df = loaded_leg_w, starting.str = "Axis.")
    loaded_leg_u_Axisonly <- SubsetByFirstChaInCol(input.df = loaded_leg_u, starting.str = "Axis.")
    # Only the Axis values and the rownames (SEQN) have been selected.
    head(loaded_leg_w_Axisonly,1)
    head(loaded_leg_u_Axisonly,1)
    
# ---------------------------------------------------------------------------------------------------------------
# Generate correlation matrices.

# correlate them with each other
# x <- as.data.frame(t(foodgroups_s))
  x <- as.data.frame(food3_s)
  dim(x)
  min(rowSums(x)) # there shouldn't be zero, as only those who consumed legume-containing foods are selected.
  min(colSums(x)) # there shouldn't be zero, as only foods that were consumed at least some were selected.
  write.table(as.data.frame(colSums(x)), 
              "Food_D12_FC_QC_demo_QCed_males60to79_3Lv_AmountSums.txt", 
              sep="\t", row.names = T, quote=F )
  x[1:3, 1:3]

# y <- as.data.frame(ffq)  # food group values.
  # y <- as.data.frame(ordinated_w_vec)  # food group values.
  # y <- as.data.frame(ordinated_u_vec)  # food group values.
  y <- as.data.frame(loaded_leg_w_Axisonly)  # food group values.
  y <- as.data.frame(loaded_leg_u_Axisonly)  # food group values.
  dim(y)
  y[1:3, 1:3]

# make sure the samples are the same.
# x <- x[rownames(x) %in% rownames(y), ]
# y <- y[rownames(y) %in% rownames(x), ]
  identical(rownames(x), rownames(y))
  
# Now test correlation of each of the columns in x with columns in y. This will take several minutes.
# The variables (food items) that have been tested will be printed out in the console.
  
  w_dat <- create_corr_frame(x, y)
  dat <- w_dat
  
  u_dat <- create_corr_frame(x, y)
  dat <- u_dat
  
# Change column names of x and y to more meaningful ones.
  # colnames(dat)[1:2]<-c("Food","FFQ")
  colnames(dat)[1:2] <- c("Food","Axis")

# Mark rows that have qvalues < 0.25 with an asterisk in a column called "Significance".  
  dat$Significance <- cut(dat$qval, breaks=c(-Inf, 0.05, Inf), label=c("*", "")) 
  head(dat)
  table(dat$Significance)
  dat[order(dat$pval), ][1:40, ]
  
  # Careful which one to save!
    # WEIGHTED
    write.table(dat, "Food_D12_FC_QC_demo_QCed_males60to79_3Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt",
                sep="\t", row.names=F, quote=F)  
  
    # UNweighted
    write.table(dat, "Food_D12_FC_QC_demo_QCed_males60to79_3Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt",
                sep="\t", row.names=F, quote=F)  
  
# See each axes 
# Select Axis 1 rows
  dat_1 <- subset(dat, Axis=="Axis.1")
  head(dat_1[order(dat_1$qval), ], 10)
  head(dat_1[order(dat_1$qval), ], 20)
  
# Select Axis 2 rows and sort by qval.  
  dat_2 <- subset(dat, Axis=="Axis.2")
  head(dat_2[order(dat_2$qval), ], 20)
  
# Select Axis 3 rows and sort by qval.  
  dat_3 <- subset(dat, Axis=="Axis.3")
  head(dat_3[order(dat_3$qval), ], 4)
  
# Select Axis 4 rows and sort by qval.  
  dat_4 <- subset(dat, Axis=="Axis.4")
  head(dat_4[order(dat_4$qval), ], 6)
  
# Select significant food items.
  dat_sig <- subset(dat, Significance == "*")
  
  head(dat_sig[order(dat_sig$qval), ], 15)
  
# weighted unifrac distances are heavily influenced by liquid intake...
# Look at unweighted unifrac distances.
 
# WEIGHTED
  write.table(dat_sig[order(dat_sig$qval), ], 
              "Food_D12_FC_QC_demo_QCed_Leg_morethanone_prep_red_3Lv_ord_WEIGHTED_corr_axes_foods_sig.txt",
              sep="\t", row.names=F, quote=F)  
  
# UNweighted
  write.table(dat_sig[order(dat_sig$qval), ], 
              "Food_D12_FC_QC_demo_QCed_Leg_morethanone_prep_red_3Lv_ord_UNweighted_corr_axes_foods_sig.txt",
              sep="\t", row.names=F, quote=F)  
  
