# ===============================================================================================================
# Version 1
# Created on 02/16/2023 by Rie Sadohara
# ===============================================================================================================

# ---------------------------------------------------------------------------------------------------------------
# From sorted food OTU table, generate a table of total amount of food consumed by all the individuals,
# and a table with correlation coefficients, p-values, and q-values with desired threshold between
# food items and Axes that were saved in the ordination section.
# Be careful about not to confuse WEIGHTED and UNweighted unifrac distances

# This function uses SubsetByFirstChaInCol and create_corr_frame functions. Load them in advance.
  source("lib/SubsetByFirstChaInCol.R") 
  source("lib/create_corr_frame.R") 

CorrAxesFood <- function(food.otu_soted,
                         AmountSums.out.fn,
                         meta.users,
                         qval.threshold = 0.05,
                         corr.axes.foods.outfn  ){
  
  # Read in the food.otu_soted.
  food1 <- read.delim(food.otu_soted)
  
  # remove "taxonomy" column at the end of food1.
  food2 <- food1[, !colnames(food1) == "taxonomy"] 
  
  # transpose food2 so that rows will be the individuals (for which a distance matrix is calculated)     
  food3 <- as.data.frame(t(food2))
  
  # sort individuals (rows) in order.
  food3_s <- food3[order(rownames(food3)), ]
  
  x <- food3_s
  
  # Save the total amount consumed by all the individuals. 
  write.table(x=as.data.frame(colSums(x)), 
              file = AmountSums.out.fn, sep="\t", row.names=T, quote=F )
  
  # Load the meta.users, which has userID and Axis values.
  loaded_leg <- read.table(meta.users, sep="\t", header=T)  
  
  # Add rownames: X83732 etc. This will stay even after selecting only Axis. columns. 
  # rownames(loaded_leg_u) <- paste("X", loaded_leg_u$SEQN, sep="")
  rownames(loaded_leg) <- loaded_leg$Row.names
  
  # pick up only columns whose names start with "Axis.".
  loaded_leg_Axisonly <- SubsetByFirstChaInCol(input.df = loaded_leg, starting.str = "Axis.")
  
  y <- as.data.frame(loaded_leg_Axisonly)  # food group values.
  
  # make sure the samples are the same.
  if( identical(rownames(x), rownames(y)) == F){
    
    return("The columnnames of X and Y are different. Ensure your food.otu_soted and\n 
                       meta.users have the same set of individuals.")
    
  }else{
    
    # Now test correlation of each of the columns in x with columns in y. This will take several minutes.
    # The variables (food items) that have been tested will be printed out in the console.
    dat <- create_corr_frame(x, y)
    
    # Change column names of x and y to more meaningful ones.
    colnames(dat)[1:2] <- c("Food","Axis")
    
    # Mark rows that have qvalues < 0.25 with an asterisk in a column called "Significance".
    dat$Significance <- cut(dat$qval, breaks=c(-Inf, qval.threshold, Inf), label=c("*", ""))
    
    # Sort dat by qval (small to large). 
    dat_s <- dat[order(dat$qval), ]
    
    # Save.
    write.table(x=dat_s, file = corr.axes.foods.outfn, sep="\t", row.names=F, quote=F)
    
  } 
  
}

# ---------------------------------------------------------------------------------------------------------------
# test
#   SpecifyDataDirectory("eg_data/VVKAJ/Ordination/")
#   
# # From sorted food OTU table, generate a table of total amount of food consumed by all the individuals, 
# # and a table with correlation coefficients, p-values, and q-values with desired threshold between 
# # food items and Axes that were saved in the ordination section. 
# # Be careful about not to confuse WEIGHTED and UNweighted unifrac distances   
#   CorrAxesFood(food.otu_soted = "../Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.otu_sorted.txt", 
#                AmountSums.out.fn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_AmountSums.txt",
#                qval.threshold = 0.05,
#                meta.users =            "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt",
#                corr.axes.foods.outfn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")
# 
#   # food.otu_soted:     xxx.food.otu.sorted.txt file, saved in the ordination section.
#   # AmountSums.out.fn:  output filename to be saved which has the total consunption amount of each food.
#   # qval.threshold:     q-value threshold to call a correlation significant.
#   # meta.users:         xxx.meta_users.txt file, waved in the ordination section.
#   # corr.axes.foods.outfn: output filename to be saved which has the correlation between foods and Axees.
# 

  

  
