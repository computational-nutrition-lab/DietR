# ===============================================================================================================
# Waist 2.
# Calculate nuts/seeds/legumes consumption amount, and remove outliers.
# Version 1
# Created on 05/19/2023 by Rie Sadohara
# ===============================================================================================================
  source("lib/specify_data_dir.R")

# Set your working directory to the main directory.
# Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANESPF/Waist2")  


# ===============================================================================================================
# Load the OTU table and compute the nuts/seeds/legumes conumption.
# ===============================================================================================================

  # colSums of the OTU table to get the total consumption amount for each SEQN.  
  
  otu <- read.delim("~/GitHub/DietR/eg_data/NHANES/PF/Waist2/Foodtree/Food_D12_FC_QC_demo_QCed_n3677_4s_3Lv.food.otu.txt")
  dim(otu) # 237 x 1839 
  
  otu[, c('X83767','X.FOODID')]  # 11 + 2.92 = 13.92 g. Has both Day 1 and Day 2. 
  otu[, c('X92254','X.FOODID')]  # 3004.38  + 47.25 = 3051.63 g. 
  
  # Calculate the sum of 4xxxxxxx foods for each SEQN. This is the sum of two days.
  head(colnames(otu)) # X.FOODID, X87496, X88725, ...
  tail(colnames(otu)) # X92316, ..., taxonomy.
  otu[1:4, 1:4]
  
# exclude the food description and taxonomy columns.
  colsum <- colSums(otu[, 2: (ncol(otu)-1) ] , na.rm=T) 
  head(colsum) # This is a named vector.
  
# Create a dataframe with SEQN and amount (2 days).
  colsumdf <- data.frame(SEQN=names(colsum), amt = colsum)
  row.names(colsumdf) <- NULL
  head(colsumdf)
  
# Safety check.
  colsumdf %>% filter(SEQN=="X83767") # 13.92. Correct!
  colsumdf %>% filter(SEQN=="X92254") # 3051.63. Correct! But this person seems to be an outlier...
  
# Divide the amount by 2 to get the average/day for each SEQN.
  colsum_s <- colsumdf[order(colsumdf$amt, decreasing=T), ]
  colsum_s$amt_ave <- colsum_s$amt/2 # divide by 2 and it will be average amount (g)/ day.
  head(colsum_s)
  colnames(colsum_s)[1] <- "XSEQN" # Change SEQN to XSEQN.

# Safety check.  
  colsum_s %>% filter(amt==0) %>% nrow() # all of the people have some consumption, OK!
  
# Look for outliers.
  colsum_s %>% filter(amt== max(colsum_s$amt)) # The max is 1525 g/day.. by X92254.

# What did X92254 report eating?
  otu[, c('X92254','X.FOODID')]   
  # 3004.38 g Bean soup with macaroni home recipe canned or ready to serve
  # 47.25 g of Almonds unsalted
  # Wow... 
  # Was it consumed on both days or just on 1 day?
  
# Load food data.
  food <- read.delim("../../Food_D12_FC_QC_demo_QCed.txt", sep= "\t", header=T)

# Take out X92254.  
  subsetted <- subset(food, SEQN=="92254")
  subsetted[ order(subsetted$FoodAmt, decreasing = T),  c("Day", "FoodAmt", "Main.food.description")]
  
  # 35 g salted almonds only on day 1, and 12 g of salted almonds and 1502 g of bean soup for lunch and 
  # exactly the same 1502 g of bean soup for dinner on day 2. hmm...
  # Even though it's soup, 1.5 kg for 1 meal is extreme. I will exclude this..
  
# Filter out "X92254" who ate 3 kg of nuts/seeds/legumes over 2 days.
  colsum_s_2 <- colsum_s %>% filter(amt_ave < 1000) 
  max(colsum_s_2$amt_ave)
  # So, the max amount is 952 g per day.   
  
  hist(colsum_s_2$amt_ave)
  boxplot(colsum_s_2$amt_ave)
  summary(colsum_s_2$amt_ave)
  plot(colsum_s_2$amt_ave)
  head(colsum_s_2)
  
# Looks alright now.
  
# Save colsum_s_2.
  write.table(colsum_s_2, "n3676_SEQN_4xxxxxx_amt.txt", sep="\t", row.names = F, quote=F)
  
  
# Remove  SEQN=="92254" from the OTU table.
  
  # Define the col number. 
  coln <- which(colnames(otu) == "X92254")
  
  # Exclude the column.
  otu2 <- otu[, -coln] 
  
  otu2$X92254 # NULL.
  
# Save the OTU table without the outlier.. n=3676.
  write.table(otu2, "Foodtree/Food_D12_FC_QC_demo_QCed_n3676_4s_3Lv.food.otu.txt",
              sep="\t", row.names = F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# To be consistent, save totals without the same outlier.
  
  totals <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3677.txt")
  totals2 <- filter(totals, SEQN != "92254")
  nrow(totals)    
  # This should have 3,676 people.    
  
  write.table(totals2, "Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3676.txt",
              sep="\t", row.names = F, quote=F)
  
  
