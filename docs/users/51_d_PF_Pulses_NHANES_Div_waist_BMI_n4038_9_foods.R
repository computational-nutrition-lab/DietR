# ===============================================================================================================
# Make food/nutrient intake table for each of the DivGroups.
# Version 1
# Created on 05/08/2023 by Rie Sadohara
# ===============================================================================================================

  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  library(SASxport)
  library(dplyr)
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
# source("lib/data_overview.R") 

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist/")  

# ===============================================================================================================
# Load the prepared data with the DivGroup variable.
# ===============================================================================================================

# Load the data saved in . 
  totals_c_wa <- read.delim("../Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup_waistBMI.txt")

  dim(totals_c_wa)
# should be 4038 rows, after removing rows containing missing data.

# Ensure there is no missing data.
  naniar::vis_miss(totals_c_wa[, c("SEQN","BMXWAIST","BMXBMI","FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL", 
                                   "Gender", "RIDAGEYR")])

# Make the DivGroup as a factor.
  totals_c_wa$DivGroup <- factor(totals_c_wa$DivGroup, levels=c('DivNA', 'Div0', 'Div1', 'Div2'))

# Make Gender as a factor.
  table(totals_c_wa$Gender)
# totals_c_wa$Gender <- factor(totals_c_wa$Gender, 
# levels = c('F', 'M'))

# Let's look at the Groups
  table(totals_c_wa$DivGroup, useNA = "ifany")

# DivNA  Div0  Div1  Div2 
# 2012  1246   387   393 

# Distribution of each group
  df <- totals_c_wa

# ---------------------------------------------------------------------------------------------------------------
  
# ===============================================================================================================
# KCAL intake
# ===============================================================================================================
# Missing data? -- No.  
  summary(df$KCAL)
  summary(df$BMXBMI)

# Create a density plot of KCAL by DivGroup.
  box <- ggplot(data=df, aes(x=DivGroup, y=A_DRINKS, fill=DivGroup)) +
    geom_boxplot( outlier.shape=16, outlier.alpha=0.5 ) + space_axes + no_grid +
    # scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
    labs(y="Alcohol (No. of drinks)", x=NULL)  
  box
  df$PF_TOTAL_LEG
  means <-  df[1:10, c('PF_TOTAL', 'PF_LEGUMES', 'PF_TOTAL_LEG', 'PF_LEG_perTOTAL')]
  write.table(means, "clipboard", sep="\t", quote=F, row.names = F)
  
  df$total_MPFAT <- df$MFAT + df$PFAT
  
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(KCAL))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(PF_TOTAL))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(PF_LEGUMES))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(PF_TOTAL_LEG))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(PF_LEG_perTOTAL))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(CARB))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(FIBE))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(PROT))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(TFAT))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(SFAT)) # total saturated FA
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(MFAT)) # total monounsaturated FA
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(PFAT)) # total polyunsaturated FA
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(total_MPFAT)) # total unsaturated FA
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(NoOfItems))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(V_TOTAL))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(FOLA))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(ALCO))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(ADD_SUGARS))
  means <- df %>% group_by(DivGroup) %>% summarise(means= mean(F_TOTAL)) # total fruit
  plot(df$V_LEGUMES, df$PF_LEGUMES)
  
  write.table(t(means), "clipboard", sep="\t", quote=F, row.names = F)
  #

# ---------------------------------------------------------------------------------------------------------------
# Make a funciton to test p-values of anova for all the variables specified.
  
# Copy from Excel column.
  variablesdf <- read.table('clipboard', sep="\t", header=F)
  variables <- variablesdf$V1
# Or type.
  variables <- c("PF_TOTAL","PF_LEGUMES","PF_TOTAL_LEG","PF_LEG_perTOTAL", "F_TOTAL")
  variables
  # column number of DivGroup
  # divn <- which(colnames(df)=="DivGroup")
  # ith <- which(colnames(df)=="SFAT")
  # subset <- df[, c(divn, ith)]
  # head(subset)
  
# make a dataframe to save p-values for ANOVA.
  ptable <- data.frame(matrix(nrow=length(variables), ncol=2))
  colnames(ptable) <- c('var', 'p_val') 
  ptable
  
# Get p-values of ANOVA for each element in the 'variables' vector.
  for(i in 1:length(variables)){
    print( paste("n=", i))
    # Look for column number for the ith variable
    coln <- which(colnames(df) == variables[i])
    print(coln)
    # subset SEQN and the ith variable.
    subset <- df[, c(divn, coln)]
    # # Standardize the column names.
    colnames(subset) <- c("DivGroup", "i_variable")
    # run anova.
    myanova <- aov(i_variable ~ DivGroup, data=subset)
    ss <- summary(myanova)
    # Take out the p-value.
    pval <- as.data.frame(ss[[1]])[1,5]
    
    ptable[i, 1] <- variables[i]
    ptable[i, 2] <- pval
  }
  
  ptable
  write.table(ptable[,2], 'clipboard', sep="\t", row.names = F, quote=F)
  
  
  
    
# For my information ---------------------------------------------------------------------------------------------------------------
# Nuts/seeds/legumes (4xxxxxxx) consumption

  foods <- read.delim("../Food_D12_FC_QC_demo_QCed_4s.txt")
  dim(foods) # 3963 x 177 
  head(colnames(foods)) # X.FOODID, X87496, X88725, ...
  tail(colnames(foods)) # X92316, ..., taxonomy.
  head(foods)
  foods[1:4, 1:4]
  head(foods$SEQN)
  library(dplyr)
  plot(foods$Day)
  foods %>% filter(SEQN == 83734) %>% group_by(Day) %>% summarise(sum = sum(FoodAmt))
  foods %>% filter(SEQN == 83752) %>% group_by(Day) %>% summarise(sum = sum(FoodAmt))
  foods %>% filter(SEQN == 83767) %>% group_by(Day) %>% summarise(sum = sum(FoodAmt))
  foods %>% filter(SEQN == 90993) %>% group_by(Day) %>% summarise(sum = sum(FoodAmt))
  83767 has 2 days.
  # So, OTU is calculated with day 1 AND day 2 data, combined.
  # i.e. DivGroup is based on the 4xxxxxxxx foods consumed in the 2 days.
  # I should divide the amount by 2 to get 2-day average of nuts/seeds/legumes consumption amount.

# ---------------------------------------------------------------------------------------------------------------
# colSums of the OTU table to get the total consumption amount for each SEQN.  
  
  otu <- read.delim("~/GitHub/DietR/eg_data/NHANES/Div/Foodtree/Food_D12_FC_QC_demo_QCed_4s_3Lv.food.otu.txt")
  dim(otu) # 243 x 2110 
  
  otu[, c('X83767','X.FOODID')]  # 11 + 2.92 = 13.92 g. Has both Day 1 and Day 2. 
  otu[, c('X90993','X.FOODID')]  # 1428.75 + 47.50 = 1476.25 g. 
  
# Calculate the sum of 4xxxxxxx foods for each SEQN. This is the sum of two days.
  head(colnames(otu)) # X.FOODID, X87496, X88725, ...
  tail(colnames(otu)) # X92316, ..., taxonomy.
  otu[1:4, 1:4]
  colsum <- colSums(otu[, 2: (ncol(otu)-1) ] , na.rm=T) # exclude the food description and taxonomy columns.
  head(colsum) # This is a named vector.
  colsumdf <- data.frame(SEQN=names(colsum), amt = colsum)
  row.names(colsumdf) <- NULL
  head(colsumdf)
  colsumdf %>% filter(SEQN=="X83767") # 13.92. Correct!
  colsumdf %>% filter(SEQN=="X90993") # 1476.25. Correct! But this person seems to be an outlier...
  
# divide the amount by 2 to get the average/day for each SEQN.
  colsum_s <- colsumdf[order(colsumdf$amt, decreasing=T), ]
  colsum_s$amt_ave <- colsum_s$amt/2 # divide by 2 and it will be average amount (g)/ day.
  head(colsum_s)
  colnames(colsum_s)[1] <- "XSEQN" # Change SEQN to XSEQN.
  
  colsum_s %>% filter(amt==0) %>% nrow() # 2108 (all of the people) have some consumption, OK!
  colsum_s %>% filter(XSEQN=="X90993")
  colsum_s %>% filter(amt== max(colsum_s$amt)) # The max is 1525 g/day.. by X92254.
  # What did X92254 report eating?
  otu[, c('X92254','X.FOODID')]   
  # 3004.38 g Bean soup with macaroni home recipe canned or ready to serve
  # 47.25 g of Almonds unsalted
  # Wow... 
  # Was it consumed on both days or just on 1 day?
  foods %>% filter(SEQN=="92254")
  # 35 g salted almonds only on day 1, and 12 g of salted almonds and 1502 g of bean soup for lunch and 
  # exactly the same 1502 g of bean soup for dinner on day 2.
  # Even though it's soup, 1.5 kg for 1 meal is extreme. I will exclude this..
  
# Filter out "X92254" who ate 3 kg of nuts/seeds/legumes over 2 days.
  colsum_s_2 <- colsum_s %>% filter(amt_ave < 1000) 
  max(colsum_s_2$amt_ave)
  # So, the max amount is 952 g per day. 
  
  # Add DivGroup.
  colsum_s_2_div <- merge(x=colsum_s_2, y=div, all.x=T, by="XSEQN")
  table(colsum_s_2_div$DivGroup)
  
  # Calc the means of nuts/seeds/legume consumption by DivGroup.
  nlsmeans <- colsum_s_2_div %>% group_by(DivGroup) %>% summarise(nsl= mean(amt_ave))
  nlsmeans
  write.table(t(nlsmeans), "clipboard", sep="\t", row.names = F, quote=F)
  # This is the average of 2 days for each DivGroup. 
  
  box <- ggplot(data=colsum_s_2_div, aes(x=DivGroup, y= amt_ave, fill=DivGroup)) +
    geom_boxplot( outlier.shape=16, outlier.alpha=0.5  ) + space_axes + no_grid +
    # scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
    labs(y="Nuts/seeds/legumes (g/day)", x=NULL)
  box
  ggsave("Waist/nuts_seeds_legumes_amt_ave_DivGroup_1000gd_or_less.png", box,
         device="png", width=5.2, height=4.2, units="in")
  
    
###########  WITHOUT taking out the outlier with >1500 g/day. ####################
# Load SEQN & DivGroup assignment, and merge them.
  totaldiv <- read.delim("~/GitHub/DietR/eg_data/NHANES/PF/Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup.txt")
  head(totaldiv)

  div <- totaldiv[, c('SEQN', 'DivGroup')]  
  div$XSEQN <- paste("X", div$SEQN, sep="")
  head(div)
  table(div$DivGroup)
  colsum_s_div <- merge(x=colsum_s, y=div, all.x=T, by="XSEQN")
  colsum_s_div[10:30,]
  
# Calc the means of nuts/seeds/legume consumption by DivGroup.
  nlsmeans <- colsum_s_div %>% group_by(DivGroup) %>% summarise(nsl= mean(amt_ave))
  nlsmeans
  write.table(t(nlsmeans), "clipboard", sep="\t", row.names = F, quote=F)
# This is the average of 2 days for each DivGroup. 
  
  box <- ggplot(data=colsum_s_div, aes(x=DivGroup, y= amt_ave, fill=DivGroup)) +
    geom_boxplot( outlier.shape=16, outlier.alpha=0.5  ) + space_axes + no_grid +
    # scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
    labs(y="Nuts/seeds/legumes (g/day)", x=NULL)  
  box
  ggsave("Waist/nuts_seeds_legumes_amt_ave_DivGroup.png", box,
         device="png", width=5.2, height=4.2, units="in")
  
# Div1 seems to have an outlier.
  summary(colsum_s_div$amt_ave)
  plot(colsum_s_div$amt_ave)
  boxplot(colsum_s_div$amt_ave)
  head(colsum_s_div[ order(colsum_s_div$amt_ave, decreasing = T), ])
  
  
  
  
# ---------------------------------------------------------------------------------------------------------------
# ANOVA with amt_ave and DivGroup.
  myanova <- aov(amt_ave ~ DivGroup, data= colsum_s_div)
  summary(myanova)
# p-value
  summary(myanova)[[1]][1,5]
  
  

# ---------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------
  BMXWT (body weight)

# ANOVA
  myanova <- aov(BMXBMI ~ DivGroup, data=df)
  summary(myanova)
  
  source("~/GitHub/R_Toolbox/anova_assumption_plots.R")
  ANOVA_assumption(input.anova = myanova, input.factor = "DivGroup", df=df)
  
  # Levene's test OK, but normality...? 
  shapiro.test(df$BMXBMI)
  
  hist(df$BMXBMI)
  
  pairwise.t.test(df$BMXBMI, df$DivGroup, p.adjust.method = "holm") 
  
  
  
  # log 
  
