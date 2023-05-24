# ===============================================================================================================
# Calculate foods/nutrients consumption by DivGroups.
# Version 1
# Created on 05/24/2023 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================

# ===============================================================================================================

  library(ggplot2)

# Set your working directory to the main directory.
# Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist2")  

# ---------------------------------------------------------------------------------------------------------------

# Load the data.
  totals_c_wa <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3676_DivGroup.txt")
  
  dim(totals_c_wa)
  # should be 3676 rows, after removing rows containing missing data.
  
# Ensure there is no missing data.
  naniar::vis_miss(totals_c_wa[, c("SEQN","BMXWAIST","BMXBMI","RIDAGEYR", "RIAGENDR", "RIDRETH3", "INDFMPIR", 
                                   "DMDEDUC3", "DMDEDUC2", "KCAL")])
  
# Make the DivGroup as a factor.
  totals_c_wa$DivGroup <- factor(totals_c_wa$DivGroup, 
                                 levels = c('DivNA', 'Div0', 'Div1', 'Div2'))
# Make Gender as a factor.
  table(totals_c_wa$Gender)
  # totals_c_wa$Gender <- factor(totals_c_wa$Gender, 
  # levels = c('F', 'M'))
  
# Let's look at the Groups
  table(totals_c_wa$DivGroup, useNA="ifany")
  
  # DivNA  Div0  Div1  Div2 
  #  1840  1114   361   361 
  
# Distribution of each group
  df <- totals_c_wa
  
# ===============================================================================================================
# Add nuts/seeds/legumes amount. 
# ===============================================================================================================
  
  colsum_s_2 <- read.delim("n3676_SEQN_4xxxxxx_amt.txt")
  
# Make SEQN column.
  colsum_s_2$SEQN <- substring(colsum_s_2$XSEQN, 2) # start from the 2nd letter. 
  head(colsum_s_2)
  
# Add amt-ave.   ### This overwrites df ###
  df <- merge(df, colsum_s_2[c("SEQN", "amt_ave")], by="SEQN", all.x=T)
  head(df,1)
  summary(df$amt_ave) # amt_ave added, but has NAs.!
  
# Replace "NA" with zero for the DivMA group.
  # # Safety check
  # vector <-  df$amt_ave
  # vector[is.na(vector)] <- 0
  # vector
  # data.frame(vec=head(vector, 20), amt=head(df$amt_ave, 20))
  
  df$amt_ave[is.na(df$amt_ave)] <- 0  ### THis overwrites df ###
  summary(df$amt_ave) 

# Check that DivNA individuals have zero 'amt_ave'. Good.  
  df[1:15, c("DivGroup", "amt_ave")]  
  
# ===============================================================================================================
# Calculate PF_TOTAL_LEG, PF_LEG_perTOTAL, and total_MPFAT
# ===============================================================================================================
  
# PF_TOTAL does not include PF_LEGUME, so add them to make PF_TOTAL_LEG.
  df$PF_TOTAL_LEG <- df$PF_TOTAL + df$PF_LEGUMES
  summary(df$PF_TOTAL_LEG)
  
# Calculate the percentage of PF_LEGUME in PF_TOTAL_LEG.
# (i.e. the percentage of legumes in all the protein food intake)
  df$PF_LEG_perTOTAL <- df$PF_LEGUMES / df$PF_TOTAL_LEG *100
  summary(df$PF_LEG_perTOTAL)
  head(df[order(df$PF_TOTAL_LEG), c("PF_TOTAL_LEG", "PF_LEG_perTOTAL")])
  # The person who had zero protein intake had NaN PF_LEG_perTOTAL. 
    
# Calculate total polyunsaturate fat intake.
  df$total_MPFAT <- df$MFAT + df$PFAT
  summary(df$total_MPFAT)
  head(df$DivGroup,10)
  df %>% group_by(DivGroup) %>% summarise(means= mean(total_MPFAT))
    
# Save the totals with DivGroup, amt, and PF_LEG_perTOTAL etc.
  write.table(df, 'Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3676_DivGroup_amt_var.txt',
              sep="\t", row.names = F, quote=F)
  
  
# ===============================================================================================================
# Generate a density plot of a variable.
# ===============================================================================================================
# Missing data? -- No.  
  summary(df$KCAL)
  summary(df$BMXBMI)
  
# Create a density plot of KCAL by DivGroup.
  dens <- ggplot(data=df, aes(x=, group=DivGroup, fill=DivGroup)) +
    # geom_boxplot( outlier.shape=16, outlier.alpha=0.5 ) + space_axes + no_grid +
    geom_density(adjust=1, alpha=0.4  ) + space_axes + no_grid +
    # scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
    labs(y="Alcohol (No. of drinks)", x=NULL)  
  dens

  

# ===============================================================================================================
# Calculate the means, SD, and p-values for anova between DivGroups without adjusting by KCAL.
# ===============================================================================================================
# Define variaables.
  # Copy from Excel column.
  variablesdf <- read.table('clipboard', sep="\t", header=F)
  variables <- variablesdf$V1
  # Or type.
  variables <- c("PF_LEGUMES","PF_TOTAL_LEG","PF_LEG_perTOTAL", "F_TOTAL")
  # df$

# make a dataframe to save means, SD, and p-values for ANOVA.
  rtable <- data.frame(matrix(nrow= length(variables), ncol=10))
  colnames(rtable) <- c('var', 'm_DivNA',	'm_Div0',	'm_Div1',	'm_Div2',
                        'sd_DivNA',	'sd_Div0',	'sd_Div1',	'sd_Div2',  'p_val') 
  rtable
 
# Get p-values of ANOVA for each element in the 'variables' vector.
  for(i in 1:length(variables)){
    print( paste("n=", i))
    
    divn <- which(colnames(df) == "DivGroup")
    
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
    
    rtable[i, 1] <- variables[i]
    rtable[i, 10] <- pval
    
    # Calculate means for each DivGroup
    meantabl <- subset %>% group_by(DivGroup) %>% summarise(means=mean(i_variable))
    rtable[i, 2] <- meantabl[1,2] # DivNA 
    rtable[i, 3] <- meantabl[2,2] # Div0
    rtable[i, 4] <- meantabl[3,2] # Div1
    rtable[i, 5] <- meantabl[4,2] # Div2
    
    # Calculate SD for each DivGroup
    sdtabl <-   subset %>% group_by(DivGroup) %>% summarise(sds=sd(i_variable))
    rtable[i, 6] <- sdtabl[1,2] # DivNA 
    rtable[i, 7] <- sdtabl[2,2] # Div0
    rtable[i, 8] <- sdtabl[3,2] # Div1
    rtable[i, 9] <- sdtabl[4,2] # Div2
    
  }
  
  unadjusted <- rtable
  unadjusted
  mymeans <- df %>% group_by(DivGroup) %>% summarise(means = mean(TFAT))
  mymeans
  mysd <- df %>% group_by(DivGroup) %>% summarise(sds = sd(TFAT))
  mysd
  write.table(unadjusted, 'clipboard', sep="\t", row.names = T, quote=F, col.names = NA)
  
# ---------------------------------------------------------------------------------------------------------------
# Need to test the difference in amt_ave between Div0, Div1, and Div2, excluding DivNA.
  subset012 <- subset(df, DivGroup != "DivNA")
  table(df$DivGroup)
  table(subset012$DivGroup) # OK
  
  myanova <- aov(amt_ave ~ DivGroup, data= subset012)
  ss <- summary(myanova)
  # Take out the p-value.
  as.data.frame(ss[[1]])[1, 5]
  
# ===============================================================================================================
# Calculate the means, SD, and p-values for anova between DivGroups WITH adjusting by KCAL.
# ===============================================================================================================
  
# eg. adjustment by kcal - protian intake per 2000 kcal.
  df$PROT_adj <- df$PROT / df$KCAL *2000
  df[1:10, c("PROT", "PROT_adj", "KCAL")]
  df %>% group_by(DivGroup) %>% summarise(means= mean(PROT_adj))
  df %>% group_by(DivGroup) %>% summarise(sds= sd(PROT_adj))
  
# Define variaables.
  # Copy from Excel column.
  variablesdf <- read.table('clipboard', sep="\t", header=F)
  variables <- variablesdf$V1
  # Or type.
  variables <- c("PF_LEGUMES","PF_TOTAL_LEG","PF_LEG_perTOTAL", "F_TOTAL")
  # df$
  
  # make a dataframe to save means, SD, and p-values for ANOVA.
  rtable <- data.frame(matrix(nrow= length(variables), ncol=10))
  colnames(rtable) <- c('var', 'm_DivNA',	'm_Div0',	'm_Div1',	'm_Div2',
                        'sd_DivNA',	'sd_Div0',	'sd_Div1',	'sd_Div2',  'p_val') 
  rtable 
  
  # Get p-values of ANOVA for each element in the 'variables' vector.
  for(i in 1:length(variables)){
    print( paste("n=", i))
    
    divn <- which(colnames(df) == "DivGroup")
    
    # Look for the column number for the ith variable
    coln <- which(colnames(df) == variables[i])
    print(coln)
    # # subset SEQN and the ith variable.
    # subset <- df[, c(divn, coln)]
    # # # Standardize the column names.
    # colnames(subset) <- c("DivGroup", "i_variable")
    
    ### ADJUST BY KCAL ##############
    # Look for the column number for kcal
    kcaln <- which(colnames(df) == "KCAL")
    subset <- df[, c(divn, kcaln, coln)]
    colnames(subset) <- c("DivGroup", "KCAL", "i_variable")
    
    # Overwrite the i_variable with adjusted i_variable.
    subset$i_variable <- subset$i_variable / subset$KCAL * 2000
    ##################################
    
    # run anova.
    myanova <- aov(i_variable ~ DivGroup, data=subset)
    ss <- summary(myanova)
    # Take out the p-value.
    pval <- as.data.frame(ss[[1]])[1,5]
    
    rtable[i, 1] <- variables[i]
    rtable[i, 10] <- pval
    
    # Calculate means for each DivGroup
    meantabl <- subset %>% group_by(DivGroup) %>% summarise(means=mean(i_variable))
    rtable[i, 2] <- meantabl[1,2] # DivNA 
    rtable[i, 3] <- meantabl[2,2] # Div0
    rtable[i, 4] <- meantabl[3,2] # Div1
    rtable[i, 5] <- meantabl[4,2] # Div2
    
    # Calculate SD for each DivGroup
    sdtabl <-   subset %>% group_by(DivGroup) %>% summarise(sds=sd(i_variable))
    rtable[i, 6] <- sdtabl[1,2] # DivNA 
    rtable[i, 7] <- sdtabl[2,2] # Div0
    rtable[i, 8] <- sdtabl[3,2] # Div1
    rtable[i, 9] <- sdtabl[4,2] # Div2
    
  }
  
  adjusted <- rtable
  write.table(adjusted, 'clipboard', sep="\t", row.names = T, quote=F, col.names = NA)

  adjusted
  # Safety check with PROT  
  df$PROT_adj <- df$PROT / df$KCAL *2000
  # df[1:10, c("PROT", "PROT_adj", "KCAL")]
  df %>% group_by(DivGroup) %>% summarise(means= mean(PROT_adj))
  df %>% group_by(DivGroup) %>% summarise(sds= sd(PROT_adj))
  
  # Safety check with TFAT  
  df$TFAT_adj <- df$TFAT / df$KCAL *2000
  df[1:10, c("TFAT", "TFAT_adj", "KCAL")]
  df %>% group_by(DivGroup) %>% summarise(means= mean(TFAT_adj))
  df %>% group_by(DivGroup) %>% summarise(sds= sd(TFAT_adj))
  
  