# ===============================================================================================================
# Calculate foods/nutrients consumption by DivGroups.
# Version 1
# Created on 05/25/2023 by Rie Sadohara
# Replaced "n3676" with "n3641" o on 06/28/2023 by Rie Sadohara
# Output as comments were updated. 
# ===============================================================================================================

# ===============================================================================================================
# Load data and packages.
# ===============================================================================================================

  library(ggplot2)
  library(dplyr)

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
# Load data.
  df <- read.delim('Total_D12_FC_QC_mean_QC_demo_ga_body_meta_n3641_DivGroup_DemoCat_Amt_Var.txt')  
  
# DO NOT FORGET TO DEFINE DivGroup as a factor!
  df$DivGroup <- factor(df$DivGroup, 
                        levels = c('DivNA', 'Div0', 'Div1', 'Div2'))
  table(df$DivGroup, useNA = 'ifany')
  
# ===============================================================================================================
# Generate a density plot of a variable.
# ===============================================================================================================
# Missing data? -- No.  
  summary(df$KCAL)
  summary(df$BMXBMI)
  
# Create a density plot of KCAL by DivGroup.
  dens <- ggplot(data=df, aes(x= FIBE, group=DivGroup, fill=DivGroup)) +
    # geom_boxplot( outlier.shape=16, outlier.alpha=0.5 ) + space_axes + no_grid +
    geom_density(adjust=1, alpha=0.4  ) + space_axes + no_grid 
    # scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
    # labs(y="Alcohol (No. of drinks)", x=NULL)
  dens
  
# KCAL - different
  mylm1 <- lm(KCAL ~ DivGroup, data= df)
  summary(mylm1)  # p-value: 4.03e-11
  ggplot(data=df, aes(x=DivGroup, y=KCAL)) +
    geom_boxplot( ) 
  ggplot(data=df, aes(x=DivGroup, y=KCAL)) +
    geom_boxplot( outlier.shape = NA) + geom_jitter()
  
# ALCO were not different among DivGroups.
  mylm2 <- lm(ALCO ~ DivGroup, data= df) 
  summary(mylm2)
  ggplot(data=df, aes(x=DivGroup, y=ALCO)) +
    geom_boxplot( ) 
  ggplot(data=df, aes(x=DivGroup, y=ALCO)) +
    geom_boxplot( outlier.shape = NA) + geom_jitter()
  
# SFAT were not different among the DivGroups.
  mylm3 <- lm(SFAT ~ DivGroup, data= df) 
  summary(mylm3) #  p-value: 0.3245.
  lm3p <- summary(mylm3)
  ggplot(data=df, aes(x=DivGroup, y=SFAT)) +
    geom_boxplot( ) 
  ggplot(data=df, aes(x=DivGroup, y=SFAT)) +
    geom_boxplot( outlier.shape = NA) + geom_jitter()
  
# Define a function to calculate p-value for the F-test again. It's doing an F test with the parameters in the model.
  overall_p <- function(my_model) {
    f <- summary(my_model)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  
  overall_p(mylm1)
  overall_p(mylm2)
  overall_p(mylm3)
    
# ===============================================================================================================
# Calculate the means, SD, and p-values for anova between DivGroups without adjusting by KCAL.
# For those variables that do not need to be adjusted by KCAL.
# ===============================================================================================================
# Define variaables.
  # Copy from Excel column.
  # variablesdf <- read.table('clipboard', sep="\t", header=F)
  # variables <- variablesdf$V1
  # Or type.
  variables <- c("PF_LEGUMES","PF_TOTAL_LEG","PF_LEG_perTOTAL", "F_TOTAL", "V_TOTAL", "amt_ave", "NoOfItems", "KCAL")
  # df$

# make a dataframe to save means, SD, and p-values for ANOVA.
  rtable <- data.frame(matrix(nrow= length(variables), ncol=10))
  colnames(rtable) <- c('var',  'm_DivNA',	'm_Div0',	'm_Div1',	'm_Div2',
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
  

# ---------------------------------------------------------------------------------------------------------------
# Need to test the difference in PF_LEG_perTOTAL between Div0, Div1, and Div2, excluding DivNA, which has NA in PF_LEG_perTOTAL.
  subset012 <- subset(df, DivGroup != "DivNA")
  table(df$DivGroup, useNA = 'ifany')
  table(subset012$DivGroup, useNA='ifany') # OK
  
  myanova <- aov(PF_LEG_perTOTAL ~ DivGroup, data= subset012)
  ss <- summary(myanova)
  # Take out the p-value.
  p_amt_ave <- as.data.frame(ss[[1]])[1, 5]
  
  # Overwrite the p-value of PF_LEG_perTOTAL that was tested with three levels: Div1,2,3.
  rtable[ which(rtable$var=='PF_LEG_perTOTAL'), 'p_val'] <- p_amt_ave
  rtable
  
  # ---------------------------------------------------------------------------------------------------------------
  # Also need to test the difference in amt_ave between Div0, Div1, and Div2, excluding DivNA.
  myanova <- aov(amt_ave ~ DivGroup, data= subset012)
  ss <- summary(myanova)
  # Take out the p-value.
  p_amt_ave <- as.data.frame(ss[[1]])[1, 5]
  
  # Overwrite the p-value of amt_ave that was tested with three levels: Div1,2,3.
  rtable[ which(rtable$var=='amt_ave'), 'p_val'] <- p_amt_ave
  rtable
  
  
  unadjusted <- rtable
  unadjusted
  
  # Safety check
  mymeans <- df %>% group_by(DivGroup) %>% summarise(means = mean(PF_LEG_perTOTAL))
  mymeans
  mysd <- df %>% group_by(DivGroup) %>% summarise(sds = sd(PF_LEG_perTOTAL))
  mysd
  
  write.table(unadjusted, 'clipboard', sep="\t", row.names = T, quote=F, col.names = NA)
  
# ===============================================================================================================
# Calculate the means, SD, and p-values for anova between DivGroups WITH adjusting by KCAL.
# For those variables that should be adjusted by KCAL.
# ===============================================================================================================
  dim(df)
# eg. adjustment by kcal - protian intake per 2000 kcal.
  df$PROT_adj <- df$PROT / df$KCAL *2000
  df[1:10, c("PROT", "PROT_adj", "KCAL")]
  df %>% group_by(DivGroup) %>% summarise(means= mean(PROT_adj))
  df %>% group_by(DivGroup) %>% summarise(sds= sd(PROT_adj))
  
# Define variables.
  # Copy from Excel column of "Foods.xlsx".
  variablesdf <- read.table('clipboard', sep="\t", header=F)
  variables <- variablesdf$V1
  # Or type.
  # variables <- c("CARB", "PROT", "FIBE", "TFAT", "SFAT", "MFAT", "PFAT", "total_MPFAT", "ALCO", "ADD_SUGARS" )
  # df$
  
  # make a dataframe to save means, SD, and p-values for ANOVA and p-values for trend.
  rtable <- data.frame(matrix(nrow= length(variables), ncol=11))
  colnames(rtable) <- c('var', 'm_DivNA',	'm_Div0',	'm_Div1',	'm_Div2',
                        'sd_DivNA',	'sd_Div0',	'sd_Div1',	'sd_Div2',  'p_aov', 'p_trend') 
  rtable 
  
  # Adjust by KCAL, calc mean and SD, and get p-values of ANOVA for each element in the 'variables' vector.
  for(i in 1:length(variables)){
    print( paste("n=", i))
    
    divn <- which(colnames(df) == "DivGroup")
    
    # Look for the column number for the ith variable
    coln <- which(colnames(df) == variables[i])
    print(coln)

    ### ADJUST BY KCAL ##############
    # Look for the column number for kcal
    kcaln <- which(colnames(df) == "KCAL")
    subset <- df[, c(divn, kcaln, coln)]
    colnames(subset) <- c("DivGroup", "KCAL", "i_variable")
    
    # Overwrite the i_variable with adjusted i_variable.
    subset$i_variable <- subset$i_variable / subset$KCAL * 2000
    ##################################
    
    rtable[i, 1] <- variables[i]
    
    # run anova.
    myanova <- aov(i_variable ~ DivGroup, data=subset)
    ss <- summary(myanova)
    # Take out the p-value.
    pval <- as.data.frame(ss[[1]])[1,5]
    rtable[i, 10] <- pval
    
    # Run regression 
    mylm <- lm(i_variable ~ DivGroup, data=subset)
    print(summary(mylm))  
    
    # Save the p-value for the overall significance of the regression model.
    rtable[i, 11] <- overall_p(mylm)
    
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
  adjusted

  # The p-values for ANOVA and the p-values for the regression were exactly the same.
  # Makes sense, becuase DivGroup is a categorical variable. Running a regression with a categorical
  # variable doesn't really make sense, and it ended up doing the same thing with anova. 
  
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
  
  write.table(adjusted, 'clipboard', sep="\t", row.names = T, quote=F, col.names = NA)
  