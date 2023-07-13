# ===============================================================================================================
# Analyze n=1776 individuals that have no missing data in 
# BMI, cholesterol measurements, and KCAL intake.
# Version 1
# Created on 02/15/2023 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

  library(SASxport)

  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
  source("lib/data_overview.R") 
  source("lib/anova_assumptionI_plots.R") 
  source("lib/add_gender_and_age.R") # to use AddGenderAgeGroups function.  

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/n1776")  

# ===============================================================================================================
# Load the prepared data with LegGroup variable and DivGroup variable.
# ===============================================================================================================
  
  totals_c_hdd <- read.delim("Total_D12_FC_QC_mean_QC_demo_ga_body_meta_Div_cholesterol.txt")
                              
  dim(totals_c_hdd)
  # should be 1776 rows, after removing rows with missing data.
  
# Ensure there is no  missing data.
  library(naniar)
  vis_miss(totals_c_hdd[, c("SEQN", "LBDHDD", "LBXTR", "LBDLDL", "LBXTC", "BMXBMI", "KCAL")])
  
# Make the DivGroup as a factor.
  totals_c_hdd$DivGroup <- factor(totals_c_hdd$DivGroup, 
                                  levels = c('DivNA', 'Div0', 'Div1', 'Div2'))

# Let's look at LegGroups
  table(totals_c_hdd$DivGroup, useNA = "ifany")
  
  # DivNA  Div0  Div1  Div2 
  # 886   563   169   158 
  
  SummaryStats(inputdf=totals_c_hdd, outfn="SummaryStats_totals_c_hdd_n1776.txt")
  
# ---------------------------------------------------------------------------------------------------------------
# Distribution of each group
  df <- totals_c_hdd

# Age
  hist(df$RIDAGEYR)
  summary(df$RIDAGEYR)
  plot(as.factor(df$DivGroup), df$RIDAGEYR) 

# Gender
  table(df$Gender, useNA = "ifany")
  table(df$RIAGENDR, useNA = "ifany") # 1 is male.
  table(df$DivGroup, df$Gender)

  plot(as.factor(df$Gender_Age), df$LBDHDD)  
  plot(as.factor(df$Gender_Age), df$KCAL)  
  
# LBDHDD (HDL)
  hist(df$LBDHDD)
  plot(as.factor(df$DivGroup), df$LBDHDD) 
  df$LBDHDD_log <- log(df$LBDHDD)
  
# LBXTR (triglyceride)
  plot(as.factor(df$DivGroup), df$LBXTR) 
  hist(df$LBXTR)
  # may need to log transform  
  df$LBXTR_log <- log(df$LBXTR)
  hist(df$LBXTR_log)

# LBDLDL
  hist(df$LBDLDL)
  plot(as.factor(df$DivGroup), df$LBDLDL) 
  df$LBDLDL_log <- log(df$LBDLDL)
  hist(df$LBDLDL_log)

# LBXTC
  hist(df$LBXTC)
  plot(as.factor(df$DivGroup), df$LBXTC) 

# KCAL
  hist(df$KCAL)
  plot(as.factor(df$DivGroup), df$KCAL) 
  
  df$KCAL_log <- log(df$KCAL)
  hist(df$KCAL_log)
  plot(as.factor(df$DivGroup), df$KCAL_log) 

# BMI
  hist(df$BMXBMI)
  plot(as.factor(df$DivGroup), df$BMXBMI) 
  df$BMXBMI_log <- log(df$BMXBMI)
  hist(df$BMXBMI_log) # Looks good.
  
  
# ---------------------------------------------------------------------------------------------------------------
# can I adjust for age and gender?
  ??ancova
  # ancova.full <- aov(lm(Response_variable ~ Main_factor + Age + Gender + Age:Main_factor + Gender:Main_factor))   
  ancova.full <- aov(lm(KCAL ~ DivGroup + RIDAGEYR + Gender + RIDAGEYR:KCAL + Gender:KCAL, data=df))   
  ancova.full <- aov(lm(KCAL ~ DivGroup + RIDAGEYR + Gender , data=df))   
  ancova.full <- aov(lm(BMXBMI ~ DivGroup  + RIDAGEYR + Gender , data=df))   
  ancova.full <- aov(lm(BMXBMI ~ DivGroup + KCAL + RIDAGEYR + Gender + DivGroup:KCAL  
                                 + DivGroup:RIDAGEYR + DivGroup:Gender , data=df))   
  ancova.full <- aov(lm( LBDHDD ~ DivGroup + KCAL + RIDAGEYR + Gender + DivGroup:KCAL  
                        + DivGroup:RIDAGEYR + DivGroup:Gender , data=df))   
  ancova.full <- aov(lm( FIBE ~ DivGroup + KCAL + RIDAGEYR + Gender + DivGroup:KCAL  
                         + DivGroup:RIDAGEYR + DivGroup:Gender , data=df))   
 
  anova.simple <-     aov(lm( BMXWAIST ~ DivGroup, data=df))  
  ancova.agegender <- aov(lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender, data=df)) 
  ancova.full <-      aov(lm( BMXWAIST ~ DivGroup + FIBE + RIDAGEYR + Gender + 
                                         PF_TOTAL_LEG + KCAL, data=df))   

  car::Anova(anova.simple,     type="III")
  car::Anova(ancova.agegender, type="III")
  car::Anova(ancova.full,      type="III")
  
  # install.packages("car")
  library(car)
  
# DivGroup not significant but all others are. ??

    
  
# ---------------------------------------------------------------------------------------------------------------
# Run ANOVA
  myanova <- aov(LBDHDD     ~ DivGroup, data=df)
  myanova <- aov(LBDHDD_log ~ DivGroup, data=df)
  myanova <- aov(LBXTR      ~ DivGroup, data=df)
  myanova <- aov(LBXTR_log  ~ DivGroup, data=df)
  myanova <- aov(LBDLDL     ~ DivGroup, data=df)
  myanova <- aov(LBDLDL_log ~ DivGroup, data=df)
  myanova <- aov(LBXTC      ~ DivGroup, data=df)
  myanova <- aov(KCAL       ~ DivGroup, data=df)
  myanova <- aov(KCAL_log   ~ DivGroup, data=df)
  myanova <- aov(BMXBMI     ~ DivGroup, data=df)
  myanova <- aov(BMXBMI_log ~ DivGroup, data=df)
  
  summary(myanova)
  modelsummary <- summary(myanova)
  
  res1 <- residuals(myanova)
  # Generate a 2x2 plot field. 
  par(mfrow = c(2, 2))
  # Histogram of res1.
  hist(res1)
  # QQ plot of res1. 
  qqnorm(res1, plot.it=TRUE)
  qqline(res1)
  # Boxplot
  boxplot(res1 ~ df$DivGroup)
  title("Boxplot of res1")
  # produce residual vs. fitted plot
  plot(fitted(myanova), res1)
  #add a horizontal line at 0 
  abline(0,0)
  title("Fitted vs. Res1 plot")
  # Revert to one-plot-per-field 
  par(mfrow = c(1, 1))
  
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  Levenes_test <- anova(lm(res1sq ~ df$DivGroup))
  Levenes_test
  
  # Or do it with the ANOVA_assumption function.
  ANOVA_assumption(input.anova = myanova,
                   input.factor = "DivGroup",
                   df=df)
  # Great!!
  
  p <- modelsummary
  # p[[1]][1]
  # pvalue <- p[[1]][5]

  # If ANOVA is significant, you can do a pairwise t-test.
  pairwise.t.test(df$LBDHDD,     df$DivGroup, p.adjust.method = "none") 
  pairwise.t.test(df$LBDHDD_log, df$DivGroup, p.adjust.method = "fdr") # p value got higher... hmm?  
  pairwise.t.test(df$BMXBMI,     df$DivGroup, p.adjust.method = "none") 
  pairwise.t.test(df$BMXBMI_log, df$DivGroup, p.adjust.method = "none") 
  pairwise.t.test(df$KCAL,       df$DivGroup, p.adjust.method = "none") 
  
  aggregate(df$LBDHDD, list(df$DivGroup), FUN=mean)
  
# ---------------------------------------------------------------------------------------------------------------
## Tukey's HSD, base R. Even though this function has p.adjust.method argument, the resulting 
 # p-values are the same; what you put into the argument doesn't matter. It makes sense because
 # Tukey HSD corrects family-wise error rate already.
  
# HDL  
       TukeyHSD(aov(LBDHDD     ~ DivGroup, data=df))
# log HDL
       TukeyHSD(aov(LBDHDD_log ~ DivGroup, data=df))
    
# KCAL
       TukeyHSD(aov(KCAL     ~ DivGroup, data=df))
# log KCAL
       TukeyHSD(aov(KCAL_log ~ DivGroup, data=df))

# BMI
       TukeyHSD(aov(BMXBMI     ~ DivGroup, data=df))
# log_BMI
       TukeyHSD(aov(BMXBMI_log ~ DivGroup, data=df))
       
  
# ---------------------------------------------------------------------------------------------------------------
## Tukey's Honestly Significant Difference. by Filipe. When you want letters separation.

# Define what to repeat in function first...
  library(agricolae)

      TukeyMeansLetters <- function(model, var, mean.out.fn){
        modelsummary <- summary(model)
        writeLines(c("","<<Model summary>>", ""))
        print(summary(model))
        
        # ANOVA table.
        out <- HSD.test(model, var, group=TRUE, console=TRUE)
        
        letters <- out$groups
        letters$VarOfInterest <- rownames(letters) 
        
        means <- out$means
        means$VarOfInterest <- rownames(means) 
        
        # Add letters to the means table.
        means_abc <- merge(means, letters[, c("VarOfInterest", "groups")], all.x = T, by="VarOfInterest")
    
        writeLines(c("","<<Means with letters>>", ""))
        print(means_abc)
        
        write.table(x=means_abc, file=mean.out.fn, sep="\t", row.names=F, quote=F)
      }
  
# ---------------------------------------------------------------------------------------------------------------
  # Run the mean separation and lettering for each phenotype.  
  
  # LBDHDD
  TukeyMeansLetters(model=aov(LBDHDD     ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_HDL_n1776.txt" )
  # LBDHDD_log
  TukeyMeansLetters(model=aov(LBDHDD_log ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_HDLlog_n1776.txt" )
  
  # LBXTR
  TukeyMeansLetters(model=aov(LBXTR ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_LBXTR_n1776.txt" )
  # LBXTR_log
  TukeyMeansLetters(model=aov(LBXTR_log ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_LBXTR_log_n1776.txt" )
  
  # LBDLDL
  TukeyMeansLetters(model=aov(LBDLDL ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_LBDLDL_n1776.txt" )
  # LBDLDL_log
  TukeyMeansLetters(model=aov(LBDLDL_log ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_LBDLDL_log_n1776.txt" )

  # LBXTC - total cholesterol.
  TukeyMeansLetters(model=aov(LBXTC ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_LBXTC_n1776.txt" )

  # KCAL
  summary(df$KCAL) # no missing data.
  TukeyMeansLetters(model=aov(KCAL ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_KCAL_n1776.txt" )
  # KCAL_log
  TukeyMeansLetters(model=aov(KCAL_log ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_KCAL_log_n1776.txt" )
  
  # BMI
  summary(df$BMXBMI) # no missing data.
  TukeyMeansLetters(model=aov(BMXBMI ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_BMI_n1776.txt" )
  # BMI log
  TukeyMeansLetters(model=aov(BMXBMI_log ~ DivGroup, data=df), var="DivGroup",
                    mean.out.fn="Div_means_abs_BMI_log_n1776.txt" )
  
  
  # ####### BY HAND ########    
  # # ANOVA table.
  # summary(model)
  # out <- HSD.test(model, "DivGroup", group=TRUE, console=TRUE)
  # 
  # letters <- out$groups
  # letters$DivGroup <- rownames(letters) 
  # 
  # means <- out$means
  # means$DivGroup <- rownames(means) 
  # 
  # # Add letters to the means table.
  # means_abc <- merge(means, letters[, c("DivGroup", "groups")], all.x = T, by="DivGroup")
  # 
  # write.table(means_abc, "Div_means_abs_KCAL_n1790.txt", sep="\t", row.names=F, quote=F)
  # 
  # ###### TILL HERE ######
   