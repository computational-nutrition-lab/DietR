# ===============================================================================================================
# Covariate selection - Forward selection.
# Version 1
# Created on 03/23/2023 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Set your working directory to the main directory.

# ===============================================================================================================
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist/")  

# ===============================================================================================================
# Load the prepared data with LegGroup variable and DivGroup variable.
# ===============================================================================================================

# Load the data saved in . 
totals_c_wa <- read.delim("../Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup_waistBMI.txt")

dim(totals_c_wa)
# should be 4038 rows, after removing rows containing missing data.

# Ensure there is no  missing data.
naniar::vis_miss(totals_c_wa[, c("SEQN","BMXWAIST","BMXBMI","FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL", 
                                 "Gender", "RIDAGEYR")])

# Make the DivGroup as a factor.
totals_c_wa$DivGroup <- factor(totals_c_wa$DivGroup, 
                               levels = c('DivNA', 'Div0', 'Div1', 'Div2'))

# Make Gender as a factor.
table(totals_c_wa$Gender)
# totals_c_wa$Gender <- factor(totals_c_wa$Gender, 
# levels = c('F', 'M'))

# Let's look at Groups
table(totals_c_wa$DivGroup, useNA = "ifany")

# DivNA  Div0  Div1  Div2 
# 2012  1246   387   393 

# ---------------------------------------------------------------------------------------------------------------
# Distribution of each group
df <- totals_c_wa

# Age
hist(df$RIDAGEYR)
summary(df$RIDAGEYR)
plot(as.factor(df$DivGroup), df$RIDAGEYR) 

# Gender
table(df$Gender, useNA = "ifany")
table(df$RIAGENDR, useNA = "ifany") # 1 is male.
table(df$DivGroup, df$Gender)

plot(as.factor(df$Gender_Age), df$KCAL)  
plot(as.factor(df$Gender_Age), df$BMXWAIST)  

# Also from boxplot, it is clear that the waist.cir is decreasing.   
plot(df$DivGroup, df$BMXWAIST) 

# combinations of two numeric variables are probably too obscured...
plot(df$KCAL, df$BMXWAIST) 

# ===============================================================================================================
# Select covariates. 
# Covariates are variables that are correlated with the outcome,
# variables that are uncorrelated with each other (to avoid multi-colinearity).
# as few covariables as possible.

# Covariates are included to "adjust" or "cancel out" their effects on the outcomes.  
# BMI is highly correlated with Waist, but we don't want to include it as a covariate because
# BMI is another outcome, and we are not interested in BMI-adjusted waist circumferences.  

# ===============================================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Take the outcome and seemingly relevant covariates, and make a correlation matrix. 
x <- df[, c("BMXWAIST", "BMXBMI","RIAGENDR", "RIDAGEYR", "FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL")]
x2 <- df[, c("BMXWAIST", "BMXBMI","RIAGENDR", "RIDAGEYR",  "PF_LEGUMES", "KCAL")]
# x <- df[, c("BMXWAIST", "RIAGENDR", "RIDAGEYR", "FIBE", "PF_LEGUMES", "KCAL")]
# x <- df[, c("KCAL", "RIAGENDR", "RIDAGEYR", "FIBE", "PF_LEGUMES")]
# x <- df[, c("BMXBMI", "RIAGENDR", "RIDAGEYR",  "KCAL", "FIBE", "PF_LEGUMES", "BMXWAIST")]
head(x)  
str(x)  

# Get correlation coeff R and p-values for them.
rp <- Hmisc::rcorr(x= as.matrix(x), type ="pearson")
rp
write.table(rp[[1]], 'clipboard', sep="\t", row.names=T, quote=F) # R
write.table(rp[[3]], 'clipboard', sep="\t", row.names=T, quote=F) # p-values

PerformanceAnalytics::chart.Correlation(R=x)
PerformanceAnalytics::chart.Correlation(R=x2)

# Just by removing highly correlated variables and keeping variables that are correlated with the outcome,
# covariates were selected: Age, GEnder, and KCAL.
# BMI is another outcome, so we do not treat it as a covariate here. 

  lm.ag <-    lm( BMXWAIST ~ DivGroup + RIAGENDR, data=df)
  car::Anova(lm.g, type="III")
  # p = 1.451e-06 ***

  


cor.test(x2$BMXWAIST, x2$PF_LEGUMES) # Not correlated. So this can be omitted.  
cor.test(x2$BMXWAIST, x2$KCAL) # p=0.019.
anova(lm(KCAL ~ DivGroup, df)) # p= 9.673e-11.
anova(lm(PF_LEGUMES ~ DivGroup, df)) # p= 2.2e-16.
boxplot(PF_LEGUMES ~DivGroup, df)


# Age vs waist. R= 0.22. p=0.0000. But so scattered.
plot(df$RIDAGEYR, df$BMXWAIST)
plot(df$FIBE, df$KCAL) # R=0.52, p=0.0000. looks like. 
plot(df$FIBE, df$BMXBMI)
# BMI and Waist are very highly correlated (R=0.91, p=0.0000).

# PF_TOTAL_LEG vs waist. 
plot(df$PF_TOTAL_LEG, df$BMXWAIST)  
# No correlation. 
# low p-value for R does not necessarily mean a meaningful correlation because n is just so huge.  

# Plain means
  aggregate(df$BMXWAIST, list(df$DivGroup), FUN=mean)
  #   Group.1         x
  # 1   DivNA 101.07003
  # 2    Div0 100.65546
  # 3    Div1  98.91059
  # 4    Div2  95.78931
  # DivNA-Div2 = 101.07003-95.78931 = 5.28072.

# ===============================================================================================================
# OR Use Forward Selection of covariates.
# ===============================================================================================================
  According to: 
  #   Sauer 2013 [Book] Ch7. Covariate selection.
  # https://www.ncbi.nlm.nih.gov/books/NBK126194/
    
# ===============================================================================================================
# 00. Start with no covariates.
# ===============================================================================================================

  lm.nill <-    lm( BMXWAIST ~ DivGroup , data=df)
  anova(lm.nill)
  summary((lm.nill))
  car::Anova(lm.nill, type="III")
  # DivGroup effetc is significant.
  emmeans::emmeans(lm.nill, pairwise ~ DivGroup )
# DivNA - Div2= 5.281 cm, p<.0001. 

# ===============================================================================================================
# 01. Test the association between the initial model and each of the covariates. 
# ===============================================================================================================
  covlist1 = data.frame(cov= c("RIAGENDR", "RIDAGEYR", "KCAL", "FIBE", 
               "PF_TOTAL_LEG", "PF_LEGUMES"), pval=NA)

# ---------------------------------------------------------------------------------------------------------------
# cov1 = "RIAGENDR" Gender.
  lm.g <-    lm( BMXWAIST ~ DivGroup + RIAGENDR, data=df)
  car::Anova(lm.g, type="III")
  # p = 1.451e-06 ***
  comp = anova(lm.nill, lm.g)
  covlist1[1,2] <-  comp$`Pr(>F)`[2] 
  covlist1
  
# ---------------------------------------------------------------------------------------------------------------
# cov2 = "RIDAGEYR" Age
  lm.a <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR, data=df)
  car::Anova(lm.a, type="III")
  comp = anova(lm.nill, lm.a )
  covlist1[2,2] <-  comp$`Pr(>F)`[2] 
  
# ---------------------------------------------------------------------------------------------------------------
# cov3 = "KCAL" 
  lm.k <-    lm( BMXWAIST ~ DivGroup + KCAL, data=df)
  car::Anova(lm.k, type="III")
  comp = anova(lm.nill, lm.k )
  covlist1[3,2] <-  comp$`Pr(>F)`[2] 
  
# ---------------------------------------------------------------------------------------------------------------
# cov4 = "FIBE" 
  lm.f <-    lm( BMXWAIST ~ DivGroup + FIBE, data=df)
  car::Anova(lm.f, type="III")
  comp = anova(lm.nill, lm.f ) # NS!
  covlist1[4,2] <-  comp$`Pr(>F)`[2] 
  
# ---------------------------------------------------------------------------------------------------------------
# cov5 = "PF_TOTAL_LEG" 
  lm.tl <-    lm( BMXWAIST ~ DivGroup + PF_TOTAL_LEG, data=df)
  car::Anova(lm.tl, type="III")
  comp = anova(lm.nill, lm.tl ) 
  covlist1[5,2] <-  comp$`Pr(>F)`[2] 
  
# ---------------------------------------------------------------------------------------------------------------
# cov6 = "PF_LEGUMES" 
  lm.l <-    lm( BMXWAIST ~ DivGroup + PF_LEGUMES, data=df)
  car::Anova(lm.l, type="III")
  comp = anova(lm.nill, lm.l ) 
  covlist1[6,2] <-  comp$`Pr(>F)`[2] 
  
  covlist1[order(covlist1$pval, decreasing = F),]
# Age is the most significant. Add Age to the model.   

# ===============================================================================================================
# 02. Test the association between {the initial model + Age} and each of the covariates. 
# ===============================================================================================================
  covlist2 = data.frame(cov= c("RIAGENDR", "KCAL", "FIBE", 
                              "PF_TOTAL_LEG", "PF_LEGUMES"), pval=NA)
  
# Compare {the initial model + Age} and {the initial model + Age + cov being tested now}
# ---------------------------------------------------------------------------------------------------------------
# cov1 = "RIAGENDR" Gender.
  lm.ag <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + RIAGENDR, data=df)
  car::Anova(lm.ag, type="III")
  comp = anova(lm.a, lm.ag)
  covlist2[1,2] <-  comp$`Pr(>F)`[2] 
  covlist2
  
# ---------------------------------------------------------------------------------------------------------------
# cov2 = "KCAL" 
  lm.ak <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL, data=df)
  car::Anova(lm.ak, type="III")
  comp = anova(lm.a, lm.ak)
  covlist2[2,2] <-  comp$`Pr(>F)`[2] 
  covlist2
  
# ---------------------------------------------------------------------------------------------------------------
# cov3 = "FIBE" 
  lm.af <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + FIBE, data=df)
  car::Anova(lm.af, type="III")
  comp = anova(lm.a, lm.af) # NS.
  covlist2[3,2] <-  comp$`Pr(>F)`[2] 
  covlist2
  
# ---------------------------------------------------------------------------------------------------------------
# cov4 = "PF_TOTAL_LEG" 
  lm.atl <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + PF_TOTAL_LEG, data=df)
  car::Anova(lm.atl, type="III")
  comp = anova(lm.a, lm.atl) 
  covlist2[4,2] <-  comp$`Pr(>F)`[2] 
  covlist2
  
# ---------------------------------------------------------------------------------------------------------------
# cov5 = "PF_LEGUMES" 
  lm.al <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + PF_LEGUMES, data=df)
  car::Anova(lm.al, type="III")
  comp = anova(lm.a, lm.al) 
  covlist2[5,2] <-  comp$`Pr(>F)`[2] 
  covlist2
  
  covlist2[order(covlist2$pval), ]
  # So close, but KCAL has the lowest p-value.
  
# ===============================================================================================================
# 03. Test the association between {the initial model + Age + KCAL} and each of the covariates. 
# ===============================================================================================================
  covlist3 = data.frame(cov= c("RIAGENDR", "FIBE", 
                               "PF_TOTAL_LEG", "PF_LEGUMES"), pval=NA)
  
# Compare {the initial model + Age + KCAL} and {the initial model + Age + cov being tested now}
# ---------------------------------------------------------------------------------------------------------------
# cov1 = "RIAGENDR" Gender.
  lm.akg <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + RIAGENDR, data=df)
  car::Anova(lm.akg, type="III")
  comp = anova(lm.ak, lm.akg)
  covlist3[1,2] <-  comp$`Pr(>F)`[2] 
  covlist3

# ---------------------------------------------------------------------------------------------------------------
# cov2 = "FIBE" 
  lm.akf <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + FIBE, data=df)
  car::Anova(lm.akf, type="III")
  comp = anova(lm.ak, lm.akf)
  covlist3[2,2] <-  comp$`Pr(>F)`[2] 
  
# ---------------------------------------------------------------------------------------------------------------
# cov3 = "PF_TOTAL_LEG" 
  lm.aktl <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + PF_TOTAL_LEG, data=df)
  car::Anova(lm.aktl, type="III")
  comp = anova(lm.ak, lm.aktl)
  covlist3[3,2] <-  comp$`Pr(>F)`[2] 
  
# ---------------------------------------------------------------------------------------------------------------
# cov4 = "PF_LEGUMES" 
  lm.akl <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + PF_LEGUMES, data=df)
  car::Anova(lm.akl, type="III")
  comp = anova(lm.ak, lm.akl)
  covlist3[4,2] <-  comp$`Pr(>F)`[2] 
  
  covlist3[order(covlist3$pval), ]

# FIBE is most significant, so add FIBE to the model.   
  
# ===============================================================================================================
# 04. Test the association between {the initial model + Age + KCAL + FIBE} and each of the covariates. 
# ===============================================================================================================
  covlist4 = data.frame(cov= c("RIAGENDR", "PF_TOTAL_LEG", "PF_LEGUMES"), pval=NA)
  
# Compare {the initial model + Age + KCAL + FIBE} and {the initial model + Age + KCAL + FIBE + cov being tested now}
# ---------------------------------------------------------------------------------------------------------------
# cov1 = "RIAGENDR" Gender.
  lm.akfg <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + FIBE + RIAGENDR, data=df)
  car::Anova(lm.akfg, type="III")
  comp = anova(lm.akf, lm.akfg)
  covlist4[1,2] <-  comp$`Pr(>F)`[2] 
  covlist4

# ---------------------------------------------------------------------------------------------------------------
# cov2 = "PF_TOTAL_LEG" 
  lm.akftl <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + FIBE + PF_TOTAL_LEG, data=df)
  car::Anova(lm.akftl, type="III")
  comp = anova(lm.akf, lm.akftl)
  covlist4[2,2] <-  comp$`Pr(>F)`[2] 
  covlist4
  
# ---------------------------------------------------------------------------------------------------------------
# cov3 = "PF_LEGUMES" 
  lm.akfl <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + FIBE + PF_LEGUMES, data=df)
  car::Anova(lm.akfl, type="III")
  comp = anova(lm.akf, lm.akfl)
  covlist4[3,2] <-  comp$`Pr(>F)`[2] 
  
  covlist4[order(covlist4$pval), ]
  # PF_LEGUMES is the most significant. Add PF_LEGUMES to the model. I may end up adding all the cov 
  # to the model...
  
# ===============================================================================================================
# 05. Test the association between {the initial model + Age + KCAL + FIBE + PF_LEGUMES} and each of the covariates. 
# ===============================================================================================================
  covlist5 = data.frame(cov= c("RIAGENDR", "PF_TOTAL_LEG"), pval=NA)
  
# Compare {the initial model + Age + KCAL + FIBE + PF_LEGUMES} and 
#         {the initial model + Age + KCAL + FIBE + PF_LEGUMES + cov being tested now}
# ---------------------------------------------------------------------------------------------------------------
# cov1 = "RIAGENDR" Gender.
  lm.akflg <- lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + FIBE + PF_LEGUMES + RIAGENDR, data=df)
  car::Anova(lm.akflg, type="III")
  comp = anova(lm.akfl, lm.akflg)
  covlist5[1,2] <-  comp$`Pr(>F)`[2] 
  covlist5
  
# ---------------------------------------------------------------------------------------------------------------
# cov2 = "PF_TOTAL_LEG" 
  lm.akfltl <- lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + FIBE + PF_LEGUMES + PF_TOTAL_LEG, data=df)
  car::Anova(lm.akfltl, type="III")
  comp = anova(lm.akfl, lm.akfltl)
  covlist5[2,2] <-  comp$`Pr(>F)`[2] 
  covlist5

  # add GEnder to the model. 
  
# ===============================================================================================================
# 06. Test the association between {the initial model + Age + KCAL + FIBE + PF_LEGUMES + Gender} and each of the covariates. 
# ===============================================================================================================
  lm.akflgtl <- lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + FIBE + PF_LEGUMES + RIAGENDR + PF_TOTAL_LEG, data=df)
  car::Anova(lm.akflgtl, type="III")
  comp = anova(lm.akflg, lm.akflgtl)
  p=0.1233.

# So, do not add PF_LEGUMES as a covariate.
# By using FOrward selection method of covariates, RIDAGEYR, KCAL, FIBE, PF_LEGUMES, RIAGENDR were 
# selected as covariates. 
  
  
# ===============================================================================================================
# 11. Use the model with selected covariates to get means. 
# ===============================================================================================================
  
  lm.akflg <- lm( BMXWAIST ~ DivGroup + RIDAGEYR + KCAL + FIBE + PF_LEGUMES + RIAGENDR, data=df)
  
  emmeans::emmeans(lm.akflg, pairwise ~ DivGroup )
# DivNA - Div2 = 5.227 cm, p<0.0001.   
# emmeans are close to plain means, and look good. 
  
  aggregate(df$BMXWAIST, list(df$DivGroup), FUN=mean)
  #   Group.1         x
  # 1   DivNA 101.07003
  # 2    Div0 100.65546
  # 3    Div1  98.91059
  # 4    Div2  95.78931
  
  
  
  