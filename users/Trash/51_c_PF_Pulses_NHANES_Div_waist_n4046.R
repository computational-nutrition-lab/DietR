# ===============================================================================================================
# Continuation from "~Div_Waist_prep.R".
# Analyze n=4046 individuals that have no missing data in 
# waist circum, KCAL etc...
# Version 1
# Created on 02/22/2023 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

  library(SASxport)

  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
  # source("lib/data_overview.R") 
  # source("lib/add_gender_and_age.R") # to use AddGenderAgeGroups function.  

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/PF/Waist/")  

# ===============================================================================================================
# Load the prepared data with LegGroup variable and DivGroup variable.
# ===============================================================================================================
  
  totals_c_wa <- read.delim("../Total_D12_FC_QC_mean_QC_demo_ga_body_meta_DivGroup_waist.txt")
                              
  dim(totals_c_wa)
  # should be 4046 rows, after removing rows containing missing data.
  
# Ensure there is no  missing data.
  naniar::vis_miss(totals_c_wa[, c("SEQN", "BMXWAIST", "FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL", 
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
  # 2017  1248   388   393 
  
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
  
# ---------------------------------------------------------------------------------------------------------------
  
# ===============================================================================================================
# can I adjust for age and gender?
# ===============================================================================================================
  ??ancova
  # # ancova.full <- aov(lm(Response_variable ~ Main_factor + Age + Gender + Age:Main_factor + Gender:Main_factor))
  # ancova.full <- aov(lm(KCAL ~ DivGroup + RIDAGEYR + Gender + RIDAGEYR:KCAL + Gender:KCAL, data=df))   
  # ancova.full <- aov(lm(KCAL ~ DivGroup + RIDAGEYR + Gender , data=df))   
  # ancova.full <- aov(lm(BMXBMI ~ DivGroup  + RIDAGEYR + Gender , data=df))   
  # ancova.full <- aov(lm(BMXBMI ~ DivGroup + KCAL + RIDAGEYR + Gender + DivGroup:KCAL  
  #                                + DivGroup:RIDAGEYR + DivGroup:Gender , data=df))   
  # ancova.full <- aov(lm( LBDHDD ~ DivGroup + KCAL + RIDAGEYR + Gender + DivGroup:KCAL  
  #                       + DivGroup:RIDAGEYR + DivGroup:Gender , data=df))   
  # ancova.full <- aov(lm( FIBE ~ DivGroup + KCAL + RIDAGEYR + Gender + DivGroup:KCAL  
  #                        + DivGroup:RIDAGEYR + DivGroup:Gender , data=df))   

# Resp = Waist circumference.
  anova.simple <-     aov(lm( BMXWAIST ~ DivGroup, data=df))  
  ancova.agegender <- aov(lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender, data=df)) 
  ancova.full <-      aov(lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + FIBE +  
                                         PF_TOTAL_LEG + KCAL, data=df)) 
  
  # install.packages("car")
  library(car)
  car::Anova(anova.simple,     type="III")
  car::Anova(ancova.agegender, type="III")
  car::Anova(ancova.full,      type="III")
  
  
  #### lm... Same model but without the aov.
  lm.simple <- lm( BMXWAIST ~ DivGroup, data=df)  
  
  car::Anova(lm.simple,      type="III")  # gives the same results.
  ####
  
  
# Compare models.  
  # anova(full_model, reduced_model)
   anova(ancova.full, ancova.agegender)  
   # p=1.296e-07 *** indicates reducing full model to ancova.agegender makes a huge difference.
   
# So, all terms in full model are important in predicting waist.cir. 
# Also from boxplot, it is clear that the waist.cir is decreasing.   
  plot(df$DivGroup, df$BMXWAIST) 
  
# combinations of two numeric variables are probably too obscured...
  plot(df$KCAL, df$BMXWAIST) 

# ---------------------------------------------------------------------------------------------------------------
# Correlation matrix
  x <- df[, c("BMXWAIST", "RIAGENDR", "RIDAGEYR", "FIBE", "PF_TOTAL_LEG", "PF_LEGUMES", "KCAL")]
  x <- df[, c("BMXWAIST", "RIAGENDR", "RIDAGEYR", "FIBE", "PF_LEGUMES", "KCAL")]
  x <- df[, c("KCAL", "RIAGENDR", "RIDAGEYR", "FIBE", "PF_LEGUMES")]
  x <- df[, c("BMXBMI", "RIAGENDR", "RIDAGEYR",  "KCAL", "FIBE", "PF_LEGUMES", "BMXWAIST")]
  str(x)  
# Get correlation coeff R and p-values for them.
  rp <- Hmisc::rcorr(x= as.matrix(x), type ="pearson")
  write.table(rp[[2]], 'clipboard', sep="\t", row.names=T, quote=F)
  write.table(rp[[3]], 'clipboard', sep="\t", row.names=T, quote=F)

# Age vs waist. R= 0.22, p=0.0000. But so scattered.
  plot(df$RIDAGEYR, df$BMXWAIST)
  plot(df$FIBE, df$KCAL)
  plot(df$FIBE, df$BMXBMI)
  
# PF_TOTAL_LEG vs waist. R=0.04,  p=0.0138
  plot(df$PF_TOTAL_LEG, df$BMXWAIST)  
# No correlation. 
# low p-value for R does not necessarily mean a meaningful correlation because n is just so huge.  
  

# ===============================================================================================================
# ancova.full - BMXWAIST.
# ===============================================================================================================
# Check assumptions.  
  res1 <- residuals(ancova.full)
  res1 <- residuals(lm.kcal.agf)
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
  plot(fitted(ancova.full), res1)
  #add a horizontal line at 0 
  abline(0,0)
  title("Fitted vs. Res1 plot")
  par(mfrow = c(1, 1))
  
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  Levenes_test <- anova(lm(res1sq ~ df$DivGroup))
  Levenes_test 
  # variance is different among DivGroups...

# pairwise emmeans, from the tutorial.
# lm.full needs to be used, because emmeans only supports lm or glm as an input.
  emmeans::emmeans(lm.full, pairwise ~ DivGroup) 
                  
# pairwise difference obtained;  DivNA - Div2 = 5.24 cm, p<.0001. 
# simple means by group are this different.   
  aggregate(df$BMXWAIST, list(df$DivGroup), FUN=mean)
  
  lm.agegender <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender , data=df)
  emmeans::emmeans(lm.agegender, pairwise ~ DivGroup )

  lm.agegender.f <-  lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + FIBE, data=df)
  emmeans::emmeans(lm.agegender.f, pairwise ~ DivGroup )
  
  lm.agegender.p <-  lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + PF_TOTAL_LEG, data=df)
  emmeans::emmeans(lm.agegender.p, pairwise ~ DivGroup )
  
  lm.agegender.k <-  lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + KCAL, data=df)
  emmeans::emmeans(lm.agegender.k, pairwise ~ DivGroup )
  
  lm.agegender.pk <-  lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + PF_TOTAL_LEG + KCAL, data=df)
  emmeans::emmeans(lm.agegender.pk, pairwise ~ DivGroup )
  
  
  lm.full <-   lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + FIBE + PF_TOTAL_LEG + KCAL, data=df)
  emmeans::emmeans(lm.full, pairwise ~ DivGroup )
  # The more terms, very slighly the less difference, but overall, means don't change much. 
  
# ===============================================================================================================
# ancova - BMXWAIST ~ DivGroup + Age + Gender + FIBE.
# ===============================================================================================================
# According to eg_data\NHANES\PF\Covariate selection.xlsx
  
  lm.agf <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + FIBE, data=df)
  emmeans::emmeans(lm.agf, pairwise ~ DivGroup )
  
  car::Anova(lm.agf, type="III")
  # FIBE does not have effect. OH... 
  
  # Try KCAL instead of FIBE. KCAL and FIBE are both weakly correlated (|R|=0.04) with WAIST, and 
  # they are highly correlated with each other (R=0.52), so, only one of them is needed, I think. 
  
# ===============================================================================================================
# ancova - BMXWAIST ~ DivGroup + Age + Gender + KCAL.
# ===============================================================================================================
  
  lm.agk <-    lm( BMXWAIST ~ DivGroup + RIDAGEYR + Gender + KCAL, data=df)
  emmeans::emmeans(lm.agk, pairwise ~ DivGroup )
  
  car::Anova(lm.agk, type="III")
  # KCAL has an effect (p=0.000106). OK.. interesting. So I can just have KCAL in the model, then.
  
# ===============================================================================================================
# ancova - KCAL ~ DivGroup + Age + Gender + FIBE.
# ===============================================================================================================
# According to eg_data\NHANES\PF\Covariate selection.xlsx
  
  lm.kcal.agf <-    lm( KCAL ~ DivGroup + RIDAGEYR + Gender + FIBE, data=df)
  emmeans::emmeans(lm.kcal.agf, pairwise ~ DivGroup )
  # DivNA-Div2=251.4. 
  
  car::Anova(lm.kcal.agf, type="III")
  # All terms are significant.
  # Residuals are normally distributed, and their variances are equal among levels of DivGroup. Good.
  


  
#### Toil and moil below...     
# ===============================================================================================================
# ancova.full.log. Log-transform the response variable.
# ===============================================================================================================
  
  ancova.full.log <-    aov(lm( log(BMXWAIST) ~ DivGroup + FIBE + RIDAGEYR + Gender + 
                                                PF_TOTAL_LEG + KCAL, data=df)) 
  car::Anova(ancova.full.log, type="III")
  
  lm.full.log <-    lm( log(BMXWAIST) ~ DivGroup + FIBE + RIDAGEYR + Gender + 
                                        PF_TOTAL_LEG + KCAL, data=df) 
  
  car::Anova(lm.full.log, type="III")
  # Still, all terms are significant.
  # Compare ancova.agegender.log and ancova.full.log, and p= 3.956e-07 ***. i.e., reducing to age-gender only 
  # model makes a huge difference. Hence, keep all the terms of the full model.
  
  # Check assumptions.  
  res1 <- residuals(ancova.full.log)
  # Generate a 2x2 plot field. 
  par(mfrow = c(2, 2))
  # Histogram of res1.
  hist(res1)
  # QQ plot of res1. 
  qqnorm(res1, plot.it=TRUE)
  qqline(res1)  # Much better!
  # Boxplot
  boxplot(res1 ~ df$DivGroup)
  title("Boxplot of res1")
  # produce residual vs. fitted plot
  plot(fitted(ancova.full), res1)
  #add a horizontal line at 0 
  abline(0,0)
  title("Fitted vs. Res1 plot")
  par(mfrow = c(1, 1))
  
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  anova(lm(res1sq ~ df$DivGroup))
  # Still, variance is different among DivGroups...
  
  # I should use unequal variance model...? --> below.
  
# Anyway Let's get Means so that I can say mean difference between DivGroups is xxxx.

### HSD test ... controls family-wise error rate when doing multiple comparisons.
# can be used even when ANOVA is not significant beause the Tukey test controls the Type I error rate on its own.
# but equal variance of groups is assumed.
# agricolae package.
  hsd <- agricolae::HSD.test(ancova.full.log, "DivGroup", group=TRUE, console=TRUE)
  hsd
  # agricolae::HSD.test(lm.full.log, "DivGroup", group=TRUE, console=TRUE) also produces the same results.

  # check if the means are similar to plain means.
  aggregate(log(df$BMXWAIST), list(df$DivGroup), FUN=mean) # they are the same.
  plot(df$DivGroup, df$BMXWAIST)
  # looks good.
  
  # Calc difference between DivNA and Div2.
  diffNA_2 <- hsd$groups['DivNA',1] - hsd$groups['Div2',1]
  diffNA_2
  # this is log, so need to back-t.
  exp(diffNA_2) # 1.053 cm.... because log(DivNA)-log(Div2) = log(DivNA/Div2); different from log(DivNA-Div2).
  
  # back-t first.
  back_t_diffNA_2 <- exp(hsd$groups['DivNA',1]) - exp(hsd$groups['Div2',1])
  back_t_diffNA_2
  # 5.0097 cm. closer...   
  # The closest pairwise difference is;  exp(DivNA)-exp(Div2)=5.0097 cm, (Tukey HSD, alpha=0.05). 
  
  
### pairwise emmeans, from the tutorial.
  # This is not useful. The differences are log(DivNA) - log(Div2) etc., so the difference cannot be 
  # backtransformed properly. 
  emmeans::emmeans(lm.full.log, pairwise ~ DivGroup )
  emmeans::emmeans(lm.full.log, pairwise ~ DivGroup, mode = "df.error" ) # same results as above.
  
  # emmeans. from YBC Fiber script. backtransformed. 
  emmeans <- emmeans::emmeans(lm.full.log, "DivGroup", type="response", mode = "df.error")
  emmeans
  # This is back-transformed.
  # each level has different SE, as expected.
  
  #### trial and error according to vigniette("interactions", "emmeans")
  # pairwise_emm2 <- emmeans(fitdata3, pairwise ~ Religion * Race | Dimension, type = "response") 
  pairwise_emm2 <- emmeans::emmeans(lm.full.log, pairwise ~ DivGroup * Gender , type = "response") # all combination of DivGroup * Gender. 
  pairwise_emm2 <- emmeans::emmeans(lm.full.log, pairwise ~ DivGroup , type = "response")  # by DivGroup.
  pairwise_emm2 <- emmeans::emmeans(lm.full.log, pairwise ~ DivGroup | Gender , type = "response")  # by DivGroup and by Gender.
  pairwise_emm2
  pairwise_emm2$contrasts
  # M and F have exactly the same results...?
  # emmeans::emmip(lm.full.log, type ~ size | side)
  emmeans::emmip(lm.full.log, Gender ~ DivGroup  ) # DivGroup in x, gender in legend.
  # response variable decreases in a similar way for both F and M. 
  # This gives differences only in ratios.  
  
### use lsmeans (old version of emmeans)
  mylsmeans <- lsmeans::lsmeans(lm.full.log, "DivGroup", type="response", mode = "df.error")
  # Gives the same results as emmeans::emmeans. lsmeans has been renamed as emmeans; but it is doing the same thing.
  
  # As a convenience, a pairs method is provided that calls contrast with method="pairwise". 
  lsmeans::contrast(mylsmeans, method="pairwise")
  # contrast is constructed with ratio, not difference, because my response variable is log-transformed... hmm.... 
  
  # Create a log-transformed response variable first.
  df$BMXWAIST_log <- log(df$BMXWAIST)
  lm.full.log2 <-    lm( BMXWAIST_log ~ DivGroup + FIBE + RIDAGEYR + Gender + 
                         PF_TOTAL_LEG + KCAL, data=df) 
  
  mylsmeans2 <- lsmeans::lsmeans(lm.full.log2, "DivGroup", type="response", mode="df.error")
  mylsmeans2
  lsmeans::contrast(mylsmeans2, method="pairwise")
  # DivNA - Div2  = 0.05323.
  exp(0.05323)
  # only 1.054672. because it is doing ln(DivNA)-ln(Div2). No good!  
  
    
# ===============================================================================================================
# ancova.full.log.uv (unequal variance)
# ===============================================================================================================
# use unequal variance model.
  ancova.full.log.uv = nlme::gls(log(BMXWAIST) ~ DivGroup + FIBE + RIDAGEYR + factor(Gender) + 
                                                 PF_TOTAL_LEG + KCAL, data=df,
                                 weights = nlme::varIdent(form = ~ 1 | DivGroup))
  ancova.full.log.uv
  
  # Check assumptions again.  
  res1 <- residuals(ancova.full.log.uv)
  # Generate a 2x2 plot field. 
  par(mfrow = c(2, 2))
  # Histogram of res1.
  hist(res1)
  # QQ plot of res1. 
  qqnorm(res1, plot.it=TRUE)
  qqline(res1)  # Much better!
  # Boxplot
  boxplot(res1 ~ df$DivGroup)
  title("Boxplot of res1")
  # produce residual vs. fitted plot
  plot(fitted(ancova.full), res1)
  #add a horizontal line at 0 
  abline(0,0)
  title("Fitted vs. Res1 plot")
  par(mfrow = c(1, 1))
  
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  Levenes_test <- anova(lm(res1sq ~ df$DivGroup))
  Levenes_test
  # We already specified that the variances are unequal, so testing this again is probably redundant.
  
# pairwise emmeans, from the tutorial.
# This is not useful. The differences are log(DivNA) - log(Div2) etc., so the difference cannot be 
# backtransformed properly. 
  # emmeans::emmeans(ancova.full.log.uv, pairwise ~ DivGroup,
  #                  mode = "df.error" )
  
# emmeans. from YBC Fiber script. 
  full.log.uv.emmeans <- emmeans::emmeans(ancova.full.log.uv, "DivGroup", type="response",
                         mode = "df.error")
  summary(full.log.uv.emmeans)
  # each level has different SE, as expected.
  
  # Mean separation with letters assigned.
  # require("multcomp")
  multcomp::cld(full.log.uv.emmeans, alpha = 0.05, Letters = LETTERS)
  # mysep <- multcomp::cld(fiberiron.emm, alpha = 0.05, Letters = LETTERS)
  mysep_df <- as.data.frame(multcomp::cld(full.log.uv.emmeans, alpha = 0.05, Letters = LETTERS))
  mysep_df
  
  # But there is no pairwise difference... 
  
  # emmeans are back-transformed (if log-transformed). Check if they are close to the original.
  aggregate(df$BMXWAIST, list(df$DivGroup), FUN=mean) # Different, but close.
  aggregate(df$BMXWAIST, list(df$DivGroup), FUN=sd) # Different, but close.
  plot(    df$DivGroup, df$BMXWAIST)
  
  
    
# ---------------------------------------------------------------------------------------------------------------
# Tukey's HSD, base R doesn't work with complex models.
  TukeyHSD( ancova.full )
  # error says "non-factors ignored". So, it cannot have a numeric covariate, it seems.
  # With only numeric covariates, it runs.
  TukeyHSD( anova.simple )
  
  # how about lm.full?  
  TukeyHSD( lm.full )
  # error says "cannot apply TukeyHSD to lm object."
# Cannot use Tukey's HSD by base.R for a complex model with numeric covarirate(s). 
  
  
# ===============================================================================================================
# ancova.full. resp.var = KCAL
# ===============================================================================================================
  # Resp = KCAL.
  anova.simple.kcal     <- aov(lm( KCAL ~ DivGroup, data=df))  
  ancova.agegender.kcal <- aov(lm( KCAL ~ DivGroup + RIDAGEYR + Gender, data=df)) 
  ancova.full.kcal      <- aov(lm( KCAL ~ DivGroup + FIBE + RIDAGEYR + Gender + 
                                PF_TOTAL_LEG, data=df)) 
  # install.packages("car")
  library(car)
  car::Anova(anova.simple.kcal,     type="III")
  car::Anova(ancova.agegender.kcal, type="III")
  car::Anova(ancova.full.kcal,      type="III")

# pairwise emmeans, from the tutorial.
  # lm.full needs to be used, because emmeans only supports lm or glm as an input.
  lm.simple.kcal <-       lm( KCAL ~ DivGroup, data=df)
  emmeans::emmeans(lm.simple.kcal,        pairwise ~ DivGroup)  
  # emmeans similar to simple means.
  
  lm.agegender.kcal <-    lm( KCAL ~ DivGroup + RIDAGEYR + Gender , data=df)
  emmeans::emmeans(lm.agegender.kcal,     pairwise ~ DivGroup)  
  # emmeans similar to simple means.
  
  lm.agegender.f.kcal <-  lm( KCAL ~ DivGroup + RIDAGEYR + Gender + FIBE, data=df)
  emmeans::emmeans(lm.agegender.f.kcal,   pairwise ~ DivGroup)  
  # When I add FIBE in the model, the means change a lot!
  
  lm.agegender.p.kcal <-  lm( KCAL ~ DivGroup + RIDAGEYR + Gender + PF_TOTAL_LEG, data=df)
  emmeans::emmeans(lm.agegender.p.kcal,   pairwise ~ DivGroup)  
  # When I add PF_TOTAL_LEG in the model, the means are almost the same (2006-2037.)
  
  lm.agegender.p.kcal <-  lm( KCAL ~ DivGroup + RIDAGEYR + Gender + PF_TOTAL_LEG, data=df)
  emmeans::emmeans(lm.agegender.p.kcal,   pairwise ~ DivGroup)  
  # When I add PF_TOTAL_LEG in the model, the means are almost the same (2006-2037.)
  
  
  lm.full.kcal <-         lm( KCAL ~ DivGroup + RIDAGEYR + Gender + FIBE + PF_TOTAL_LEG, data=df)
  emmeans::emmeans(lm.full.kcal,          pairwise ~ DivGroup)  
  # When I add all the terms to the model again, emmeans difference became large again. 
  # What I add to the model is super important, because it could change the means and separation results. 
  # How do I decide which term to include....? --> need to do lit search.
  # pairwise difference;  DivNA - Div2 = 379.9 KCAL, p<.0001. But this says DivNA has higher kcal than Div2, wrong. 
  
  aggregate(df$KCAL, list(df$DivGroup), FUN=mean)
  2192.276-1947.107=245.169.
  # simple means by group are this different.   
  plot(df$DivGroup, df$KCAL)
  boxplot(df$KCAL)
  
  # Check assumptions.  
  res1 <- residuals(ancova.full.kcal)
  res1 <- residuals(lm.agegender.p.kcal)
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
  plot(fitted(ancova.full.kcal), res1)
  #add a horizontal line at 0 
  abline(0,0)
  title("Fitted vs. Res1 plot")
  par(mfrow = c(1, 1))
  
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  Levenes_test <- anova(lm(res1sq ~ df$DivGroup))
  Levenes_test 
  # variance is not different among DivGroups.
  
  
    
# HSD
  hsd <- agricolae::HSD.test(lm.full.kcal, "DivGroup", group=TRUE, console=TRUE)
  hsd
  # compare means of DivNA and Div2.
  hsd$means['DivNA',1] - hsd$means['Div2',1]
  # -245.1695 kcal. So different from emmeans difference! 
  
  emmeans::emmip(lm.full.kcal, Gender ~ DivGroup  ) # DivGroup in x, gender in legend.
  emmeans::emmip(lm.simple.kcal, Gender ~ DivGroup  ) # DivGroup in x, gender in legend.
  
  emmeans <- emmeans::emmeans(lm.full.kcal, "DivGroup", type="response", mode="df.error")
  emmeans
  
  hsd.simple <- agricolae::HSD.test(lm.simple.kcal, "DivGroup", group=TRUE, console=TRUE)
  hsd.simple
  # hsd.simple and hsd(.full) .. the letter separation is different, but means and std are exactly the same.
  # that shouldn't be correct, because they are based on different models...?

  #  Use lsmeans.
  mylsmeans <- lsmeans::lsmeans(lm.full.kcal, "DivGroup", type="response", mode = "df.error")
  mylsmeans
  # similar to emmeans. DivNA has the highest. WHY??
  df$DivGroup
  
# emmens and hsd means are so different. emmeans do not seem to be correct. Not good...!
  
# ===============================================================================================================
# ancova.full. resp.var = FIBE
# ===============================================================================================================
  # Resp = KCAL.
  anova.simple.fibe     <- aov(lm( FIBE ~ DivGroup, data=df))  
  ancova.agegender.fibe <- aov(lm( FIBE ~ DivGroup + RIDAGEYR + Gender, data=df)) 
  ancova.full.fibe      <- aov(lm( FIBE ~ DivGroup + KCAL + RIDAGEYR + Gender + 
                                     PF_TOTAL_LEG, data=df)) 
  # install.packages("car")
  library(car)
  car::Anova(anova.simple.fibe,     type="III")
  car::Anova(ancova.agegender.fibe, type="III")
  car::Anova(ancova.full.fibe,      type="III")
  
  lm.full.fibe <-   lm( FIBE ~ DivGroup + RIDAGEYR + Gender + KCAL + PF_TOTAL_LEG, data=df)
  
  lm.simple.fibe <- lm( FIBE ~ DivGroup + RIDAGEYR + Gender, data=df)
  
  emmeans::emmeans(lm.full.fibe, pairwise ~ DivGroup) 
  # pairwise difference;  DivNA - Div2 = -10.57, p<.0001. 
  
  aggregate(df$FIBE, list(df$DivGroup), FUN=mean)
  # emmeans and simple means are kinda close...
  
  hsd <- agricolae::HSD.test(lm.full.fibe, "DivGroup", group=TRUE, console=TRUE)
  hsd
  # compare means of DivNA and Div2.
  hsd$means['DivNA',1] - hsd$means['Div2',1]
  # -12.07102. hmm, almost close to emmeans' difference.
  
  hsd.simple <- agricolae::HSD.test(lm.simple.fibe, "DivGroup", group=TRUE, console=TRUE)
  hsd.simple
  hsd.simple$means['DivNA',1] - hsd.simple$means['Div2',1]
  # -12.07102. same as the lm.full.fibe results.
  
  
  #
  
  
  #### OLD BELOW ####
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
  
  # Create a new variable of squared residuals.
  res1sq <- res1*res1
  # Run Levene's test (ANOVA for the squared residuals as the response).
  Levenes_test <- anova(lm(res1sq ~ df$DivGroup))
  Levenes_test
  
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
   