# ===============================================================================================================
# Function to do pairwise adonis. 
# Version 1
# Created on 10/20/2022 by Rie Sadohara
# ===============================================================================================================

# Source: https://www.researchgate.net/post/How_can_I_do_PerMANOVA_pairwise_contrasts_in_R
# FYI https://rdrr.io/github/Jtrachsel/funfuns/man/pairwise.adonis.html
# ---------------------------------------------------------------------------------------------------------------
# x = the community table
# factors = a column or vector with all factors to be tested pairwise
# sim.method = similarity function, one of the functions available in vegdist(); default is 'bray' for bray-curtis
# ?vegdist()
# p.adjust.m = the p.value correction method, one of the methods supported by p.adjust(); default is 'bonferroni'
# The function will return a table with the pairwise factors, F-values, R^2, p.value and adjusted p.value
# Regards Pedro

PairwiseAdonis <- function(x, factors, sim.method, p.adjust.m ='bonferroni')
{
  library(vegan)
  co = combn(unique(factors),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  for(elem in 1:ncol(co)){
    ad = adonis(x[factors %in% c(co[1,elem],co[2,elem]),] ~ factors[factors %in% c(co[1,elem],co[2,elem])] , method =sim.method);
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}


# ---------------------------------------------------------------------------------------------------------------

# Filter demog_glu so that it will only contain the individuals in the target subpopulation, then 
# run adonis pairwise. 

PairwiseAdonisNHANES <- function(food.otu.table, demog, factor, sim.method, p.adjust.m ='bonferroni'){
  
  # Remove the last column (taxonomy) from food.otu.table.
  foodotu <- food.otu.table[, -ncol(food.otu.table)]
  
  # Transform so that samples (individuals) will be rows.
  foodotu_t <- as.data.frame(t(foodotu))
  # This has xSEQN as row names.
  
  # Create a vector that contains the individuals ("XSEQN") 
  individuals_v <- colnames(foodotu)
  
  # Need to remove the "X" prefix. - remove the first character from individuals_v.
  individuals_v_num <- sub('.', '', individuals_v)
  
  # Get TRUE or FALSE for all the rows of df.
  indTF <- demog[, 'SEQN'] %in% individuals_v_num
  
  # Pick up only those with TRUE (rows with SEQN in individuals_num)
  demog_sel <- demog[indTF, ]
  
  # Use the PairwiseAdonis defined above for the variable of interest.
  PairwiseAdonis(x = foodotu_t, factors = demog_sel[, factor] )
  
}

# Trial and error below
# # food is an OTU table with taxonomy at the end..
# colnames(food)
# foodotu <- food[, -281]
# colnames(foodotu)
# head(foodotu,1)
# 
# foodotu_t <- as.data.frame(t(foodotu))
# head(foodotu_t[1, ],1)
# # This has xSEQN as row names.
# 
# colnames(demog_glu)
# head(demog_glu, 1)
# # This has xSEQN as row names.
# dim(demog_glu)
# # This has all the 1943 people.
# # Take only the indiviuals in the OTU table (men in 60plus).
# 
# individuals <- colnames(foodotu)
# # Need to remove the "X" prefix. - remove the first character.
# individuals_num <- sub('.', '', individuals)
# 
# # Get TRUE or FALSE for all the rows of df.
# indTF <- demog_glu[, 'SEQN'] %in% individuals_num
# table(indTF)
# # Pick up only those with TRUE (rows with SEQN in individuals_num)
# demog_glu_sel <- demog_glu[indTF, ]
# head(demog_glu_sel,1)
# dim(demog_glu_sel)
# 
# co = combn(unique(demog_glu$GLU_index),2)
# pairs = c()
# F.Model =c()
# R2 = c()
# p.value = c()
# 
# PairwiseAdonis(x = foodotu_t, factors = demog_glu_sel$GLU_index )
# PairwiseAdonis(x = foodotu_t, factors = c("Normal", "Prediabetic" ) )
# 
# 
# factors = demog_glu_sel$GLU_index
# ad3 = adonis( foodotu_t[factors %in% c(co[1,3],co[2,3]),] ~ factors[factors %in% c(co[1,3],co[2,3])] , method ="bray");
# ad3 = adonis( foodotu_t[factors %in% c("Normal", "Prediabetic" ), ] ~ factors[factors %in% c("Normal", "Prediabetic" )] , method ="bray");
# 
# head(foodotu)
# 
# n_p <- vegan::adonis()
# 
# 
#   
