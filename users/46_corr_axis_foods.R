# ===============================================================================================================
# Generate a heatmap of correlation between food categories and ordination Axes.  
# Version 1
# Created on 10/28/2022 by Rie Sadohara
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
  source("lib/ggplot2themes.R")
  source("lib/create_corr_frame.R") 

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# You can come back to the main directory by:
  setwd(main_wd)

# Set working directory.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data/Ordination/")

# ===============================================================================================================
# Load ordination data
# ===============================================================================================================

# Run this code below after making your phyloseq object and doing ordination. 

  
  
# SEQN-sorted food
  dim(food) # 1274 foods x {237 people + 1 taxonomy columns}
  food[1:3, 1:3]
  
# samples (individuals) needs to be the rownames. So, transform it.
# Remove the taxonomy column from 'food'.
  food2 <- food[, !colnames(food) == "taxonomy"] 
  colnames(food2)
  dim(food2) # 1274 x 237

# Transpose food so that rows will be the SEQN (for which a distance matrix is calculated)
  food3 <- as.data.frame(t(food2))
  head(colnames(food3))    # columns have foods
  head(rownames(food3))    # rows have individuals
  dim(food3) # 237 x 1274

# Sort individuals (rows) in order. **IMPORTANT!** 
  food3_s <- food3[order(rownames(food3)), ]
  head(rownames(food3_s), 10)       # individuals should be in order.
  dim(food3_s) # 237 people x 1274 foods

# food3_s is x. 

# WEIGHTED
  # Axis values of each individual in ordinated_w
    ordinated_w_vec <- ordinated_w$vectors
    head(ordinated_w_vec) # it is a matrix...
    dim(ordinated_w_vec) # 237 x 153. Only 153 Axes were saved, even though there were 237 people (samples)??
    
  # ordinated_w_vec is y.

# UNweighted
  # Axis values of each individual in ordinated_u
    ordinated_u_vec <- ordinated_u$vectors 
    head(ordinated_u_vec)
    dim(ordinated_u_vec) # 237 x 236 
  # ordinated_u_vec is y.
  
# ---------------------------------------------------------------------------------------------------------------
# script from Abby to generate correlation matrix.

# correlate them with each other
# x <- as.data.frame(t(foodgroups_s))
  x <- as.data.frame(food3_s)
  dim(x)
  x[1:3, 1:3]

# y <- as.data.frame(ffq)  # food group values.
  y <- as.data.frame(ordinated_w_vec)  # food group values.
  y <- as.data.frame(ordinated_u_vec)  # food group values.
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
  colnames(dat)[1:2]<-c("Food","Axis")

# Mark rows that have qvalues < 0.25 with an asterisk in a column called "Significance".  
  dat$Significance <- cut(dat$qval, breaks=c(-Inf, 0.25, Inf), label=c("*", "")) 
  head(dat)
  table(dat$Significance)

  # Careful which one to choose!
    # WEIGHTED
    write.table(dat, "Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv_ord_WEIGHTED_corr_axes_foods.txt",
                sep="\t", row.names=F, quote=F)  
  
    # UNweighted
    write.table(dat, "Food_D12_FC_QC_demo_QCed_males60to79_red_3Lv_ord_UNweighted_corr_axes_foods.txt",
                sep="\t", row.names=F, quote=F)  
  
# See each axes 
# Select Axis 1 rows   
  dat_1 <- subset(dat, Axis=="Axis.1")
  head(dat_1[order(dat_1$qval), ], 10)
  
# Select Axis 2 rows and sort by qval.  
  dat_2 <- subset(dat, Axis=="Axis.2")
  head(dat_2[order(dat_2$qval), ], 10)
  
# Select Axis 3 rows and sort by qval.  
  dat_3 <- subset(dat, Axis=="Axis.3")
  head(dat_3[order(dat_3$qval), ], 15)
  
# Select Axis 4 rows and sort by qval.  
  dat_4 <- subset(dat, Axis=="Axis.4")
  head(dat_4[order(dat_4$qval), ], 15)
  
# Select significant food items.
  dat_sig <- subset(dat, Significance=="*")
  
  head(dat_sig[order(dat_sig$qval), ], 10)
  
# weighted unifrac distances are heavily influenced by liquid intake...
# Look at unweighted unifrac distances.
  

  
#####################################################################################################################
#### Code to make heatmap??  
  
# prune for complete cases with significant correlations
  dat.c <- dat[complete.cases(dat),]
  dim(dat.c)
  
  max(dat.c$qval)
  
# Select Foods that have qval <= 1.
  taxkeep <- unique(dat.c$Food[dat.c$qval<=1])
  length(taxkeep)

  taxassign <- taxkeep    # which things to keep
  dat.w <- dat.c[dat.c$Food %in% taxassign,]

  ffqkeep <- unique(dat.w$FFQ[dat.w$qval<=1])
  dat.m <- dat.w[dat.w$FFQ %in% ffqkeep,]
  head(dat.m)

# make a factor before ordering
  dat.m$FFQ <- as.factor(dat.m$FFQ)
# ?remove_rownames

# order the FFQ categories  # order PC1, 3, 
# make wide taxa v. Kegg (as columns) filled with correlation
  order_FFQ <- dat.m %>% dplyr::select(Food, FFQ, correlation)
  order_FFQ <- order_FFQ %>% tidyr::spread(FFQ, correlation)
  order_FFQ <- tibble::remove_rownames(order_FFQ)
  order_FFQ <- tibble::column_to_rownames(order_FFQ, "Food")
  head(order_FFQ)

  ffqorder <- hclust((dist(1-cor(order_FFQ))/2))$order
  head(ffqorder)

# reorder the factors in the dataframe to be in the taxaorder
  dat.m$FFQ <- factor(dat.m$FFQ, levels(dat.m$FFQ)[ffqorder])

# Change the food names before ordering
  dat.m$Food <- gsub(".*L3_", "", dat.m$Food)
  dat.m$Food <- gsub("_", " ", dat.m$Food)

# make factor before ordering
  dat.m$Food <- as.factor(dat.m$Food)

# make wide kegg v. taxa (as columns) filled with correlation
  order_food <- dat.m %>% dplyr::select(Food, FFQ, correlation)
  order_food <- order_food %>% tidyr::spread(Food, correlation)
  order_food <- tibble::remove_rownames(order_food)
  order_food <- tibble::column_to_rownames(order_food, "FFQ")
  foodorder  <- hclust((dist(1-cor(order_food))/2))$order

# reorder the factors in the dataframe to be in the taxaorder
  dat.m$Food <- factor(dat.m$Food, levels(dat.m$Food)[foodorder])
  length(unique(dat.m$Food))
  head(dat.m)

# ---------------------------------------------------------------------------------------------------------------
library(dplyr)
####### Look at the correlation coeff with Food and Axis1 or Axis2
dat.m_Axis1 <- dat.m %>% filter(FFQ =="Axis.1" ) 
dat.m_Axis2 <- dat.m %>% filter(FFQ =="Axis.2" ) 
dat.m_Axis3 <- dat.m %>% filter(FFQ =="Axis.3" ) 
dat.m_Axis4 <- dat.m %>% filter(FFQ =="Axis.4" ) 
identical(dat.m_Axis1$Food, dat.m_Axis2$Food)

head(dat.m_Axis1 %>% arrange(pval), 20)
head(dat.m_Axis2 %>% arrange(pval), 10)
head(dat.m_Axis3 %>% arrange(pval), 16)
head(dat.m_Axis4 %>% arrange(pval), 16)

tail(dat.m_Axis2 %>% arrange(pval), 16)
hist(dat.m_Axis4$correlation)


# Merge Axis1 and Axis2.
dat.m_Axis12 <- inner_join(dat.m_Axis1, dat.m_Axis2, by="Food")
head(dat.m_Axis12)

head(dat.m_Axis12)

# write.table(dat.m_Axis12, "dat.m_Axis12.txt", sep="\t", row.names=F, quote=F)

# Make food3_s_GLU for plotting people by their food consumption.
  head(food3_s[1:2, 1:2] )
  # Create a column of XSEQN for merging.
  food3_s$XSEQN <- rownames(food3_s)
  head(food3_s$XSEQN) 
    
  # Add GLU_index info to this food OTU table.
  head(demog_glu) 
  # Create a column of XSEQN for merging.
  demog_glu$XSEQN <- rownames(demog_glu)
  head(demog_glu$XSEQN)
  
  food3_s_glu <- left_join(food3_s, demog_glu[, c("XSEQN", "GLU_index")], by="XSEQN")
  head(food3_s_glu[1:2, 1274:1276]) 
  
  # food3_s_glu
  food3_s_glu$GLU_index <- factor(food3_s_glu$GLU_index, levels=c("Normal", "Prediabetic", "Diabetic"))
  table(food3_s_glu$GLU_index)

# Look at histogram of ice cream consumption.
  hist(food3_s_glu$`Milk cows fluid whole`)


# Histogram (that looks like a barplot) of a particular food item consumption.
  icecream_GLU <- ggplot(food3_s_glu) + 
    geom_histogram(aes(x = `Tea chamomile`, 
                 fill = GLU_index), 
             position = position_dodge(preserve = 'single')) +
    scale_fill_manual(values=c("steelblue3", "gold3", "hotpink"))
  icecream_GLU
  ggsave("Food_D12_FC_cc_f_males60to79_red_Lv3_ord_WEIGHTED_corr_icecream_GLU.png", 
         icecream_GLU, device="png", width=10, height=5.5, unit="in", dpi=300)
  
# Barplot of Milk cows fluid 2 fat consumption ------------------------
# Look at histogram.
  hist(food3_s_glu$`Milk cows fluid 2 fat`)
  
  df <- data.frame(Milk_2_prc = food3_s_glu$`Milk cows fluid 2 fat`,
                   SEQN =       food3_s_glu$XSEQN,
                   GLU_index =  food3_s_glu$GLU_index)
  
  milk2prc_GLU <- ggplot(data= df, aes(x=GLU_index, y=Milk_2_prc, fill=GLU_index)) +
    geom_boxplot(outlier.shape = NA) + no_grid + space_axes +
    scale_fill_manual(values=c("steelblue3", "gold3", "hotpink")) +
    # scale_fill_manual(values= c("aquamarine2", "lightgoldenrod1", "lightpink1") ) +
    geom_jitter(width=0.2, alpha=0.3)
  milk2prc_GLU
  
  ggsave("Food_D12_FC_cc_f_males60to79_red_Lv3_ord_WEIGHTED_corr_milk2prc_GLU.png", 
         milk2prc_GLU, device="png", width=5.3, height=4.5, unit="in", dpi=300)
  
# Barplot of Milk cows fluid 2 fat consumption ---------------------
  # Look at histogram.
  hist(food3_s_glu$`Milk cows fluid whole`)
  
  df <- data.frame(Milk_whole = food3_s_glu$`Milk cows fluid whole`,
                   SEQN = food3_s_glu$XSEQN,
                   GLU_index = food3_s_glu$GLU_index)
  
  milkwhole_GLU <- ggplot(data= df, aes(x=GLU_index, y=Milk_whole)) +
    geom_boxplot(outlier.shape = NA) + no_grid + space_axes +
    scale_fill_manual(values=c("steelblue3", "gold3", "hotpink")) +
    # scale_fill_manual(values= c("aquamarine2", "lightgoldenrod1", "lightpink1") ) +
    geom_jitter(width=0.2, alpha=0.3)
  milkwhole_GLU
  
  ggsave("Food_D12_FC_cc_f_males60to79_red_Lv3_ord_WEIGHTED_corr_milkwhole_GLU.png", 
         milkwhole_GLU, device="png", width=5.3, height=4.5, unit="in", dpi=300)
  
# ---------------------------------------------------------------------------------------------------------------
# Calculate the total consumption by colSums and compare it with correlation coeff?
  food3_s_glu[1:2, 1:2] 
  
  sums <- as.data.frame(colSums(food3_s_glu[, 1:1274]))
  head(sums)
  sums$Food <- rownames(sums)
  dim(sums)
  
  head(dat.m_Axis1)
  dim(dat.m_Axis1)
  
  dat.m_Axis1_sums <- merge(x=dat.m_Axis1, sums, by="Food") 
  head(dat.m_Axis1_sums)
  
  dat.m_Axis1_sums$abs_corr <- abs(dat.m_Axis1_sums$correlation)
  
  plot(dat.m_Axis1_sums$`colSums(food3_s_glu[, 1:1274])`, dat.m_Axis1_sums$abs_cor)
  plot(dat.m_Axis1_sums$`colSums(food3_s_glu[, 1:1274])`, dat.m_Axis1_sums$pval  )
  
  
  
  
####### BACK TO ABBY'S CODE
# ---------------------------------------------------------------------------------------------------------------

#plot the correlations for the collapsed levels
ffq_v_recall <- ggplot(data = dat.m, aes(x=FFQ, y=Food, fill=correlation)) +
  geom_tile(color = "white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradient2(low="#5e3c99", mid="#f7f7f7", high="#e66101", midpoint = 0, limit = c(-1,1), space = "Lab", name = "Spearman") +
  geom_text(data = dat.m, aes(label=Significance), color="black", size=1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 1, hjust = 1),
        axis.text.y = element_text(size = 1)) +
  labs(x = "FFQ HEI Variables", y = "24-hour Recall Food Groups")
ffq_v_recall

ggsave("Food_D12_FC_cc_f_males60to79_red_Lv3_ord_WEIGHTED_corr_heat.pdf", ffq_v_recall, device = "pdf") #, width = 6, height = 4, dpi = 300)







# ---------------------------------------------------------------------------------------------------------------
# ORIGINAL --- My notes on this code I took during meeting with Abby 

# correlate them with each other
x <- as.data.frame(t(foodgroups_s))
y <- as.data.frame(ffq_s)  # food group values.

# make sure the samples are the same
x <- x[rownames(x) %in% rownames(y),]
y <- y[rownames(y) %in% rownames(x),]

# Now want to correlate each of the columns in x with columns in y
dat <- create_corr_frame(x, y)
colnames(dat)[1:2]<-c("Food","FFQ")
dat$Significance<-cut(dat$qval, breaks=c(-Inf, 0.25, Inf), label=c("*", "")) #converts numeric p-values to factors based on significance level

# prune for complete cases with significant correlations
dat.c <- dat[complete.cases(dat),]
taxkeep <- unique(dat.c$Food[dat.c$qval<=1])

taxassign <- taxkeep    # which things to keep
dat.w <- dat.c[dat.c$Food %in% taxassign,]

ffqkeep <- unique(dat.w$FFQ[dat.w$qval<=1])
dat.m <- dat.w[dat.w$FFQ %in% ffqkeep,]

#make a factor before ordering
dat.m$FFQ <- as.factor(dat.m$FFQ)

# order the FFQ categories  # order PC1, 3, 
# make wide taxa v. Kegg (as columns) filled with correlation
order_FFQ <- dat.m %>% select(Food, FFQ, correlation)
order_FFQ <- order_FFQ %>% spread(FFQ, correlation)
order_FFQ <- remove_rownames(order_FFQ)
order_FFQ <- column_to_rownames(order_FFQ, "Food")
ffqorder <- hclust((dist(1-cor(order_FFQ))/2))$order

# reorder the factors in the dataframe to be in the taxaorder
dat.m$FFQ <- factor(dat.m$FFQ, levels(dat.m$FFQ)[ffqorder])

# Change the food names before ordering
dat.m$Food <- gsub(".*L3_", "", dat.m$Food)
dat.m$Food <- gsub("_", " ", dat.m$Food)

# make factor before ordering
dat.m$Food <- as.factor(dat.m$Food)

# make wide kegg v. taxa (as columns) filled with correlation
order_food <- dat.m %>% select(Food, FFQ, correlation)
order_food <- order_food %>% spread(Food, correlation)
order_food <- remove_rownames(order_food)
order_food <- column_to_rownames(order_food, "FFQ")
foodorder <- hclust((dist(1-cor(order_food))/2))$order

# reorder the factors in the dataframe to be in the taxaorder
dat.m$Food <- factor(dat.m$Food, levels(dat.m$Food)[foodorder])



#plot the correlations for the collapsed levels
ffq_v_recall <- ggplot(data = dat.m, aes(x=FFQ, y=Food, fill=correlation)) +
  geom_tile(color = "white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradient2(low="#5e3c99", mid="#f7f7f7", high="#e66101", midpoint = 0, limit = c(-1,1), space = "Lab", name = "Spearman") +
  geom_text(data = dat.m, aes(label=Significance), color="black", size=3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 9, hjust = 1),
        axis.text.y = element_text(size = 9)) +
  labs(x = "FFQ HEI Variables", y = "24-hour Recall Food Groups")


ggsave("Publication_figs/ffq_v_recall.pdf", ffq_v_recall, device = "pdf", width = 6, height = 4, dpi = 300)

```


