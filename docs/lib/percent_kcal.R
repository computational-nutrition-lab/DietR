# ====================================================================================================================
# Analyze the kcal from carbohydrate, protein, and fat in the totals file of  
# ASA24 output (data) 
# Version 1
# Created on 12.16.2021 by Rie Sadohara
# ====================================================================================================================

# ====================================================================================================================
# Function to add gender and age groups to NHANES totals data.
# ====================================================================================================================

AddGenderAgeGroups <- function(input=totals, age.col="RIDAGEYR", gender.col="RIAGENDR"){
  
  # Rename input. 
  totals2 <- input
  
  # column number of gender 
  gender.col.number <- which(colnames(totals2)==gender.col)
  
  # Add a new column of Gender.
  totals2$Gender = NA
  
  # Add gender index to totals.
  for(i in 1:nrow(totals2)){
    if(     totals2[i, gender.col.number]==1){totals2$Gender[i] <- "M"}
    else if(totals2[i, gender.col.number]==2){totals2$Gender[i] <- "F"}
  }  
  
  # column number of age 
  age.col.number <- which(colnames(totals2)==age.col)
  
  # Add a new column of Age group.
  totals2$AgeGroup = NA
  
  # Add age group. 
  for(i in 1:nrow(totals2)){
    if(     totals2[i, age.col.number] < 20){totals2$AgeGroup[i] <- "18_19"}
    else if(totals2[i, age.col.number] < 30){totals2$AgeGroup[i] <- "20s"}
    else if(totals2[i, age.col.number] < 40){totals2$AgeGroup[i] <- "30s"}
    else if(totals2[i, age.col.number] < 50){totals2$AgeGroup[i] <- "40s"}
    else if(totals2[i, age.col.number] < 60){totals2$AgeGroup[i] <- "50s"}
    else if(totals2[i, age.col.number] < 70){totals2$AgeGroup[i] <- "60s"}
    else                                    {totals2$AgeGroup[i] <- "70s_80s"}
  }
  
  # Combine Age_Group and Gender as a new factor. e.g. "M_40s".
  totals2$Gender_Age <- paste(totals2$Gender, totals2$AgeGroup, sep="_")
  
  # Name the output and make it usable outside the function. 
  totals_out <<- totals2
  
}

# ====================================================================================================================
# Function to calculate the mean and SD of CARB, PROT, and TFAT. with UserName in ASA24.
# ====================================================================================================================

  CPTgramsPerUser <- function(inputfn, user.name='UserName', 
                              recall.no='RecallNo', outfn){
    
    # Get index numbers for username, recallno, "CARB","PROT","TFAT", "KCAL"
    indexno_username <- which(names(inputfn)== user.name)
    indexno_recallno <- which(names(inputfn)== recall.no)
    indexno_carb <-     which(names(inputfn)== "CARB")
    indexno_prot <-     which(names(inputfn)== "PROT")
    indexno_tfat <-     which(names(inputfn)== "TFAT")
    
    # Take only the relevant columns from inputfn.
    totalssub <- inputfn[, c(indexno_username, 
                             indexno_recallno, 
                             indexno_carb,
                             indexno_prot,
                             indexno_tfat)]
    
    # Change the column names for the following process
    colnames(totalssub)[1:2] <- c("UserName", "RecallNo")
    
    # Calc grams of macronutrients.
    CARBmean <- aggregate(totalssub$CARB, by = list(totalssub$UserName), FUN = mean)
    colnames(CARBmean) <- c("UserName", "CARB_mean")
    CARBsd <- aggregate(totalssub$CARB, by = list(totalssub$UserName), FUN = sd)
    colnames(CARBsd) <- c("UserName", "CARB_sd")
    CARBlength <- aggregate(totalssub$CARB, by = list(totalssub$UserName), FUN = length)
    colnames(CARBlength) <- c("UserName", "CARB_n")
    C_length_mean <-   merge(x=CARBlength, y=CARBmean, all.x=T)
    C_length_mean_sd <- merge(x=C_length_mean, y=CARBsd, all.x=T)
    C_length_mean_sd$macronutrient <- "CARB"
    
    PROTmean <- aggregate(totalssub$PROT, by = list(totalssub$UserName), FUN = mean)
    colnames(PROTmean) <- c("UserName", "PROT_mean")
    PROTsd <-   aggregate(totalssub$PROT, by = list(totalssub$UserName), FUN = sd)
    colnames(PROTsd) <- c("UserName", "PROT_sd")
    PROTlength <- aggregate(totalssub$PROT, by = list(totalssub$UserName), FUN = length)
    colnames(PROTlength) <- c("UserName", "PROT_n")
    P_length_mean <-   merge(x=PROTlength, y=PROTmean, all.x=T)
    P_length_mean_sd <- merge(x=P_length_mean, y=PROTsd, all.x=T)
    P_length_mean_sd$macronutrient <- "PROT"
    
    TFATmean <- aggregate(totalssub$TFAT, by = list(totalssub$UserName), FUN = mean)
    colnames(TFATmean) <- c("UserName", "TFAT_mean")
    TFATsd <- aggregate(totalssub$TFAT, by = list(totalssub$UserName), FUN = sd)
    colnames(TFATsd) <- c("UserName", "TFAT_sd")
    TFATlength <- aggregate(totalssub$TFAT, by = list(totalssub$UserName), FUN = length)
    colnames(TFATlength) <- c("UserName", "TFAT_n")
    T_length_mean <-    merge(x=TFATlength, y=TFATmean, all.x=T)
    T_length_mean_sd <- merge(x=T_length_mean, y=TFATsd, all.x=T)
    T_length_mean_sd$macronutrient <- "TFAT"
    T_length_mean_sd
    
    # Change column names for rbind
    colnames(C_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    colnames(P_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    colnames(T_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    
    rbound <- rbind(C_length_mean_sd, P_length_mean_sd, T_length_mean_sd)
    CPT_g_fn <- rbound[, c(1,5,2,3,4)] # bring macronutrient to 2nd
    
    # Save CPT_g_fn. (fn stands for "function")
    write.table(x=CPT_g_fn, file=outfn, sep="\t", row.names=F, quote=F)
  }


# ====================================================================================================================
# Function to calculate the mean % of energy intake (kcal) and SD of CARB, PROT, and TFAT.
# Use KCAL as the denominator. with UserName in ASA24.
# ====================================================================================================================
  CPTpctKcalPerUser <- function(inputfn, user.name='UserName', 
                                recall.no='RecallNo', outfn){
    
    # Get index numbers for username, recallno, "CARB","PROT","TFAT", "KCAL"
    indexno_username <- which(names(inputfn)== user.name)
    indexno_recallno <- which(names(inputfn)== recall.no)
    indexno_carb <-     which(names(inputfn)== "CARB")
    indexno_prot <-     which(names(inputfn)== "PROT")
    indexno_tfat <-     which(names(inputfn)== "TFAT")
    indexno_kcal <-     which(names(inputfn)== "KCAL")
    
    # Take only the relevant columns from inputfn.
    totalssub2 <- inputfn[, c(indexno_username, 
                              indexno_recallno, 
                              indexno_carb,
                              indexno_prot,
                              indexno_tfat,
                              indexno_kcal)]
    
    # Change the column names for the following process
    colnames(totalssub2)[1:2] <- c("UserName", "RecallNo")
    
    # % KCAL
    # calculate calories
    totalssub2$CARB_kcal <- totalssub2$CARB * 4
    totalssub2$PROT_kcal <- totalssub2$PROT * 4
    totalssub2$TFAT_kcal <- totalssub2$TFAT * 9
    
    # calculate kcal of each macronutrient per engergy (%)
    totalssub2$CARB_kcal_pct <- totalssub2$CARB_kcal / totalssub2$KCAL * 100
    totalssub2$PROT_kcal_pct <- totalssub2$PROT_kcal / totalssub2$KCAL * 100
    totalssub2$TFAT_kcal_pct <- totalssub2$TFAT_kcal / totalssub2$KCAL * 100
    
    CARB_kcal_pctmean <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$UserName), FUN = mean)
    colnames(CARB_kcal_pctmean) <- c("UserName", "CARB_kcal_pct_mean")
    CARB_kcal_pctsd <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$UserName), FUN = sd)
    colnames(CARB_kcal_pctsd) <- c("UserName", "CARB_kcal_pct_sd")
    CARB_kcal_pctlength <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$UserName), FUN = length)
    colnames(CARB_kcal_pctlength) <- c("UserName", "CARB_kcal_pct_n")
    C_length_mean <-   merge(x=CARB_kcal_pctlength, y=CARB_kcal_pctmean, all.x=T)
    C_length_mean_sd <- merge(x=C_length_mean, y=CARB_kcal_pctsd, all.x=T)
    C_length_mean_sd$macronutrient <- "CARB_kcal_pct"
    
    PROT_kcal_pctmean <- aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$UserName), FUN = mean)
    colnames(PROT_kcal_pctmean) <- c("UserName", "PROT_kcal_pct_mean")
    PROT_kcal_pctsd <-   aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$UserName), FUN = sd)
    colnames(PROT_kcal_pctsd) <- c("UserName", "PROT_kcal_pct_sd")
    PROT_kcal_pctlength <- aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$UserName), FUN = length)
    colnames(PROT_kcal_pctlength) <- c("UserName", "PROT_kcal_pct_n")
    P_length_mean <-   merge(x=PROT_kcal_pctlength, y=PROT_kcal_pctmean, all.x=T)
    P_length_mean_sd <- merge(x=P_length_mean, y=PROT_kcal_pctsd, all.x=T)
    P_length_mean_sd$macronutrient <- "PROT_kcal_pct"
    
    TFAT_kcal_pctmean <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$UserName), FUN = mean)
    colnames(TFAT_kcal_pctmean) <- c("UserName", "TFAT_kcal_pct_mean")
    TFAT_kcal_pctsd <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$UserName), FUN = sd)
    colnames(TFAT_kcal_pctsd) <- c("UserName", "TFAT_kcal_pct_sd")
    TFAT_kcal_pctlength <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$UserName), FUN = length)
    colnames(TFAT_kcal_pctlength) <- c("UserName", "TFAT_kcal_pct_n")
    T_length_mean <-    merge(x=TFAT_kcal_pctlength, y=TFAT_kcal_pctmean, all.x=T)
    T_length_mean_sd <- merge(x=T_length_mean, y=TFAT_kcal_pctsd, all.x=T)
    T_length_mean_sd$macronutrient <- "TFAT_kcal_pct"
    
    # Change column names for rbind 
    colnames(C_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    colnames(P_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    colnames(T_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    
    rbound <- rbind(C_length_mean_sd, P_length_mean_sd, T_length_mean_sd)
    CPT_kcal_fn <- rbound[, c(1,5,2,3,4)] # bring macronutrient to 2nd
    
    # Save CPT_kcal_fn. (fn stands for "function")
    write.table(x=CPT_kcal_fn, file=outfn, sep="\t", row.names=F, quote=F)
    
  }

# ====================================================================================================================
# Function to calculate the mean % of energy intake (kcal) and SD of CARB, PROT, and TFAT.
# Use the sum of XXXX_pct as the denominator. For NHANES.
# ====================================================================================================================
  CPTpctKcalPerUser_NHANES <- function(inputfn, group='Group', 
                                across='SEQN', outfn){
    
    # Get index numbers for Group, recallno, "CARB","PROT","TFAT", "KCAL"
    indexno_group <- which(names(inputfn)== group)
    indexno_across <- which(names(inputfn)== across)
    indexno_carb <-     which(names(inputfn)== "CARB")
    indexno_prot <-     which(names(inputfn)== "PROT")
    indexno_tfat <-     which(names(inputfn)== "TFAT")
    # indexno_kcal <-     which(names(inputfn)== "KCAL")
    
    # Take only the relevant columns from inputfn.
    totalssub2 <- inputfn[, c(indexno_group, 
                              indexno_across, 
                              indexno_carb,
                              indexno_prot,
                              indexno_tfat
                              # indexno_kcal
                              )]
    
    # Change the column names for the following process
    colnames(totalssub2)[1:2] <- c("Group", "SEQN")
    
    # % KCAL
    # calculate calories
    totalssub2$CARB_kcal <- totalssub2$CARB * 4
    totalssub2$PROT_kcal <- totalssub2$PROT * 4
    totalssub2$TFAT_kcal <- totalssub2$TFAT * 9
    totalssub2$kcal_sum <-  totalssub2$CARB_kcal + totalssub2$PROT_kcal + totalssub2$TFAT_kcal
    
    # calculate kcal of each macronutrient per engergy (%)
    totalssub2$CARB_kcal_pct <- totalssub2$CARB_kcal / totalssub2$kcal_sum * 100
    totalssub2$PROT_kcal_pct <- totalssub2$PROT_kcal / totalssub2$kcal_sum * 100
    totalssub2$TFAT_kcal_pct <- totalssub2$TFAT_kcal / totalssub2$kcal_sum * 100
    
    CARB_kcal_pctmean <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$Group), FUN = mean)
    colnames(CARB_kcal_pctmean) <- c("Group", "CARB_kcal_pct_mean")
    CARB_kcal_pctsd <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$Group), FUN = sd)
    colnames(CARB_kcal_pctsd) <- c("Group", "CARB_kcal_pct_sd")
    CARB_kcal_pctlength <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$Group), FUN = length)
    colnames(CARB_kcal_pctlength) <- c("Group", "CARB_kcal_pct_n")
    C_length_mean <-   merge(x=CARB_kcal_pctlength, y=CARB_kcal_pctmean, all.x=T)
    C_length_mean_sd <- merge(x=C_length_mean, y=CARB_kcal_pctsd, all.x=T)
    C_length_mean_sd$macronutrient <- "CARB_kcal_pct"
    
    PROT_kcal_pctmean <- aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$Group), FUN = mean)
    colnames(PROT_kcal_pctmean) <- c("Group", "PROT_kcal_pct_mean")
    PROT_kcal_pctsd <-   aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$Group), FUN = sd)
    colnames(PROT_kcal_pctsd) <- c("Group", "PROT_kcal_pct_sd")
    PROT_kcal_pctlength <- aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$Group), FUN = length)
    colnames(PROT_kcal_pctlength) <- c("Group", "PROT_kcal_pct_n")
    P_length_mean <-   merge(x=PROT_kcal_pctlength, y=PROT_kcal_pctmean, all.x=T)
    P_length_mean_sd <- merge(x=P_length_mean, y=PROT_kcal_pctsd, all.x=T)
    P_length_mean_sd$macronutrient <- "PROT_kcal_pct"
    
    TFAT_kcal_pctmean <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$Group), FUN = mean)
    colnames(TFAT_kcal_pctmean) <- c("Group", "TFAT_kcal_pct_mean")
    TFAT_kcal_pctsd <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$Group), FUN = sd)
    colnames(TFAT_kcal_pctsd) <- c("Group", "TFAT_kcal_pct_sd")
    TFAT_kcal_pctlength <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$Group), FUN = length)
    colnames(TFAT_kcal_pctlength) <- c("Group", "TFAT_kcal_pct_n")
    T_length_mean <-    merge(x=TFAT_kcal_pctlength, y=TFAT_kcal_pctmean, all.x=T)
    T_length_mean_sd <- merge(x=T_length_mean, y=TFAT_kcal_pctsd, all.x=T)
    T_length_mean_sd$macronutrient <- "TFAT_kcal_pct"
    
    # Change column names for rbind 
    colnames(C_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    colnames(P_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    colnames(T_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    
    rbound <- rbind(C_length_mean_sd, P_length_mean_sd, T_length_mean_sd)
    CPT_kcal_fn <- rbound[, c(1,5,2,3,4)] # bring macronutrient to 2nd
    
    # Save CPT_kcal_fn. (fn stands for "function")
    write.table(x=CPT_kcal_fn, file=outfn, sep="\t", row.names=F, quote=F)
    
  }

# ====================================================================================================================
# Plot a stacked barchart without SD  ... with UserName of ASA24.
# ====================================================================================================================
  
  StackedwoSD <- function(data){
    ggplot(data, aes(x = UserName, y = mean, fill = macronutrient)) + 
      geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
      # change colors and labels of legend. Ensure the factor order is correct. 
      scale_fill_manual(values = distinct100colors, 
                        labels=c( "Carbohydrates", "Protein", "Total fat")) +
      labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
      # Specify the font size and angle of the x axis label.  
      theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) + no_grid
  }

# ====================================================================================================================
# Plot a stacked barchart without SD  ... with Group of NHANES.
# ====================================================================================================================
  
  StackedwoSD_NHANES <- function(data){
    ggplot(data, aes(x = Group, y = mean, fill = macronutrient)) + 
      geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
      # change colors and labels of legend. Ensure the factor order is correct. 
      scale_fill_manual(values = distinct100colors, 
                        labels=c( "Carbohydrates", "Protein", "Total fat")) +
      labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
      # Specify the font size and angle of the x axis label.  
      theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) + no_grid
  }
  
  
# ====================================================================================================================
# Plot individual bars for each macronutrients (3 bars in total) with SD ... with UserName of ASA24.
# ====================================================================================================================
  DodgedBarchart  <- function(data){
    ggplot(data, aes(x = factor(UserName), y = mean, fill = macronutrient, colour = macronutrient)) + 
      geom_bar(stat = "identity", position = "dodge", color="black")  +
      geom_errorbar(aes(ymin= mean, ymax= mean + sd), position = position_dodge(0.9), width = 0.25,
                    color="black") +
      scale_fill_manual(values = distinct100colors,
                        labels=c( "Carbohydrates", "Protein", "Total fat")) +
      labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
      no_grid + space_axes +
      theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  }
  
# ====================================================================================================================
# Plot individual bars for each macronutrients (3 bars in total) with SD ... NHANES. 
# ====================================================================================================================
  DodgedBarchart_NHANES <- function(data){
    ggplot(data, aes(x = factor(Group), y = mean, fill = macronutrient, colour = macronutrient)) + 
      geom_bar(stat = "identity", position = "dodge", color="black")  +
      geom_errorbar(aes(ymin= mean, ymax= mean + sd), position = position_dodge(0.9), width = 0.25,
                    color="black") +
      scale_fill_manual(values = distinct100colors,
                        labels=c( "Carbohydrates", "Protein", "Total fat")) +
      labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
      no_grid + space_axes +
      theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  }
  
# ====================================================================================================================
# Calculation for stacked barchart with SD ... with "UserName" of ASA24.
# ====================================================================================================================

# Function to calculate sd_base and sd_stacked for stacked barchart. 
# This assumes all users (individuals) have CARB, PROT, and TFAT values.

  CalcStackedSD <- function(input.df, out.fn){

    # Generate a dataframe to save sd data.
    CPT_kcal_forstacked <- data.frame(matrix(NA, nrow=length(individuals)*3, ncol=7)) 
    
    # Specify its column names.
    colnames(CPT_kcal_forstacked) <- c("UserName", "macronutrient", "n", "mean", "sd", "sd_base", "sd_stacked")
    
    for(i in 1:length(individuals)){
      
      if(i == 1){
        ith_user <- subset(input.df, UserName == individuals[i])
        
        # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
        PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
        TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
        CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
        PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
        TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
        
        # sd values for stacked barchart. 
        ith_user$sd_base <- c(TFATmeanval+PROTmeanval,  # carb, on top of the stacked barchart.
                              TFATmeanval,              # prot, in the middle.
                              0)                        # tfat, on the bottom.
        ith_user$sd_stacked <-  c(CARBsdval+PROTmeanval+TFATmeanval,    # carb, on top of the stacked barchart.
                                  PROTsdval+TFATmeanval,                # prot, in the middle.
                                  TFATsdval)                            # tfat, on the bottom.
        
        # for i=1, make the first result dataframe. 
        CPT_kcal_forstacked[c(i,i+1,i+2), ] <- ith_user
        
      }else{
        
        ith_user <- subset(input.df, UserName == individuals[i])
        
        # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
        PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
        TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
        CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
        PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
        TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
        
        # sd values for stacked barchart. 
        ith_user$sd_base <- c(TFATmeanval+PROTmeanval,  # carb, on top of the stacked barchart.
                              TFATmeanval,              # prot, in the middle.
                              0)                        # tfat, on the bottom.
        ith_user$sd_stacked <-  c(CARBsdval+PROTmeanval+TFATmeanval,    # carb, on top of the stacked barchart.
                                  PROTsdval+TFATmeanval,                # prot, in the middle.
                                  TFATsdval)                            # tfat, on the bottom.
        
        # need another value k in order to specify the correct row.
        k = i-2
        
        # for i = 2,3,4,..., combine rows with the previously made CPT_kcal_forstacked. 
        CPT_kcal_forstacked[c(i+i+k, i+i+k+1, i+i+k+2), ] <- ith_user
        
      }
    }
    # Save the resultant file as .txt file.
    write.table(x=CPT_kcal_forstacked, file = out.fn, sep="\t", row.names=F, quote=F)
    
  }

# ====================================================================================================================
# Calculation for stacked barchart with SD ... with "Group" of NHANES.
# ====================================================================================================================
  
  # Function to calculate sd_base and sd_stacked for stacked barchart. 
  # This assumes all groups (Gender_Age groups) have CARB, PROT, and TFAT values.
  
  CalcStackedSD_NHANES <- function(input.df, out.fn){
    
    # Generate a dataframe to save sd data.
    CPT_kcal_forstacked <- data.frame(matrix(NA, nrow=length(groups)*3, ncol=7)) 
    
    # Specify its column names.
    colnames(CPT_kcal_forstacked) <- c("Group", "macronutrient", "n", "mean", "sd", "sd_base", "sd_stacked")
    
    for(i in 1:length(groups)){
      
      if(i == 1){
        ith_user <- subset(input.df, Group == groups[i])
        
        # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
        PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
        TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
        CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
        PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
        TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
        
        # sd values for stacked barchart. 
        ith_user$sd_base <- c(TFATmeanval+PROTmeanval,  # carb, on top of the stacked barchart.
                              TFATmeanval,              # prot, in the middle.
                              0)                        # tfat, on the bottom.
        ith_user$sd_stacked <-  c(CARBsdval+PROTmeanval+TFATmeanval,    # carb, on top of the stacked barchart.
                                  PROTsdval+TFATmeanval,                # prot, in the middle.
                                  TFATsdval)                            # tfat, on the bottom.
        
        # for i=1, make the first result dataframe. 
        CPT_kcal_forstacked[c(i,i+1,i+2), ] <- ith_user
        
      }else{
        
        ith_user <- subset(input.df, Group == groups[i])
        
        # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
        PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
        TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
        CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
        PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
        TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
        
        # sd values for stacked barchart. 
        ith_user$sd_base <- c(TFATmeanval+PROTmeanval,  # carb, on top of the stacked barchart.
                              TFATmeanval,              # prot, in the middle.
                              0)                        # tfat, on the bottom.
        ith_user$sd_stacked <-  c(CARBsdval+PROTmeanval+TFATmeanval,    # carb, on top of the stacked barchart.
                                  PROTsdval+TFATmeanval,                # prot, in the middle.
                                  TFATsdval)                            # tfat, on the bottom.
        
        # need another value k in order to specify the correct row.
        k = i-2
        
        # for i = 2,3,4,..., combine rows with the previously made CPT_kcal_forstacked. 
        CPT_kcal_forstacked[c(i+i+k, i+i+k+1, i+i+k+2), ] <- ith_user
        
      }
    }
    # Save the resultant file as .txt file.
    write.table(x=CPT_kcal_forstacked, file = out.fn, sep="\t", row.names=F, quote=F)
    
  }
  
  
# ====================================================================================================================
# Plot a stacked barchart with SDs ... With UserName in ASA24. 
# ====================================================================================================================
  
  StackedWithSD <- function(data){
    ggplot(data, aes(x = UserName, y = mean, fill=macronutrient, colour=macronutrient)) + 
      geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.7)  +
      geom_errorbar(aes(ymin= mean+sd_base, ymax= mean+sd_stacked), width = 0.15, color="grey10") + 
      scale_fill_manual(values = distinct100colors,
                        labels=c( "Carbohydrates", "Protein", "Total fat")) +
      labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
      no_grid + space_axes +
      theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  }
  
# ====================================================================================================================
# Plot a stacked barchart with SDs ... With Groups in NHANES. 
# ====================================================================================================================
  
  StackedWithSD_NHANES <- function(data){
    ggplot(data, aes(x = Group, y = mean, fill=macronutrient, colour=macronutrient)) + 
      geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.7)  +
      geom_errorbar(aes(ymin= mean+sd_base, ymax= mean+sd_stacked), width = 0.15, color="grey10") + 
      scale_fill_manual(values = distinct100colors,
                        labels=c( "Carbohydrates", "Protein", "Total fat")) +
      labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
      no_grid + space_axes +
      theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  }
  
# --------------------------------------------------------------------------------------------------------------















# --------------------------------------------------------------------------------------------------------------------
####################################### OLD BELOW #########################################

# # ---------------------------------------------------------------------------------------------------------------
# # Calculate the mean kcal from carb/protein/fat per participant, no other factors.
# # Since this is an average of all the food items they reported, SD doesn't really make sense. 
# # (because SD will be the variability of kcal among the food items one person reported.)
#   
#   CalcKcal_user <- function(){
#     # Get total (g) for each user and save as a separate dataframe. 
#     PROTsum <<- aggregate(totals$PROT, by = list(totals$UserName), FUN = sum)
#     TFATsum <<- aggregate(totals$TFAT, by = list(totals$UserName), FUN = sum)
#     CARBsum <<- aggregate(totals$CARB, by = list(totals$UserName), FUN = sum)
#     colnames(PROTsum) <- c("UserName", "PROT_sum_g")  
#     colnames(TFATsum) <- c("UserName", "TFAT_sum_g")  
#     colnames(CARBsum) <- c("UserName", "CARB_sum_g")  
#     
#     # Calculate the calories for each macronutrient. 
#     PROTsum$PROT_sum_kcal <- PROTsum$PROT_sum * 4
#     TFATsum$TFAT_sum_kcal <- TFATsum$TFAT_sum * 9
#     CARBsum$CARB_sum_kcal <- CARBsum$CARB_sum * 4
#     
#     # Combine the three tables
#     temp1 <- merge(PROTsum, TFATsum, all = T)
#     macronutr.sum <- merge(temp1, CARBsum, all = T)
#     
#     # Add a column of summed total calories per item/user. 
#     macronutr.sum$total_kcal <- macronutr.sum$PROT_sum_kcal +  
#                                 macronutr.sum$TFAT_sum_kcal +
#                                 macronutr.sum$CARB_sum_kcal 
#     
#     # Add a column of percentage of kcal/macronutrient
#     macronutr.sum$PROT_pk <- macronutr.sum$PROT_sum_kcal / macronutr.sum$total_kcal *100
#     macronutr.sum$TFAT_pk <- macronutr.sum$TFAT_sum_kcal / macronutr.sum$total_kcal *100
#     macronutr.sum$CARB_pk <- macronutr.sum$CARB_sum_kcal / macronutr.sum$total_kcal *100
#     
#     # Modify the dataframe structure for plotting.
#     mean.p <<- macronutr.sum[, c("UserName",  "PROT_pk")]
#     mean.t <<- macronutr.sum[, c("UserName",  "TFAT_pk")]
#     mean.c <<- macronutr.sum[, c("UserName",  "CARB_pk")]
#     
#     # Add a column of macronutrients
#     mean.p$macronutrient <- "PROT"
#     mean.t$macronutrient <- "TFAT"
#     mean.c$macronutrient <- "CARB"
#     
#     # Change XXXX_pk to "value"
#     colnames(mean.p)[2] <- colnames(mean.t)[2] <- colnames(mean.c)[2] <- "value"
#     
#     # Bind the 3 datasets
#     bound <<- rbind(mean.p, mean.t, mean.c)
#     macronutr.mean.l <<- bound[, c(1,3,2)] # sort columns
#     
#     # Check the dimention of the macronutr.mean.l (for programmers)
#     # dim(macronutr.mean.l)  # l means a long table.
#   }
# # ---------------------------------------------------------------------------------------------------------------
# 
# # ===============================================================================================================
# # Plot stacked barcharts 
# # ===============================================================================================================
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Plot the mean kcal from carbs, protein, and fat by participant (normalized)
#   NormalizedPercentKcal <- function(){
#     
#     cat("Showing a normalized stacked barchart.", "\n")
#     
#     library(ggplot2)
#     bwoe <<- ggplot(macronutr.mean.l,
#                     aes(x = factor(UserName), y = value, fill = macronutrient)) + 
#       geom_bar(position = "fill", stat = "identity", colour = "black", width = 0.7) +
#       theme_bw(base_size = 10) +
#       # scale_fill_manual(values = my15colors ) +
#       # labels=c("Protein", "Fat", "Carbohydrates")) +
#       labs(x = element_blank(), y = "Percentages of total kcal intake", fill = "Macronutrients") +
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#       theme(axis.title.x = element_text(margin=margin(t = 5, r = 0, b = 0, l = 0))) +
#       theme(axis.title.y = element_text(margin=margin(t = 0, r = 5, b = 0, l = 0))) + 
#       theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
#       theme(aspect.ratio = 0.4)
#     bwoe
#   }
# # ---------------------------------------------------------------------------------------------------------------
#   
# # ---------------------------------------------------------------------------------------------------------------
# # If there are factor(s) that can group participants, SD will be meaningful.
# # Plot the mean kcal from carbs, protein, and fat by participant (non-normalized)
#   NonNormalizedPercentKcal <- function(show.sd = TRUE){
#     
#     if(show.sd == TRUE){ # default
#     cat("Showing a non-normalized stacked barchart with SD as error bars.", "\n")
#     
#     # Rearrange the macronutr.mean table
#     # Merge mean and SD for PROT
#     temp3 <<- merge(PROTmeans, PROTsd, all = T)
#     colnames(temp3) <<- c("UserName", "Means", "SD")
#     temp3$Macronutrient <<- "PROT"
#     
#     # Merge mean and SD for CARB
#     temp4 <<- merge(CARBmeans, CARBsd, all = T)
#     colnames(temp4) <<- c("UserName", "Means", "SD")
#     temp4$Macronutrient <<- "CARB"
#     
#     # Merge mean and SD for TFAT
#     temp5 <<- merge(TFATmeans, TFATsd, all = T)
#     colnames(temp5) <<- c("UserName", "Means", "SD")
#     temp5$Macronutrient <<- "TFAT"
#     
#     # Bind temp3-5
#     pfc.mean.sd <<- rbind(temp3, temp4, temp5)
#     pfc.mean.sd <<- pfc.mean.sd[, c(1,4,2,3)] # Sort columns
#     
#     # Calculate the cumulative value for creating a stacked chart with error bars.
#     library(dplyr)
#     pfc.mean.sd2 <<- pfc.mean.sd %>% 
#       arrange(desc(Macronutrient)) %>% group_by(UserName) %>% mutate(cumsum_Means=cumsum(Means))  
#     
#     # Stacked barchart with SD error bars.
#     library(ggplot2)
#     bwe <<- ggplot(pfc.mean.sd2, aes(x = UserName, y = Means, fill = Macronutrient)) +
#       geom_col(color = "black", width = 0.6, position = position_stack(vjust=1)) +
#       geom_errorbar(inherit.aes = FALSE, 
#                     aes(x = UserName, ymin = cumsum_Means, ymax = cumsum_Means + SD), 
#                     width = 0.2) +
#       theme_bw(base_size = 10) +
#       # scale_fill_manual(values = my15colors ) +
#       # scale_fill_manual(values=c("lightgoldenrod2", "steelblue", "palevioletred2"))+
#       #labels=c("Carbohydrates", "Protein", "Fat")) +
#       labs(x = element_blank(), y = "Percentages of total kcal intake", fill = "variable") +
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#       theme(axis.title.x = element_text(margin=margin(t = 5, r = 0, b = 0, l = 0))) +
#       theme(axis.title.y = element_text(margin=margin(t = 0, r = 5, b = 0, l = 0))) + 
#       theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
#       theme(aspect.ratio = 0.4)
#     bwe
#     }
#     
#     else if(show.sd == FALSE){
#       
#       cat("Showing a non-normalized stacked barchart without error bars.", "\n")
#       
#       # Not normalized
#       library(ggplot2)
#       nonnormal <<- ggplot(macronutr.mean.l, 
#                           aes(x = UserName, y = value, fill = macronutrient)) + 
#         geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
#         theme_bw(base_size = 10) +
#         # scale_fill_manual(values = my15colors, # ) +
#         # labels=c( "Carbohydrates", "Protein", "Total fat")) +
#         labs(x = element_blank(), y = "Percentages of total kcal intake", fill = "Macronutrients") +
#         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#         theme(axis.title.x = element_text(margin=margin(t = 5, r = 0, b = 0, l = 0))) +
#         theme(axis.title.y = element_text(margin=margin(t = 0, r = 5, b = 0, l = 0))) + 
#         theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
#         theme(aspect.ratio = 0.4)
#       nonnormal
#     }  
#   
#   }
# # --------------------------------------------------------------------------------------------------------------- 
# 
