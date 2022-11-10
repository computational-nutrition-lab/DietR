# FUNCTIONS ==============================================================================

# ========================================================================================
# Prep data for PCA and other cluster analysis.
# Version 1
# Created on 01.13.2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Subset the columns to be used and calculate means by a category if desired 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Function to subset specific data from the totals.

  SubsetColumns <- function(data, start.col, end.col){
    # Column Variables of "totals" dataframe.
    colvars <- names(data)
    # Get the first ID
    start.col <- match(start.col, colvars)
    # Get the second ID
    end.col <- match(end.col, colvars)
    # Subset range
    subsetted <<- data[, start.col:end.col]
    # Print what was loaded.
    cat("'subsetted' contains the following", length(colnames(subsetted)), "columns.", "\n")
    print(colnames(subsetted))
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Keep only the columns with non-zero variance in order to perform PCA.
  KeepNonZeroVarColumns <- function(data){
    subsetted_non0var <<- data[, which(apply(data, 2, var) != 0)] 
    # Print which column(s) were removed.
    if(ncol(data) == ncol(subsetted_non0var)){
      cat("No columns were removed.", "\n")
    }
    if(ncol(data) != ncol(subsetted_non0var)){
      cat("The following column(s) in ", deparse(substitute(data)), " had zero variance and were removed.", "\n")
      print(which(apply(data, 2, var) == 0))
    }
  }
# ---------------------------------------------------------------------------------------------------------------


# ========================================================================================
# Collapse variables by correlation: take only 1 variable if 2 ore more are highly 
# correlated with each other. Code from:
# https://github.com/knights-lab/MLRepo/blob/master/example/lib/collapse-features.r
# ========================================================================================
  
# ---------------------------------------------------------------------------------------------------------------
# CLUSTER
# ClusterByCorrelation returns a vector of cluster ids for clusters with internal
  # complete-linkage correlation of min.cor
  # This is to be used in CollapseByCorrelation.
  "ClusterByCorrelation" <- function(x, min.cor=0.75){
    #     library('fastcluster')
    cc <<- cor(x, use='pairwise.complete.obs', method='pear')
    # if(ncol(x) == 379) browser()
    cc_1 <<- as.dist(1-cc)
    hc <<- hclust(cc_1)
    res <<- cutree(hc, h=1-min.cor)
    names(res) <<- colnames(x)
    return(res)
  }  

# COLLAPSE  
# CollapseByCorrelation returns a vector of cluster ids for clusters with internal  
# complete-linkage correlation of min.cor
#
# By default, chooses cluster reps as highest-variance member if select.rep.fcn=mean
  "CollapseByCorrelation" <- function(x, min.cor=0.75, 
                                        select.rep.fcn=c('var','mean','lowest.mean',
                                                         'longest.name', 'shortest.name')[2],
                                        verbose=FALSE){
    if(verbose) cat('Clustering', ncol(x), 'features...')
    gr <- ClusterByCorrelation(x, min.cor=min.cor)
    if(verbose) cat('getting means...')
    if(select.rep.fcn == 'mean'){
      v <- apply(x,2,function(xx) mean(xx, na.rm=TRUE))
    } else if(select.rep.fcn == 'lowest.mean'){
      v <- apply(x,2,function(xx) -mean(xx, na.rm=TRUE))
    } else if(select.rep.fcn == 'longest.name'){
      v <- nchar(colnames(x))
    } else if(select.rep.fcn == 'shortest.name'){
      v <- -nchar(colnames(x))
    } else {
      v <- apply(x,2,function(xx) var(xx,use='complete.obs'))
    }
    if(verbose) cat('choosing reps...')
    reps <- sapply(split(1:ncol(x),gr),function(xx) xx[which.max(v[xx])])
    if(verbose)
      cat(sprintf('collapsed from %d to %d.\n',ncol(x), length(reps)))
    return(list(reps=reps, groups=gr))
  }
# ---------------------------------------------------------------------------------------------------------------  

# ---------------------------------------------------------------------------------------------------------------  
# Function to save the correlation matrix as a txt file.
# The correlation matrix (cc) is produced by ClusterByCorrelation().
  SaveCorrMatrix <- function(x=cc, out.fn){
    write.table(as.data.frame(as.matrix(x)), out.fn, sep = "\t", row.names = F)
  } 
# ---------------------------------------------------------------------------------------------------------------  
  
  
