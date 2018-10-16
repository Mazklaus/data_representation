#==========================#
##### Packages loading #####
#==========================#

library(ggplot2)
library(ggpubr)
#library(gridExtra)
#library(cowplot)

#===========================#
##### Box plot function #####
#===========================#

# data corresponding to the dataset
# values_name to the name in character/numeric of the values column
# group_name to the name in character/numeric of the group column
# two_group indicate if you have two or more group in the dataset
# notched indicate if you prefer notch plot rather than boxplot

imBplot <- function(data, values_name, group_name, two_group = TRUE, notched = FALSE, aditional_grouping = FALSE, aditional_grouping_name = NA){
  
  # Loading the require package if not
  
  require(ggplot2)
  require(ggpubr)
  
  ## check input parameters ##
  
  if(!is.data.frame(data)){stop("data parameters must be a dataFrame")}
  if(!is.factor(data[,group_name])){stop("group column must be a factor")}
  if( !is.logical(two_group) ){stop("two_group parameter must a logical")}
  if( !is.logical(notched) ){stop("notched parameter must a logical")}
  if(!is.logical(aditional_grouping)){stop("aditional_grouping parameter must be a logical")}
  if(!is.na(aditional_grouping_name)){
    if(!is.factor(data[,aditional_grouping_name])){stop("aditional_grouping parameter must be a factor")}
  }
  
  ## Create boxplot
  
  if(two_group){
    varres <- var.test(data[,values_name][data[,group_name] == 0], data[,values_name][data[,group_name] ==1])[[3]]
    if (varres > 0.05 ){tres <- TRUE} else {tres <- FALSE}
    
    if(aditional_grouping){
      
      box_comp <- ggplot(data, aes(data[,group_name],data[,values_name], fill = data[,aditional_grouping_name])) +
        theme_light() +
        geom_boxplot(notch = notched) +
        geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 7/10,position = position_dodge(0.75)) +
        stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))
    } else {
      
      box_comp <- ggplot(data, aes(data[,group_name],data[,values_name], color = data[,group_name])) +
        theme_light() +
        geom_boxplot(notch = notched) +
        geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
        stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))
    }
  } else {
    
    my_comparison <- combn(unique(as.character(data[,group_name])),2,simplify = FALSE)
    
    if (aditional_grouping){
      
      box_comp <- ggplot(data, aes(data[,group_name],data[,values_name], fill = data[,aditional_grouping_name])) +
        theme_light() +
        geom_boxplot(notch = notched) +
        geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 7/10,position = position_dodge(0.75)) +
        stat_compare_means(method = "t.test", comparisons = my_comparison) +
        stat_compare_means(label.y =(max(data[,values_name]+3/6*max(data[,values_name]))))
    } else {
      
      box_comp <- ggplot(data, aes(data[,group_name],data[,values_name], color = data[,group_name])) +
        theme_light() +
        geom_boxplot(notch = notched) +
        geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
        stat_compare_means(method = "t.test", comparisons = my_comparison) +
        stat_compare_means(label.y =(max(data[,values_name]+3/6*max(data[,values_name]))))
    }
    print("The t-test done between each group do not take account of a variance test, thus it might be interpreted carefully.",col = "red")
  }
  return(box_comp)
}

#==============================#
##### Violin plot function #####
#==============================#

# data corresponding to the dataset
# values_name to the name in character/numeric of the values column
# group_name to the name in character/numeric of the group column
# two_group indicate if you have two or more group in the dataset
# box indicate if you want to add a boxplot inside the violin

imVplot <- function(data, values_name, group_name, two_group = TRUE, box = FALSE, aditional_grouping = FALSE, aditional_grouping_name = NA){
  
  # Loading the require package if not
  
  require(ggplot2)
  require(ggpubr)
  
  ## check input parameters ##
  
  if(!is.data.frame(data)){stop("data parameters must be a dataFrame")}
  if(!is.factor(data[,group_name])){stop("group column must be a vectorType")}
  if( !is.logical(two_group) ){stop("two_group parameter must a logical")}
  if( !is.logical(box)){stop("box parameter must a logical")}
  if(!is.logical(aditional_grouping)){stop("aditional_grouping parameter must be a logical")}
  if(!is.na(aditional_grouping_name)){
    if(!is.factor(data[,aditional_grouping_name])){stop("aditional_grouping parameter must be a factor")}
  }
  
  ## Create violin plot
  
  if(two_group){
    
    varres <- var.test(data[,values_name][data[,group_name] == 0], data[,values_name][data[,group_name] ==1])[[3]]
    if (varres > 0.05 ){tres <- TRUE} else {tres <- FALSE}
    
    if(aditional_grouping){
      if(box){
        
        vio_comp <- ggplot(df, aes(data[,group_name],data[,values_name], fill = data[,aditional_grouping_name])) +
          theme_light() +
          geom_violin() +
          geom_boxplot(width = 0.1,position =  position_dodge(0.90)) +
          geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 7/10,position = position_dodge(0.90)) +
          stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))
      } else {
        
        vio_comp <- ggplot(df, aes(data[,group_name],data[,values_name], fill = data[,aditional_grouping_name])) +
          theme_light() +
          geom_violin() +
          geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 7/10,position = position_dodge(0.90)) +
          stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))
      }
    } else {
      if(box){
        
        vio_comp <- ggplot(df, aes(data[,group_name],data[,values_name], color = data[,group_name])) +
          theme_light() +
          geom_violin() +
          geom_boxplot(width = 0.1,position =  position_dodge(0.90)) +
          geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
          stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))
      } else {
        
        vio_comp <- ggplot(df, aes(data[,group_name],data[,values_name], color = data[,group_name])) +
          theme_light() +
          geom_violin() +
          geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
          stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))
      }
    }
  } else {
    
    my_comparison <- combn(unique(as.character(data[,group_name])),2,simplify = FALSE)
    
    if(aditional_grouping){
      if(box) {
        
        vio_comp <- ggplot(data, aes(data[,group_name],data[,values_name], fill = data[,aditional_grouping_name])) +
          theme_light() +
          geom_violin() +
          geom_boxplot(width = 0.1,position =  position_dodge(0.90)) +
          geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 7/10, position =  position_dodge(0.90)) +
          stat_compare_means(method = "t.test", comparisons = my_comparison) +
          stat_compare_means(label.y =(max(data[,values_name])+3/6*max(data[,values_name])))
        
      } else {
        
        vio_comp <- ggplot(data, aes(data[,group_name],data[,values_name], fill = data[,aditional_grouping_name])) +
          theme_light() +
          geom_violin() +
          geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 7/10,position =  position_dodge(0.90)) +
          stat_compare_means(method = "t.test", comparisons = my_comparison) +
          stat_compare_means(label.y =(max(data[,values_name])+3/6*max(data[,values_name])))
      }
    } else {
      if(box) {
        
        vio_comp <- ggplot(data, aes(data[,group_name],data[,values_name], color = data[,group_name])) +
          theme_light() +
          geom_violin() +
          geom_boxplot(width = 0.1,position =  position_dodge(0.90)) +
          geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
          stat_compare_means(method = "t.test", comparisons = my_comparison) +
          stat_compare_means(label.y =(max(data[,values_name])+3/6*max(data[,values_name])))
        
      } else {
        
        vio_comp <- ggplot(data, aes(data[,group_name],data[,values_name], color = data[,group_name])) +
          theme_light() +
          geom_violin() +
          geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
          stat_compare_means(method = "t.test", comparisons = my_comparison) +
          stat_compare_means(label.y =(max(data[,values_name])+3/6*max(data[,values_name])))
      }
    }
    print("The t-test done between each group do not take account of a variance test, thus it might be interpreted carefully.",col = "red")
    
  }
  
  return(vio_comp)
}


#============================#
##### Data set generator #####
#============================#


dataG <- function(nrow, ngroup = 2, secondary_group = FALSE, nsecondary_group = NA, groupAsFactor = TRUE, forceDif = FALSE){
  
  # check parameters
  
  if(nrow <= 1 | !is.numeric(nrow) | nrow%%1 != 0){
    stop("nrow parameter must be an integer greater or equal to 2")
  }
  
  if(ngroup <= 1 | !is.numeric(ngroup) | ngroup%%1 != 0){
    stop("ngroup parameter must be an integer greater or equal to 2")
  }
  
  if( !is.logical(groupAsFactor) ){
    stop("groupAsFactor parameter must a logical")
  }
  
  if( !is.logical(forceDif) ){
    stop("forceDif parameter must a logical")
  }
  
  if(!is.logical(secondary_group)){
    stop("secondary_group parameter must be a logical")
  }
  
  
  if(!is.na(nsecondary_group)){
    if(nsecondary_group <= 1 | !is.numeric(nsecondary_group) | nsecondary_group%%1 != 0){
      stop("nsecondary_group parameter must be an integer greater or equal to 2")
    }
  }
  
  # creation of the dataset
  
  if(secondary_group){
    
    df <- data.frame(values = runif(nrow), group = sample(seq(0,(ngroup-1)), replace=TRUE, size=nrow),secondary_group = sample(seq(0,(nsecondary_group-1)), replace=TRUE, size=nrow))
    
    if(forceDif){
      
      for(i in seq(0,(ngroup-1))){
        df[which(df$group == i),]$values <- df[which(df$group == i),]$values * (runif(1)*10)
        df[which(df$secondary_group == i),]$values <- df[which(df$secondary_group == i),]$values * (runif(1)*10)
      }
      
    }
    
    if(groupAsFactor){
      df$group <- as.factor(df$group)
      df$secondary_group <- as.factor(df$secondary_group)
    }
    
  } else {
    
    df <- data.frame(values = runif(nrow), group = sample(seq(0,(ngroup-1)), replace=TRUE, size=nrow))
    
    if(forceDif){
      
      for(i in seq(0,(ngroup-1))){
        df[which(df$group == i),]$values <- df[which(df$group == i),]$values * (runif(1)*10)
      }
      
    }
    
    if(groupAsFactor){
      df$group <- as.factor(df$group)
    }
  }
  return(df)
}
