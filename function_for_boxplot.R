#==========================#
##### Packages loading #####
#==========================#

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(cowplot)

#===========================#
##### Box plot function #####
#===========================#

# data corresponding to the dataset
# values_name to the name in character of the values column
# group_name to the name in character of the group column
# two_group indicate if you have two or more group in the dataset

imBplot <- function(data, values_name, group_name, two_group = TRUE, notched = FALSE){
  
  ## check input parameters ##
  
  if(!is.data.frame(data)){
    stop("data parameters must be a dataFrame")
  }
  
  if(!is.factor(data[,group_name])){
    stop("group column must be a vectorType")
  }
  
  if(two_group){
  
    varres <- var.test(data[,values_name][data[,group_name] == 0], data[,values_name][data[,group_name] ==1])[[3]]
    if (varres > 0.05 ){
      tres <- TRUE
    } else {
      tres <- FALSE
    }
    
    box_comp <- ggplot(data, aes(group,values, color = group)) +
      theme_light() +
      geom_boxplot(notch = notched) +
      geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
      stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))
    
  } else {
    
    my_comparison <- combn(unique(as.character(data[,group_name])),2,simplify = FALSE)
    
    box_comp <- ggplot(data, aes(group,values, color = group)) +
      theme_light() +
      geom_boxplot(notch = notched) +
      geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
      stat_compare_means(method = "t.test", comparisons = my_comparison) +
      stat_compare_means(label.y =(max(data[,values_name]+3/6*max(data[,values_name]))))
    
    print("The t-test done between each group do not take account of a variance test, thus it might be interpreted carefully.",col = "red")
    
  }
  
  return(box_comp)
}

#============================#
##### Data set generator #####
#============================#


dataG <- function(nrow, ngroup = 2, groupAsFactor = TRUE){
  
  # check parameters
  
  if(nrow <= 1 | !is.numeric(nrow) | nrow%%1 != 0){
    stop("nrow parameter must be an integer greater or equal to 2")
  }
  
  if(ngroup <= 1 | !is.numeric(ngroup) | ngroup%%1 != 0){
    stop("ngroup parameter must be an integer greater or equal to 2")
  }
  
  if( !is.logical(groupAsFactor) ){
    stop("groupAsFactor parameters must a logical")
  }
  
  # creation of the dataset
  
  df <- data.frame(values = runif(nrow), group = sample(seq(0,(ngroup-1)), replace=TRUE, size=nrow))
  
  if(groupAsFactor){
    df$group <- as.factor(df$group)
  }
  
  return(df)
}
