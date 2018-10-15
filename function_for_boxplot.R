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


ImBplot <- function(data,values_name,group_name,two_group = TRUE){
  
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
      geom_boxplot(notch = TRUE) +
      geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
      stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))
    
  } else {
    
    my_comparison <- combn(unique(as.character(data[,group_name])),2,simplify = FALSE)
    
    box_comp <- ggplot(data, aes(group,values, color = group)) +
      theme_light() +
      geom_boxplot(notch = TRUE) +
      geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
      stat_compare_means(method = "t.test", comparisons = my_comparison) +
      stat_compare_means(label.y =(max(data[,values_name]+3/6*max(data[,values_name]))))
    
  }
  
  return(box_comp)
}
