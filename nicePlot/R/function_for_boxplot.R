#============================#
##### Utilitary function #####
#============================#
#' @export

basePlot <- function(data,values_name,group_name, aditional_grouping_name = NA){

  ## recuperation of the labels

  if(is.character(values_name)){name_y <- values_name} else {name_y <- colnames(data)[values_name]}
  if(is.character(group_name)){
    name_x <- group_name
    name_l <- group_name
  } else {
    name_x <- colnames(data)[group_name]
    name_l <- colnames(data)[group_name]
  }
  if(!is.na(aditional_grouping_name)){
    aditional_grouping = TRUE
  } else {
    aditional_grouping = FALSE
  }
  if(aditional_grouping){
    if(is.character(aditional_grouping_name)){
      name_l <- aditional_grouping_name
    } else {
      name_l <- colnames(data)[aditional_grouping_name]
    }
  }

  ## Generation of the comon plot spine

  if(aditional_grouping){

  basep <- ggplot(data, aes(data[,group_name],data[,values_name], fill = data[,aditional_grouping_name])) +
    theme_light() +
    scale_color_discrete(name_l) +
    xlab(name_x) +
    ylab(name_y) +
    labs(fill = name_l)
  } else {

    basep <- ggplot(data, aes(data[,group_name],data[,values_name], color = data[,group_name])) +
      theme_light() +
      scale_color_discrete(name_l) +
      xlab(name_x) +
      ylab(name_y) +
      labs(fill = name_l)
  }

  return(basep)
}

#===========================#
##### Box plot function #####
#===========================#
#' @export

# data corresponding to the dataset
# values_name to the name in character/numeric of the values column
# group_name to the name in character/numeric of the group column
# two_group indicate if you have two or more group in the dataset
# notched indicate if you prefer notch plot rather than boxplot

imBplot <- function(data, values_name, group_name, aditional_grouping_name = NA, two_group = TRUE, notched = FALSE){

  # Loading the require package if not

  require(ggplot2)
  require(ggpubr)

  ## check input parameters ##

  if(!is.data.frame(data)){stop("data parameters must be a dataFrame")}
  if(!is.factor(data[,group_name])){stop("group column must be a factor")}
  if( !is.logical(two_group) ){stop("two_group parameter must a logical")}
  if( !is.logical(notched) ){stop("notched parameter must a logical")}
  if(!is.na(aditional_grouping_name)){
    if(!is.factor(data[,aditional_grouping_name])){stop("aditional_grouping parameter must be a factor")}
  }

  ## generation of the spine of the plot

  box_comp <- basePlot(data,values_name,group_name, aditional_grouping_name = aditional_grouping_name)

  ## Create boxplot

  if(two_group){
    varres <- var.test(data[,values_name][data[,group_name] == levels(data[,group_name])[1]], data[,values_name][data[,group_name] == levels(data[,group_name])[2]])[[3]]
    if (varres > 0.05 ){tres <- TRUE} else {tres <- FALSE}

    box_comp <- box_comp +
      geom_boxplot(notch = notched) +
      geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 7/10,position = position_dodge(0.75)) +
      stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))
  } else {

    my_comparison <- combn(unique(as.character(data[,group_name])),2,simplify = FALSE)

    box_comp <- box_comp +
      geom_boxplot(notch = notched) +
      geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
      stat_compare_means(method = "t.test", comparisons = my_comparison) +
      stat_compare_means(label.y =(max(data[,values_name]+3/6*max(data[,values_name]))))

    print("The t-test done between each group do not take account of a variance test, thus it might be interpreted carefully.",col = "red")
  }
  return(box_comp)
}

#==============================#
##### Violin plot function #####
#==============================#
#' @export

# data corresponding to the dataset
# values_name to the name in character/numeric of the values column
# group_name to the name in character/numeric of the group column
# two_group indicate if you have two or more group in the dataset
# box indicate if you want to add a boxplot inside the violin

imVplot <- function(data, values_name, group_name, aditional_grouping_name = NA, two_group = TRUE, box = TRUE, dot = FALSE){

  # Loading the require package if not

  require(ggplot2)
  require(ggpubr)

  ## check input parameters ##

  if(!is.data.frame(data)){stop("data parameters must be a dataFrame")}
  if(!is.factor(data[,group_name])){stop("group column must be a factor")}
  if( !is.logical(two_group) ){stop("two_group parameter must a logical")}
  if( !is.logical(box)){stop("box parameter must a logical")}
  if(!is.na(aditional_grouping_name)){
    if(!is.factor(data[,aditional_grouping_name])){stop("aditional_grouping parameter must be a factor")}
  }

  ## generation of the spine of the plot

  if(!is.na(aditional_grouping_name)){
    vio_comp <- basePlot(data,values_name,group_name, aditional_grouping_name = aditional_grouping_name)
  } else {
    vio_comp <- basePlot(data,values_name,group_name, aditional_grouping_name = NA)
  }

  ## Create violin plot

  if(two_group){

    varres <- var.test(data[,values_name][data[,group_name] == levels(data[,group_name])[1]], data[,values_name][data[,group_name] == levels(data[,group_name])[2]])[[3]]# adpat it to automatically put group with 0 in the script
    if (varres > 0.05 ){tres <- TRUE} else {tres <- FALSE}

    vio_comp <- vio_comp +
      geom_violin() +
      stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(data[,values_name])+1/6*max(data[,values_name])))

    if(box) vio_comp <- vio_comp + geom_boxplot(width = 0.1,position =  position_dodge(0.90))
    if(dot) vio_comp <- vio_comp + geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 7/10,position = position_dodge(0.90))

  } else {

    my_comparison <- combn(unique(as.character(data[,group_name])),2,simplify = FALSE)

    vio_comp <- vio_comp +
      stat_compare_means(method = "t.test", comparisons = my_comparison) +
      stat_compare_means(label.y =(max(data[,values_name])+3/6*max(data[,values_name])))


    if(box) vio_comp <- vio_comp + geom_boxplot(width = 0.1,position =  position_dodge(0.90))
    if(dot) vio_comp <- vio_comp + geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 7/10,position = position_dodge(0.90))

    print("The t-test done between each group do not take account of a variance test, thus it might be interpreted carefully.",col = "red")
  }
  return(vio_comp)
}


#============================#
##### Data set generator #####
#============================#
#' @export

dataG <- function(nrow, ngroup = 2, nsecondary_group = NA, groupAsFactor = TRUE, forceDif = FALSE){

  # check parameters

  if(nrow <= 1 | !is.numeric(nrow) | nrow%%1 != 0){stop("nrow parameter must be an integer greater or equal to 2")}
  if(ngroup <= 1 | !is.numeric(ngroup) | ngroup%%1 != 0){stop("ngroup parameter must be an integer greater or equal to 2")}
  if( !is.logical(groupAsFactor) ){stop("groupAsFactor parameter must a logical")}
  if( !is.logical(forceDif) ){stop("forceDif parameter must a logical")}
  if(!is.na(nsecondary_group)){
    secondary_group = TRUE
    if(nsecondary_group <= 1 | !is.numeric(nsecondary_group) | nsecondary_group%%1 != 0){stop("nsecondary_group parameter must be an integer greater or equal to 2")}
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
