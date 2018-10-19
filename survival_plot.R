## load library

library(survival)
library(survminer)
library(stringr)

## load dataset example

data(lung)

## Descriptiver stat

nicePrint <- function(data,tab_length = 6){
  
  ## required package
  
  require(stringr)
  require(dplyr)
  
  ## Check input
  
  if(!is.data.frame(data)){stop("data parameter must be dataframe")}
  if(!is.numeric(tab_length) | tab_length <= 1 | tab_length%%1 != 0){stop("tab_length parameters must be a integer greater or equal to 1")}
  
  ## determination of the tabulation of each column
  
  columnNames <- colnames(data)
  data <- data %>% mutate_all(as.character)
  ref_tab <- c()
  
  
  for (i in seq(1,length(columnNames))) {## I need to add the line name and thus take its length into account
    vect <- data[str_length(data[,i]) == max(str_length(data[,i]),na.rm = TRUE),i]
    vect <- vect[!is.na(vect)]
    val <- str_length(vect[1])
    if(tab_length <= val){ ## i need to adapt the code to take how much it's longer to be sure to not colide
      ref_tab[i] <- 2*tab_length
    } else {
      ref_tab[i] <- tab_length
    }
  }
  
  for (i in 1:nrow(data)) {
    for (y in 1:length(columnNames)) {
      if(is.na(data[i,y])) ret <- 2 else ret <- str_length(data[i,y])
      cat(data[i,y],strrep(" ",ref_tab[y]-ret))
      if(y == length(columnNames)) cat("\n")
    }
  }
}

dstat <- function(data,quant,qual){
  
  print("Quantitative data :")
  for (i in quant) {
    if(is.character(i)){varname <- i} else {varname <- colnames(data)[i]}
    print(varname)
    print(mean(data[,i]))
    print(var(data[,i]))
  }
  for (i in qual) {
    if(is.character(i)){varname <- i} else {varname <- colnames(data)[i]}
    data[,i] <- as.factor(data[,i])
    print(varname)
    for (y in levels(data[,i])) {
      print(y)
      print(nrow(data[data[,i]==y,])/nrow(data)*100)
    }
  }
}

## fit cox model

data <- coxph(Surv(time, status) ~ sex, data = lung)
verif <- cox.zph(data)

## make the survival plot

ggsurvplot(survfit(Surv(time, status) ~ ph.ecog, data = lung),conf.int = TRUE)
pairwise_survdiff(Surv(time, status) ~ ph.ecog, data = lung)

## Make a plot to check the hazards assumption in the cox model

ggcoxzph(verif)

