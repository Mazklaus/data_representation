## load library

library(survival)
library(survminer)
library(stringr)

## load dataset example

data(lung)

## Descriptiver stat

nicePrint <- function(data, tab_length = 6, rowNames = TRUE, colNames = TRUE){
  
  ## required package
  
  require(stringr)
  require(dplyr)
  
  ## Check input
  
  if(!is.data.frame(data)){stop("data parameter must be dataframe")}
  if(!is.numeric(tab_length) | tab_length <= 1 | tab_length%%1 != 0){stop("tab_length parameters must be a integer greater or equal to 1")}
  if(!is.logical(rowNames)){stop("rowNames parameters must be a logical")}
  
  ## determination of the tabulation of each column
  
  columnNames <- colnames(data)
  lineNames <- rownames(data)
  data <- data %>% mutate_all(as.character)
  ref_tab <- c()
  
  if(colNames){ 
    
    if(rowNames){
      for (i in seq(0,(length(columnNames)))) {
        if(i == 0){
          val <- max(str_length(lineNames))
        } else {
          vect <- data[str_length(data[,i]) == max(str_length(data[,i]),na.rm = TRUE),i]
          vect <- vect[!is.na(vect)]
          val <- max(c(str_length(vect[1]),max(str_length(columnNames[i]))))
        }
        if((tab_length*2) <= val){
          ref_tab[i+1] <- (val+2)
        } else if(tab_length <= val){
          ref_tab[i+1] <- 2*tab_length
        } else {
          ref_tab[i+1] <- tab_length
        }
      }
      for (i in 0:nrow(data)) {
        for (y in 0:length(columnNames)) {
          if(y == 0){
            if(i == 0){
              cat(strrep(" ",(ref_tab[y+1]+1)))
            } else {
              cat(lineNames[i],strrep(" ",ref_tab[y+1]-str_length(lineNames[i])))
            }
          } else {
            if(i == 0){
              cat(columnNames[y],strrep(" ",ref_tab[y+1]-str_length(columnNames[y])))
            } else {
              if(is.na(data[i,y])) ret <- 2 else ret <- str_length(data[i,y])
              cat(data[i,y],strrep(" ",ref_tab[y+1]-ret))
            }
          }
          
          if(y == length(columnNames)) cat("\n")
        }
      }
      
    } else {
      for (i in seq(1,length(columnNames))) {
        vect <- data[str_length(data[,i]) == max(str_length(data[,i]),na.rm = TRUE),i] ## opti make this art a function one #B
        vect <- vect[!is.na(vect)]
        val <- max(c(str_length(vect[1]),max(str_length(columnNames[i]))))
        if((tab_length*2) <= val){
          ref_tab[i] <- (val+2)
        } else if(tab_length <= val){
          ref_tab[i] <- 2*tab_length
        } else {
          ref_tab[i] <- tab_length
        } #E
      }
      for (i in 0:nrow(data)) {
        for (y in 1:length(columnNames)) {
          if(i == 0){
            cat(columnNames[y],strrep(" ",ref_tab[y]-str_length(columnNames[y])))
          } else {
            if(is.na(data[i,y])) ret <- 2 else ret <- str_length(data[i,y])
            cat(data[i,y],strrep(" ",ref_tab[y+1]-ret))
          }
          if(y == length(columnNames)) cat("\n")
        }
      }
    }
    
  } else {
    
    if(rowNames){
      for (i in seq(0,(length(columnNames)))) {
        if(i == 0){
          val <- max(str_length(lineNames))
        } else {
          vect <- data[str_length(data[,i]) == max(str_length(data[,i]),na.rm = TRUE),i]
          vect <- vect[!is.na(vect)]
          val <- str_length(vect[1])
        }
        if((tab_length*2) <= val){
          ref_tab[i+1] <- (val+2)
        } else if(tab_length <= val){
          ref_tab[i+1] <- 2*tab_length
        } else {
          ref_tab[i+1] <- tab_length
        }
        print(ref_tab)
      }
      for (i in 1:nrow(data)) {
        for (y in 0:length(columnNames)) {
          if(y == 0){
            cat(lineNames[i],strrep(" ",ref_tab[y+1]-str_length(lineNames[i])))
          } else {
            if(is.na(data[i,y])) ret <- 2 else ret <- str_length(data[i,y])
            cat(data[i,y],strrep(" ",ref_tab[y+1]-ret))
          }
          if(y == length(columnNames)) cat("\n")
        }
      }
      
    } else {
      for (i in seq(1,length(columnNames))) {
        vect <- data[str_length(data[,i]) == max(str_length(data[,i]),na.rm = TRUE),i] ## opti make this art a function one #B
        vect <- vect[!is.na(vect)]
        val <- str_length(vect[1])
        if((tab_length*2) <= val){
          ref_tab[i] <- (val+2)
        } else if(tab_length <= val){
          ref_tab[i] <- 2*tab_length
        } else {
          ref_tab[i] <- tab_length
        } #E
      }
      for (i in 1:nrow(data)) {
        for (y in 1:length(columnNames)) {
          if(is.na(data[i,y])) ret <- 2 else ret <- str_length(data[i,y])
          cat(data[i,y],strrep(" ",ref_tab[y]-ret))
          if(y == length(columnNames)) cat("\n")
        }
      }
    }
  }
}

dstat <- function(data,quant,qual){
  
  y <- 0
  quant_frame <- data_frame("variable","mean","variance")
  print("Quantitative data :")
  for (i in quant) {
    y <- y +1
    if(is.character(i)){varname <- i} else {varname <- colnames(data)[i]}
    quant_frame[y,] <- c(varname,round(mean(data[,i]),2),round(var(data[,i]),2))
  }
  nicePrint(as.data.frame(quant_frame))
  cat("\n")
  print("Qualitative data :")
  for (i in qual) {
    if(is.character(i)){varname <- i} else {varname <- colnames(data)[i]}
    data[,i] <- as.factor(data[,i])
    percent <- c()
    for (y in levels(data[,i])) {
      percent[y] <- paste(round(nrow(data[data[,i]==y,])/nrow(data)*100,2),"%",sep=" ")
    }
    temp <- as.data.frame(t(as.data.frame(percent)))
    colnames(temp) <- as.character(levels(data[,i]))
    rownames(temp) <- varname
    nicePrint(temp)
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

