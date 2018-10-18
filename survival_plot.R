## load library

library(survival)
library(survminer)

## load dataset example

data(lung)

## Descriptiver stat

dstat <- function(data,quant,qual){
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



##### Below this is an example of a script we could conduct to realise a survival analysis #####

### Data importation

data <- read.table

### Check of incoherence in the database ###

# death before people came back to dialise

