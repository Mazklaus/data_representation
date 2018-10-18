## load library

library(survival)
library(survminer)

## load dataset example

data(lung)

## fit cox model

data <- coxph(Surv(time, status) ~ sex, data = lung)
verif <- cox.zph(data)

## make the survival plot

ggsurvplot(survfit(Surv(time, status) ~ ph.ecog, data = lung),conf.int = TRUE) + pairwise_survdiff()

## Make a plot to check the hazards assumption in the cox model

ggcoxzph(verif)
