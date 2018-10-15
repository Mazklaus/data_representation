#=====================#
##### Preparation #####
#=====================#

### Loading of packages ###

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(cowplot)

### Function ###

# calculation of the standard error #

std <- function(x) sd(x)/sqrt(length(x))

#=================================#
##### Two groups data example #####
#=================================#

### Generation of a random test dataset ###

df <- data.frame(values = runif(100), group = sample(c(0,1), replace=TRUE, size=100))
df$group <- as.factor(df$group)

df$mean[df$group == 0] <- sum(df$values[df$group==0])/length(df$values[df$group==0])
df$mean[df$group == 1] <- sum(df$values[df$group==1])/length(df$values[df$group==1])
df$std[df$group == 0] <- std(df$values[df$group == 0])
df$std[df$group == 1] <- std(df$values[df$group == 1])

### Compare the two groups ###

varres <- var.test(df$values[df$group == 0], df$values[df$group ==1])[[3]]
if (varres > 0.05 ){
  tres <- TRUE
} else {
  tres <- FALSE
}

### simple boxplot example ###

box <- ggplot(df, aes(group,values, color = group)) +
  theme_light() +
  geom_boxplot() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10)

### simple violin plot example ###

violin <- ggplot(df, aes(group,values, color = group)) +
  theme_light() +
  geom_violin() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10)

### Complex boxplot example ###

box_comp <- ggplot(df, aes(group,values, color = group)) +
  theme_light() +
  geom_boxplot(notch = TRUE) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
  stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(df$values)+1/6*max(df$values)))

### Complex violin plot example ###

violin_comp <- ggplot(df, aes(group,values, color = group)) +
  theme_light() +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
  stat_compare_means(method = "t.test", method.args = list(var.equal = tres),label.y = (max(df$values)+1/6*max(df$values)))

### Generate graphs together on the same page ###

plot_grid(box,violin, box_comp, violin_comp)


#===========================================#
##### Three or more groups data example #####
#===========================================#

### Generation of a random test dataset ###

df2 <- data.frame(values = runif(100), group = sample(c(0,1,2), replace=TRUE, size=100))
df2$group <- as.factor(df2$group)

df2$mean[df$group == 0] <- sum(df$values[df$group==0])/length(df$values[df$group==0])
df2$mean[df$group == 1] <- sum(df$values[df$group==1])/length(df$values[df$group==1])
df2$mean[df$group == 2] <- sum(df$values[df$group==2])/length(df$values[df$group==2])
df2$std[df$group == 0] <- std(df$values[df$group == 0])
df2$std[df$group == 1] <- std(df$values[df$group == 1])
df2$std[df$group == 2] <- std(df$values[df$group == 2])

### Compare the two groups ###

my_comparison <- list(c("0","1"),c("1","2"),c("0","2"))

### simple boxplot example ###

box <- ggplot(df2, aes(group,values, color = group)) +
  theme_light() +
  geom_boxplot() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10)

### simple violin plot example ###

violin <- ggplot(df2, aes(group,values, color = group)) +
  theme_light() +
  geom_violin() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10)

### Complex boxplot example ###

box_comp <- ggplot(df2, aes(group,values, color = group)) +
  theme_light() +
  geom_boxplot(notch = TRUE) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
  stat_compare_means(method = "t.test", comparisons = my_comparison) +
  stat_compare_means(label.y =(max(df2$values)+3/6*max(df2$values)))

### Complex violin plot example ###

violin_comp <- ggplot(df2, aes(group,values, color = group)) +
  theme_light() +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7,aes(color = NULL),alpha = 2/10) +
  stat_compare_means(method = "t.test", comparisons = my_comparison) +
  stat_compare_means(label.y =(max(df2$values)+3/6*max(df2$values)))

### Generate graphs together on the same page ###

plot_grid(box, violin, box_comp, violin_comp)
