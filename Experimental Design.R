######################################################################################
################ Comparing the means of two groups (or only one) #####################
#####################################################################################

# library(dplyr) 
library(ggplot2)
library(ggpubr)

data("ToothGrowth")
?ToothGrowth

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
ToothGrowth$supp <- as.factor(ToothGrowth$supp)
levels(ToothGrowth$supp)=c("Orange Juice","Ascorbic Acid")

head(ToothGrowth)

ToothGrowth$len[ToothGrowth$supp=="Orange Juice"]
ToothGrowth$len[ToothGrowth$supp=="Ascorbic Acid"]

# Simple numerical comparison

mg1=mean(ToothGrowth$len[ToothGrowth$supp=="Orange Juice"])
mg2=mean(ToothGrowth$len[ToothGrowth$supp=="Ascorbic Acid"])

mg1
mg2

mg1-mg2


# Graphical comparison

# compare different delivery methods groups
e <- ggplot(ToothGrowth, aes(x = supp, y = len))

# add a title in ggplot
e + geom_boxplot() + 
  ggtitle("Teeth length by delivery methods") 

# use different colors
e + geom_boxplot(aes(color = supp)) + 
  ggtitle("Teeth length by delivery methods") 

# fill the box and add names to axes
e + geom_boxplot(aes(fill = supp)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) + 
  ggtitle("Teeth length by delivery methods") +
  xlab("Delivery method") + ylab("Teeth length")


# compare different dose groups
e2 <- ggplot(ToothGrowth, aes(x = dose, y = len))
e2 + geom_boxplot(aes(fill = dose)) +
  scale_fill_manual(values = c("red", "green", "yellow")) + 
  ggtitle("Teeth length by dose") +
  xlab("Dose") + ylab("Teeth length")


# Graphical comparison considering two factors
e <- ggplot(ToothGrowth, aes(x = supp, y = len))
e + geom_boxplot(aes(fill = dose)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) + 
  ggtitle("Teeth length by delivery method and dose combinations") +
  xlab("Delivery method and dose Combinations") + ylab("Teeth length")

e2 <- ggplot(ToothGrowth, aes(x = dose, y = len))
e2 + geom_boxplot(aes(fill = supp)) +
  scale_fill_manual(values = c("red", "yellow")) + 
  ggtitle("Teeth length by delivery method and dose combinations") +
  xlab("Delivery method and dose Combinations") + ylab("Teeth length") 


# Between-group variation and within-group variation

attach(ToothGrowth)

s1=subset(ToothGrowth, ToothGrowth$dose=="0.5")
s2=subset(ToothGrowth, ToothGrowth$dose=="1")
s3=subset(ToothGrowth, ToothGrowth$dose=="2")

mean(s1$len)
mean(s2$len)
mean(s3$len)

# within-group variation
sd(s1$len)
sd(s2$len)
sd(s3$len)

# Between-group variation

total_mean=mean(c(mean(s1$len), mean(s2$len), mean(s3$len)))
total_mean
mean(ToothGrowth$len) # the same

sd(c(mean(s1$len), mean(s2$len), mean(s3$len)))
sd(ToothGrowth$len) # not the same


###########################################################################
############################ parametric tests #############################
###########################################################################

########################################################################
### one-sample t-test ##################################################
########################################################################

set.seed(127)
?rnorm

x<-rnorm(1000, mean = 0, sd = 20) # generate some data

#############################################################################
# assumption 1: the data are normally distributed ##########################
#############################################################################

hist(x)

# how to check normality manually with other tests

library(tseries)
jarque.bera.test(x)
shapiro.test(x)

jarque.bera.test(x)

library(nortest) # Anderson-Darling normality test
ad.test(x)

library(nortest) # Cramer-von Mises normality test
cvm.test(x)

library(nortest) #  Lilliefors (Kolmogorov-Smirnov) normality test
lillie.test(x)

library(nortest) # Shapiro-Francia normality test
sf.test(x)

library(nortest)
pearson.test(x) # Pearson chi-square normality test

