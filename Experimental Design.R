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


#############################################################################
# assumption 2: The dependent variable should not contain any outliers #####
#############################################################################

# How to check for outliers

boxplot(x)
outliers_values=boxplot.stats(x)$out
outliers_values


# IQR to Detect Outliers

summary(x)
summary(x)[5]
summary(x)[2]
IQR=summary(x)[5]-summary(x)[2]
IQR # Interquartile range

up_out=summary(x)[5]+1.5*IQR
down_out=summary(x)[2]-1.5*IQR

up_out
down_out

max(x)
min(x)

which(x>up_out)
which(x<down_out)

out_data=c(which(x<down_out), which(x>up_out))
out_data

length(x)
x[out_data]

x_new=x[-out_data]
length(x_new)

boxplot(x_new)
outliers_values_new=boxplot.stats(x_new)$out
outliers_values_new

# Using Z-scores to Detect Outliers

z=(x-mean(x))/sd(x)
summary(z)
boxplot(z)

which(z > 2.5)
which(z < -2.5)

out_data_z=c(which(z > 2.5), which(z < -2.5))
out_data_z

x_new2=x[-out_data_z]
length(x_new2)


# create a function to find and remove outliers automatically using z score

remove_outliers_z=function(x, value=2.5){
  z=(x-mean(x))/sd(x)
  out_data_z=c(which(z > value), which(z < -value))
  x_new=x[-out_data_z]
  print(length(x_new))
  boxplot(x_new)
  summary(x_new)
  par(mfrow=c(1,2))
  boxplot(x_new, main="New Distribution BoxPlot")
  hist(x_new, main="New Distribution Histogram")
}

remove_outliers_iqr=function(x){
  IQR=summary(x)[5]-summary(x)[2]
  up_out=summary(x)[5]+1.5*IQR
  down_out=summary(x)[2]-1.5*IQR
  out_data=c(which(x<down_out), which(x>up_out))
  x_new=x[-out_data]
  print(length(x_new))
  boxplot(x_new)
  summary(x_new)}

?runif
?rbeta

# x=runif(300, min=0, max=100)
# x=sample(1:10000, 1000, replace=T)

x=rbeta(1000, 3, 0.6)

hist(x)
boxplot(x)

remove_outliers_z(x)
remove_outliers_z(x,3)
remove_outliers_z(x,1)

boxplot(x)
remove_outliers_iqr(x)

########################################################################
# One sample t-test - One-tailed hypothesis tests
########################################################################

?t.test

t.test(x, mu = 0, alternative = "greater")
t.test(x, mu = 0, alternative = "greater",  conf.level = 0.90) # set a different 1????? level


# One sample t-test - Using a Two-tailed hypothesis tests
t.test(x, mu = 0, alternative = "two.sided")
t.test(x, mu = 0, alternative = "two.sided", conf.level = 0.90) # set a different 1????? level


# Real Example

cats_sample_weight=rnorm(49, mean=4, sd=5)
hist(cats_sample_weight)

# we can check the outliers before to do the test
boxplot(cats_sample_weight)

sd(cats_sample_weight)
mean(cats_sample_weight)
t=(mean(cats_sample_weight)-7)/(sd(cats_sample_weight)/7)

t.test(cats_sample_weight, mu = 7, alternative = "two.sided")

# if we know sd pop - for example is equal to sd sample
library(DescTools)
?ZTest

sd_pop=sd(cats_sample_weight)
ZTest(cats_sample_weight, mu=7, sd_pop=sd_pop)


#############################################################################
## unpaired two samples t test with similar variance (homoscedastic case) ###
#############################################################################

# create two random groups
x1<-rnorm(1000, mean=3, sd=0.98)
x2<-rnorm(1000, mean=4, sd=1)

boxplot(x1, main="First group")
boxplot(x2, main="Second group")

# assumption 1: the data are normally distributed
shapiro.test(x1)
shapiro.test(x2)

# assumption 2: samples have "equal" variances
# rough check: in general, the ratio between the SDs must not be greater than 2
sd(x1)
sd(x2)
sd(x2)/sd(x1)  

# testing the homogeneity of variances
# low P-values rejection H0, high P-values I accept homogeneity variances
# need of dataframe

values=c(x1,x2)
length(values)
group_label=c(rep("A",length(values)/2),rep("B",length(values)/2))
group_label=as.factor(group_label)
class(group_label)

data=cbind(values, group_label)
head(data)
tail(data)

bartlett.test(values, group_label)
fligner.test(values, group_label)

# the null is that the means are equal
t.test(x1, x2, alternative = "two.sided", var.equal = TRUE)

# nice function: get the function "rquery.t.test" 
source('http://www.sthda.com/upload/rquery_t_test.r')
rquery.t.test(x1, x2)




