##########################################################
#
# Program: DetailedExampleRCode.R
#
# Author: Martin Piotrowski
#
# Date Created: 1/15/2018
#
# Purpose: Demostrate how to use R for basic Statistics
#
##########################################################

###############################
### Pre-Programming Options ###
###############################

#Remove All Objects
rm(list=ls())

#Set Working Directory
setwd("C:/Users/Martin/Documents/Work/Courses/statistics/new/DetailedExamples/R_Programs")
getwd()

#####################
### Bring in Data ###
#####################

#Bring in Foreign Library
library(foreign)

dataset = read.spss("GSS2012.sav",
                    to.data.frame = TRUE)

#Note: some functions below operate only on numeric objects
#need to convert non-numeric (e.g., factors) to numeric
#EX: dataset$SEX = as.numeric(dataset$SEX)

#View Data Set
names(dataset)
head(dataset)

###################################
### Organization of Information ###
###################################

### Univariate Frequency Table for Education ###

#Check for Missing
summary(dataset$EDUC)         # 2 NA's

educ.total = nrow(subset(dataset, (!is.na(dataset$EDUC))))

print(educ.total)

#FrequencY Table (Hardcoded)
educ.freq = table(dataset$EDUC)             # Frequency Distribution
cum.freq = cumsum(educ.freq)                # Cumulative Frequency
perc = table(dataset$EDUC)/educ.total * 100 # Percent Distribution
cum.perc = cumsum(perc)                     # Cumulative Percent

print(cbind(educ.freq, cum.freq, perc, cum.perc), digits = 1)

#Publication Quality Tables
#install.packages("stargazer")

library(stargazer)
#help("stargazer")

stats.list = data.frame(
  Freq = as.matrix(educ.freq), # Bring in as Matrix because originally table
  CumFreq = cum.freq, 
  Perc = as.matrix(perc),      # Bring in as Matrix because originally table
  CumPerc = cum.perc)

names(stats.list)
print(stats.list)

#?stargazer

column.names = c("Freq", "Cum. Freq", "Perc", "Cum. Perc")

stargazer(stats.list, summary = FALSE,
          type="text",
          title="Table 1. Univariate Frequency of Highest Year of School",
          notes = c("N = 1,972", "Source: GSS 2012"), digits = 2)

##############################
### Graphical Presentation ###
##############################

names(dataset)

### Pie Chart of RACE ###

#1) Frequency Table
race.freq = table(dataset$RACE)

print(race.freq)
sum(race.freq)

#2) Value Labels (May Not Need; Some Datasets Already Include)
race.labels = c("White", "Black", "Other")

#3) Actual Plot
pie(race.freq,
    labels = race.labels,
    col = rainbow(length(race.freq)),
    main = "Figure 1. Pie Chart of Race",
    sub = "N=1,974\nSource: GSS 2012\n")

# 3D Pie Chart #

#Install "Plotrix Package"
#install.packages("plotrix")

library(plotrix)

pie3D(race.freq, 
      labels = race.labels,
      col = rainbow(length(race.freq)),
      explode = 0.1,
      main = "Figure 1b. Pie Chart of Race",
      sub = "N=1,974\nSource: GSS 2012\n")

### Bar Graph of Class ###

#1) Frequency Table
class.freq = table(dataset$CLASS)

print(cbind(class.freq))
print(sum(class.freq))

#2) Value Labels (May Not Need; Some Datasets Already Include)
class.labels = c("Lower", "Working", "Middle", "Upper")

?title

#3) Actual Plot
barplot(class.freq,
        xlab = "Subjective Class Identification\n",
        ylab = "Frequency",
        names.arg = class.labels, 
        main = "Figure 2. Bar Graph of Subjective Class Identification")
title(sub = "N=1,957\nSource: GSS 2012", adj=0) # adj= 0 is left align
        
### Histogram of Education ###

hist(dataset$EDUC,
     breaks = 21,
     xlab = "Education (in Years)\n",
     ylab = "Frequency",
     main = "Figure 3. Histogram of Years of Education")
title(sub = "N=1,972\nSource: GSS 2012", adj=0) # Total calculated below 

### Line Graph of Education ###

#1) Frequency Table
educ.freq = table(dataset$EDUC)

print(cbind(educ.freq))
print(sum(educ.freq))

#2) Actual Plot
plot(educ.freq,
     type = "l",     
     xlab = "Education (in Years)\n",
     ylab = "Frequency",
     main = "Figure 4. Line Graph of Years of Education")
title(sub = "N=1,972\nSource: GSS 2012", adj=0)

####################################
### Measures of Central Tendency ###
####################################

### Mean, Median, Mode for Sex, Class, Education ###

names(dataset)

## Mean and Median ##

mean(dataset$EDUC, na.rm=TRUE)
median(dataset$EDUC, na.rm=TRUE)

#Make New Dataset with Subset of Variables
myvars = c("SEX", "CLASS", "EDUC")

newdata = dataset[myvars]

names(newdata)

myvars.means = sapply(newdata, mean, na.rm=TRUE)
myvars.md = sapply(newdata, median, na.rm=TRUE)

## Mode ##

#Create mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

myvars.mode = sapply(newdata, getmode) 

## Total count ##

myvars.total = sapply(newdata, function(x) length(which(!is.na(x))))

print(rbind(myvars.means, myvars.md, myvars.mode, myvars.total), digits = 3) 

#Publication Quality Table
cent.tend = data.frame(
  Mean = myvars.means,
  Median = myvars.md,
  Mode = myvars.mode,
  N = myvars.total)

#library(stargazer)
stargazer(cent.tend, 
          type = "text",
          summary = FALSE,
          title="Table 1. Measures of Central Tendency for Variables",
          notes = "Source: GSS 2012", digits = 2)

#Alternative Way (Not as Complete)
stargazer(newdata, 
          type = "text",
          summary.stat = c("n", "mean", "median"),
          title="Table 1b. Measures of Central Tendency for Variables",
          notes = "Source: GSS 2012", digits = 2)
  
###############################
### Measures of Variability ###
###############################

names(dataset)

## Index of Qualitative Variation ##

iqv <- function(var) {
  table = table(var)  # Make Table, Get Frequencies for each Category
  n = sum(table(var)) # Total Number of Cases
  perc.sqr = 0        # Initialize Values
  sum.perc.sqr = 0
  for(i in 1:nrow(table)) {                   # For loop -- all values of var
    perc[i] = table[i] / n * 100              # Calculate Percentage
    perc.sqr[i] = perc[i]^2                   # Square Percentage
    sum.perc.sqr = sum.perc.sqr + perc.sqr[i] # Increment Sum of Square Percent
    }
  k = i
  #print(k)
  #print(n)
  #print(sum.perc.sqr)
  result = ((k*(10000 - sum.perc.sqr))/(10000*(k-1))) # Equation
  print(result)
}

#table(dataset$CLASS)

iqv(dataset$CLASS)

## Measures of Variability for Education ##

#Min, Max, Mean, Median, and Quartiles 
print(cbind(summary(dataset$EDUC)), digits = 2)

#Variance
var(dataset$EDUC, na.rm = TRUE)

#Standard Deviation
sd(dataset$EDUC, na.rm = TRUE)

## By Group ##

by(dataset$EDUC, dataset$SEX, function(x) summary(x, na.rm = TRUE))

## Box Plot ##

#Total Number of Cases
print(sum(table(dataset$EDUC)))

#Box Plot of Education
boxplot(dataset$EDUC,
        ylab = "Education (in Years)",
        main = "Figure 1. Box Plot of Highest Degree Earned",
        sub = "N = 1,972\nSource: GSS 2012\n")

#Vector of Labels for Sex
sex.labels = c("Male", "Female")

#Total N for Education by Sex
by(dataset$EDUC, dataset$SEX, function(x) length(which(!is.na(x))))

#Box Plot of Education by Sex
boxplot(dataset$EDUC ~ dataset$SEX,
        names = sex.labels,
        ylab = "Education (in Years)",
        main = "Figure 2. Box Plot of Highest Degree Earned by Sex")
title(sub = "N = 884 (Men) & 1,088 (Women)\nSource: GSS 2012\n", adj = 0)

##########################
### Bivariate Analysis ###
##########################

names(dataset)

## Recode Missing Values ##

#Install car package
#install.packages("car")
library(car)

attach(dataset) # Programming convenience -- automatic reference to dataset

#Recodes
health = recode(HEALTH, "c(0,8,9)=NA")
happy  = recode(HAPPY, "c(8,9)=NA")

table(health)
table(happy)

# Designate Variables As Ordered Variables, Assign Levels
health = ordered(health, 
                 levels = c(1, 2, 3, 4),
                 labels = c("Excellent", "Good", "Fair", "Poor"))

happy = ordered(happy, 
                levels = c(1, 2, 3),
                labels = c("Very Happy", "Pretty Happy", "Not Too Happy"))

#Contingency Table of Happiness by Health (table function)
table(happy, health)

mytable = table(happy, health)
print(prop.table(mytable), digits = 1)      # Cell Percentages
print(prop.table(mytable, 1), digits = 1)   # Row Percentages
print(prop.table(mytable, 2), digits = 1)   # Col Percentages

## Alternative Approach ##

#Install "gmodels" package
#install.packages("gmodels")
library(gmodels)

#?CrossTable

#Contingency Table of Happiness by Health (CrossTable function)
CrossTable(happy, health, # Notice Variable Order Reversed
           prop.c = TRUE,
           prop.r = FALSE,
           prop.chisq = FALSE,
           prop.t = FALSE,
           format=c("SPSS"))

detach(dataset) # Housekeeping attach paired with detach

###############################
### Measures of Association ###
###############################

names(dataset)

attach(dataset)

## Recode ##

library(car)

abany = recode(ABANY, "c(0, 8, 9) = NA")

## Lambda: Abortion Attitude and Sex ##

#1) Create Table
mytable = table(abany, SEX)

#Install "DescTools" Package
#install.packages("DescTools")
library(DescTools)

#2) Calculate Lambda
Lambda(mytable, direction = "column") # Direction specifies where independent variable located

## Gamma: Health and Happiness ##

mytable = table(happy, health)

GoodmanKruskalGamma(mytable)

detach(dataset)

##################################
### Covariance and Correlation ###
##################################

names(dataset)

attach(dataset)

## Scatterplot of Education by Father's Education ##

plot(PAEDUC, EDUC,
     main = "Figure 1. Scatter Plot of Education by Father's Education",
     xlab = "Highest Year of School Completed, Father\n",
     ylab = "Higest Year of School Completed")
title(sub = "N = 1,439\nSource: GSS 2012", adj = 0)

## Covariance of Education by Father's Education ##

cov(PAEDUC, EDUC, use = "pairwise.complete.obs")

## Correlation of Education by Father's Education ##

cor(PAEDUC, EDUC, use = "pairwise.complete.obs")

detach(dataset)

## Correlation of More than Two Variables ##

myvars = c("PAEDUC", "EDUC", "AGE")

cor(dataset[myvars], use = "pairwise.complete.obs")

#########################
### Linear Regression ###
#########################

names(dataset)

#Estimate Regression Function
model1 = lm(EDUC ~ PAEDUC, data = dataset)

#Examine output
summary(model1)

#Residuals
model1.res = resid(model1)

#Plot Residuals (Histogram)
hist(model1.res,
     main = "Figure 1. Residual Plot for Education Regression",
     xlab = "Residuals")

#Predicted Value (When Father's Education = 12)
newdata = data.frame(PAEDUC=12)               # Make new data frame

predict(model1, newdata, interval="predict")  # Prediction interval with 12 years of father's education

#Scatter Plot with Regression Line
plot(dataset$PAEDUC, dataset$EDUC,
     main = "Figure 1. Scatter Plot of Education by Father's Education",
     xlab = "Highest Year of School Completed, Father\n",
     ylab = "Highest Year of School Completed")
title(sub = "N = 1,439\nSource: GSS 2012", adj = 0)
abline(model1)

###########################
### Multiple Regression ###
###########################

names(dataset)

### Estimate Regression Equations (Models) ###

#Simple Regression of Education on Father's Education
model1 = lm(EDUC ~ PAEDUC, data = dataset)
summary(model1)

#Add Age to Model
model2 = lm(EDUC ~ PAEDUC + AGE, data = dataset)
summary(model2)

#Add Age Square to Model
model3 = lm(EDUC ~ PAEDUC + AGE + I(AGE^2), data = dataset)
summary(model3)

#Install Margins
#install.packages("margins")
library(margins)

#Average Marginal Effect
margins(model3, at=list(AGE = 18:89))

#Plot of Prediction Interval for Age
cplot(model3, "AGE", what = "prediction", 
      main = "Figure 1. Prediction Interval for Age")

#Add Sex (as Factor)
model4 = lm(EDUC ~ PAEDUC + AGE + I(AGE^2) + factor(SEX), data = dataset)
summary(model4)

#Add Race Dummies (as Factors)
model5 = lm(EDUC ~ PAEDUC + AGE + I(AGE^2) + factor(SEX) + factor(RACE), data = dataset)
summary(model5)

#Table

library(stargazer)

variable.labels = c("Father's Education", "Age", "Age Square", 
                    "Male", "Black", "Other Race", "Intercept")

stargazer(model1, model2, model3, model4, model5,
          type="text",
          title="Table 1. Linear Regression Models of Education",
          dep.var.labels = "Education", dep.var.caption = "", 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          model.numbers = FALSE,
          covariate.labels = variable.labels,
          out="table1.txt", keep.stat = c("n", "rsq"), 
          digits = 2, star.cutoffs = c(.05, .01, .001),
          single.row = TRUE, notes = "Source: GSS 2012")

###########################
### Normal Distribution ###
###########################

#Plot of Standard Normal Curve
curve(dnorm(x,0,1),
      xlim = c(-4,4))

#Plot of Standard Normal Curve, Shading left tail from -4 to -2 SD
cord.x1 <- c(-4,seq(-4,-2,0.01),-2) 
cord.y1 <- c(0,dnorm(seq(-4,-2,0.01)),0) 

curve(dnorm(x,0,1),xlim=c(-4,4),
      main='Standard Normal') 
polygon(cord.x1,cord.y1,col='grey')

#Plot of Standard Normal Curve, Shading Right tail from 2 to 4 SD
cord.x2 <- c(2,seq(2,4,0.01),4) 
cord.y2 <- c(0,dnorm(seq(2,4,0.01)),0) 

curve(dnorm(x,0,1),xlim=c(-4,4),
      main='Standard Normal') 
polygon(cord.x2,cord.y2,col='grey')

#Plot of Standard Normal Curve, Shading Bothtails from 2 to 4 SD
curve(dnorm(x,0,1),xlim=c(-4,4),
      main='Standard Normal') 
polygon(cord.x1,cord.y1,col='grey')
polygon(cord.x2,cord.y2,col='grey')

##################
### Estimation ###
##################

names(dataset)

## Confidence Intervals for the Mean of Education ##

#Install Rmisc Package
#install.packages("Rmisc")
library(Rmisc)

#Check for Missing (NA)
summary(dataset$EDUC)

newdata = subset(dataset, !is.na(dataset$EDUC), select=c(EDUC))

names(newdata)

#95% CI
CI(x=newdata$EDUC, ci = 0.95)

#99% CI
CI(x=newdata$EDUC, ci = 0.99)

## Confidence Intervals for Proportion Male ##

summary(dataset$SEX)

library(car)

#Recode Sex to Dummay Variable Male
dataset$male = recode(dataset$SEX, "1=1; 2=0") # Remember to load 'car' package if using new session

#Determine Proportion Male and Total
table(dataset$male)
sum(table(dataset$male))

#Calculate CI
binom.test(886, 1974,
           0.5,                      # p = 0.5, hypothesized probability of success
           alternative="two.sided",
           conf.level=0.95)

##########################
### Hypothesis Testing ###
##########################

names(dataset)

#Two-Tailed Test, 0.05 alpha
t.test(x=dataset$EDUC, 
       alternative = "two.sided",   # Other Alternatives: "greater", "less"
       mu = 12,
       conf.level = 0.95)

#One-Tailed Test, 0.01 alpha
t.test(x=dataset$EDUC, 
       alternative = "greater", 
       mu = 12,
       conf.level = 0.99)

##################
### Chi-Square ###
##################

names(dataset)

attach(dataset)

library(car)

#Recodes
health = recode(HEALTH, "c(0,8,9)=NA")
happy  = recode(HAPPY, "c(8,9)=NA")

table(health)
table(happy)

# Designate Variables As Ordered Variables, Assign Levels
health = ordered(health, 
                 levels = c(1, 2, 3, 4),
                 labels = c("Excellent", "Good", "Fair", "Poor"))

happy = ordered(happy, 
                levels = c(1, 2, 3),
                labels = c("Very Happy", "Pretty Happy", "Not Too Happy"))

chisq.test(x=table(happy, health))

detach(dataset)