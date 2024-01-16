## ===================================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ===================================================================================================================================
##                                DATA ANALYSIS | AV PERSPECTIVE STUDY | PRETEST 1               
## ===================================================================================================================================
## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
library(dplyr)
library(ordinal)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # probably not using..
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'emmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'nlme',            # get p values for mixed effect model
               'DescTools',        # get Cramer's V
               'rstatix',
               'effects',
               'car'
)

## ===================================================================================================================================
##                                                  PRE-PROCESSING                 
## ===================================================================================================================================

## read in data: 
# set working directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('avp_pretest.csv') 

## explore data frame: 
head(d)
str(d)
dim(d) # dimensions of data frame by row [1] and column [2]
colnames(d) # all column names
summary(d)

## ===================================================================================================================================
##                                                   EXCLUSIONS                
## ===================================================================================================================================

## number of participants BEFORE exclusions: 
dim(d)[1] 

## attention exclusions: 
# remove responses from data frame that failed attention checks
d <- subset(d, (d$att_1 == 4 & d$att_2 == 1))
dim(d) # number of participants should decrease after attention exclusions
n_original <- dim(d)[1]
n_original
mean(d$age, trim = 0, na.rm = TRUE) # mean age for original dataset
table(d$gender)[2]/sum(table(d$gender)) # female percentage for original dataset

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp == 2))
dim(d) # number of participants should decrease after comprehension exclusions

## number of participants AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final 
percent_excluded <- (n_original - n_final)/n_original 
percent_excluded
table(d$cond)

## ===================================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ===================================================================================================================================

## age
mean(d$age, trim = 0, na.rm = TRUE) ## mean age 
hist(d$age, main = "Histogram for Age", xlab = "Age", ylab = "Frequency")

## gender
table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
table(d$gender)[2]/sum(table(d$gender)) ## percentage of females
barplot(table(d$gender), main = "Bar Chart for Gender", xlab = "Gender", names.arg = c("Male","Female","Other"))

## license
table(d$license)[1]/sum(table(d$license)) ## percentage with driver's license
table(d$license)[2]/sum(table(d$license)) ## percentage without driver's license
barplot(table(d$license), main = "Bar Chart for License", xlab = "License", names.arg = c("Yes","No"))

#### by condition
table(d$cond)
table_gen <- table(d_merged$gender, d_merged$cond)
gen2_self <- table_gen[2,1]/sum(table_gen[,1])*100 #percent female in you condition
gen2_others <- table_gen[2,2]/sum(table_gen[,2])*100 #percent female in others condition
aggregate(age ~ cond, data = d_merged, mean)

## ===================================================================================================================================
##                                            DATA EXPLORATION 
## ===================================================================================================================================

# Chi Squared Test
contingency_table <- table(d$pretest_1)
chisq.test(contingency_table)

# Bar plot 
barplot(table(d$pretest_1), main = "Bar Chart", xlab = "Other People", names.arg = c("Yes","No"))

##----------------------------------------------------------------------------------------------------------------------------------

