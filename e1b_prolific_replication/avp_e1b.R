## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV PERSPECTIVE STUDY | EXPERIMENT 1b             
## ================================================================================================================
## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load("ggplot2",         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # probably not using..
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'lsmeans',         # contrast analysis for regression models
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
               'rstudioapi', 
               'tidyverse', 
               'mediation',
               'lavaan', 
               'semPlot', 
               'sjPlot',
               'corrplot',
               'RColorBrewer',
               'yarrr',
               'ordinal',
               'brms',
               'dplyr',
               'data.table'
)

mediation <- TRUE #whether to run mediation analyses

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# set working directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('avp_e1b_anon.csv') 

## rename variables:
names(d)[names(d) == 'FL_8_DO'] <- 'cond'
names(d)[names(d) == 'Q142'] <- 'prolificID'
names(d)[names(d) == 'comp_0'] <- 'comp_1'

## change condition entries
d$cond[d$cond == "FL_53"] <- "pref_others"
d$cond[d$cond == "FL_51"] <- "pref_you"

## subjects randomized:
table(d$cond)

##age to numeric
d$age = as.numeric(d$age)

## ================================================================================================================
##                                                   EXCLUSIONS                
## ================================================================================================================

## number of participants BEFORE exclusions: 
 
## attention exclusions: 
# remove responses from data frame that failed attention checks
d <- subset(d, (d$att_1 == 4 & d$att_2 == 1))
n_original <- dim(d)[1]; n_original # extracting number of rows only, not columns

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp_1 == 1 & d$comp_2 == 2 & d$comp_3 == 4))
dim(d) # number of participants should decrease after comprehension exclusions

#remove incomplete responses
d <- d[complete.cases(d[, c("age")]), ]

## number of participants AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final 
percent_excluded <- (n_original - n_final)/n_original 
percent_excluded
table(d$cond)

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

#rearrange data
d <- d %>% relocate(pref_others, .after = pref_you)
d <- d %>% relocate(pref_others_explain, .after = pref_you_explain)
d <- d %>% relocate(safety_others_1, .after = safety_you_1)
d <- d %>% relocate(trust_others_1, .after = trust_you_1)
colnames(d)

## new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 5))
colnames(d_subset) <- c('cond','pref', 'explain', 'safety', 'trust')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract data of interest from middle part of raw data:
for(i in 1:dim(d)[1]) {
  curr <- d[i,23:26][!is.na(d[i,23:26])] # for a given row, get only the non-NA values
  d_subset[i,2:3] <- as.numeric(curr[curr!= ""]) # and only the non-empty values
  curr1 <- d[i,29:30][!is.na(d[i,29:30])] # for a given row, get only the non-NA values
  d_subset[i,4] <- as.numeric(curr1[curr1!= ""])
  curr2 <- d[i,31:32][!is.na(d[i,31:32])] # for a given row, get only the non-NA values
  d_subset[i,5] <- as.numeric(curr2[curr2!= ""])
  d_subset[i,1] <- d[i,43][!is.na(d[i,43])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,33:42])
d_merged$ss <- 1:dim(d_merged)[1]

# make pref_you as grp 1 and pref_other as grp 2
str(d_merged)
d_merged$cond <- as.factor(d_merged$cond)
d_merged$cond <- factor(d_merged$cond, levels = c('pref_you','pref_others'))
levels(d_merged$cond)

# reverse code trust scale to make it trust in you/other drivers relative to AVs
d_merged$trust <- 100 - d_merged$trust

#check for correlation between trust and safety
cronbach.alpha(d_merged[,c("safety", "trust")])

# cronbach's alpha = 0.79 so measures can be averaged
d_merged <- d_merged %>%
  mutate(avgtrust = (safety + trust) / 2)
## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(d$age, trim = 0, na.rm = TRUE) ## mean age 
hist(d$age, main = "Histogram for Age", xlab = "Age", ylab = "Frequency")

## gender
table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
table(d$gender)[2]/sum(table(d$gender)) ## percentage of females
barplot(table(d$gender), main = "Bar Chart for Gender", xlab = "Gender", names.arg = c("Male","Female","Undisclosed","Other"))

## av knowledge
mean(d$ai_knowledge_1, trim = 0, na.rm = TRUE) ## mean av knowledge 
hist(d$ai_knowledge, main = "Histogram for AV Knowledge", xlab = "AV Knowledge", ylab = "Frequency")

## license
table(d$license)[1]/sum(table(d$license)) ## percentage with driver's license
table(d$license)[2]/sum(table(d$license)) ## percentage without driver's license
barplot(table(d$license), main = "Bar Chart for License", xlab = "License", names.arg = c("Yes","No"))

## by condition
table(d$cond)
table_gen <- table(d_merged$gender, d_merged$cond)
gen2_self <- table_gen[2,1]/sum(table_gen[,1])*100; gen2_self
gen2_others <- table_gen[2,2]/sum(table_gen[,2])*100; gen2_others
aggregate(age ~ cond, data = d_merged, mean)

## ================================================================================================================
##                                             DATA ANALYSIS - T-TESTS and MEDIATION            
## ================================================================================================================

## check for normal distribution (shapiro-wilk normality test) and homegeneity of variance
##ADDED IN RESPONSE TO NON-PARAMETRIC NATURE OF DV
shapiro.test(d_merged$pref)
leveneTest(pref ~ cond, d_merged)

t = as.factor(d_merged$cond) # turn into factor
d_merged$cond = as.numeric(t) # set numeric of factor to variable

## PREFERENCE
## summary statistics
d_merged %>%
  group_by(cond) %>%
  get_summary_stats(pref, type = "mean_sd")

## two sample t-test
t.test(pref ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(avgtrust ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)

# separate t-tests for safety and trust
t.test(trust ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(safety ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)

## effect size
d_merged %>% cohens_d(pref ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(avgtrust ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(trust ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(safety ~ cond, paired = FALSE, var.equal = TRUE)

## non-parametric t-test
wilcox.test(pref ~ cond, data = d_merged, paired = FALSE)

## linear models
d_merged$pref2 <- factor(d_merged$pref, ordered = TRUE) #ORDINAL VARIABLE NEEDS TO BE FACTORED FOR CLM
pref_lm1 <- clm(pref2 ~ cond + avgtrust, data = d_merged)  #model 1
summary(pref_lm1)

## ================================================================================================================
##                                            DATA ANALYSIS - MEDIATION                
## ================================================================================================================

if(mediation) {
    source('../common_functions/process.r')
    d_merged$cond = as.numeric(d_merged$cond)
    
    ## PARALLEL MEDIATION:
    process(data = d_merged, y = "pref", x = "cond", 
            m =c("avgtrust"), model = 4, effsize = 1, total = 1, stand = 1, 
            contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
    
    process(data = d_merged, y = "pref", x = "cond", 
            m =c("age", "ai_knowledge_1"), model = 4, effsize = 1, total = 1, stand = 1, 
            contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
  
}

## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                 
## ================================================================================================================
t_names <- c("Self", "Others")
d_merged$pref1 <- d_merged$pref - 1 # only for the purpose of the plot, to match automation levels to SAE's official levels

## (1) PREFERENCE
p1 <- ggplot(d_merged,aes(x=factor(cond),y=pref1)) +  
  theme_bw() +coord_cartesian(ylim=c(1,7))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_you", "pref_others")),map_signif_level=TRUE, y_position = c(6.5), textsize = 14)

p1

p1 <- p1 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Automation Preference") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=60)) +
  theme(axis.text.y = element_text(size=60)) +
  theme(axis.text.x = element_text(size=50)) +
  theme(axis.text.y = element_text(size=50)) +
  theme(plot.title = element_text(size=60, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=2, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 1.5),
               geom="errorbar", width = 0.4)
p1 

## ================================================================================================================

## (2) TRUST
p2 <- ggplot(d_merged,aes(x=factor(cond),y=trust)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_others", "pref_you")),map_signif_level=TRUE, y_position = c(100), textsize = 14)

p2 <- p2 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Trust") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=60)) +
  theme(axis.text.y = element_text(size=60)) +
  theme(axis.text.x = element_text(size=50)) +
  theme(axis.text.y = element_text(size=50)) +
  theme(plot.title = element_text(size=60, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +  
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=2, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 1.5),
               geom="errorbar", width = 0.4)
p2

## ================================================================================================================

## (3) SAFETY
p3 <- ggplot(d_merged,aes(x=factor(cond),y=safety)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_others", "pref_you")),map_signif_level=TRUE, textsize = 14)

p3 <- p3 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Safety") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=60)) +
  theme(axis.text.y = element_text(size=60)) +
  theme(axis.text.x = element_text(size=50)) +
  theme(axis.text.y = element_text(size=50)) +
  theme(plot.title = element_text(size=60, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +  
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=2, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 1.5),
               geom="errorbar", width = 0.4)
p3

## ================================================================================================================
##                                                ADDING CONTROLS
## ================================================================================================================

## Control for driver's license
##subset population that has a drivers license
d_merged2 <-  d_merged[d_merged$license == "1", ]
dim(d_merged2)

mean(d_merged2$age)
table(d_merged2$gender)[2]/sum(table(d_merged$gender))

##perform t-tests on subset
wilcox.test(pref ~ cond, data = d_merged2)
t.test(pref ~ cond, data = d_merged2, paired = FALSE, var.equal = TRUE)
t.test(avgtrust ~ cond, data = d_merged2, paired = FALSE, var.equal = TRUE)

d_merged2 %>% cohens_d(pref ~ cond, paired = FALSE, var.equal = TRUE)
d_merged2 %>% cohens_d(avgtrust ~ cond, paired = FALSE, var.equal = TRUE)

if(mediation) {
process(data = d_merged2, y = "pref", x = "cond", 
        m =c("avgtrust"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}

## RESULTS REMAIN UNCHANGED IF TESTED FOR PEOPLE WHO HAVE A DRIVERS LICENSE

##subset population that do not have a drivers license
d_merged3 <-  d_merged[d_merged$license == "2", ]

##perform t-tests on subset
t.test(pref ~ cond, data = d_merged3, paired = FALSE, var.equal = TRUE)
t.test(avgtrust ~ cond, data = d_merged3, paired = FALSE, var.equal = TRUE)

#### MAIN EFFECT DISSAPEARS IF TESTED FOR PEOPLE WHO DO NOT HAVE A DRIVERS LICENSE

## Control for familiarity with AVs
pref_lm2 <- clm(pref2 ~ cond + ai_knowledge_1, data = d_merged) # model 2
summary(pref_lm2)

## Control for familiarity with AVs with safety-trust
pref_lm3 <- clm(pref2 ~ cond + avgtrust + ai_knowledge_1, data = d_merged) #model 3
summary(pref_lm3) 

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================

