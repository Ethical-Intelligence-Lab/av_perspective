## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV PERSPECTIVE STUDY | EXPERIMENT 1a               
## ================================================================================================================
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
library(corrplot)
library(RColorBrewer)

mediation <- TRUE #whether to conduct mediation analyses

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# set working directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('avp_e1a_anon.csv') 

## rename variables:
names(d)[names(d) == 'FL_8_DO'] <- 'cond'

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
d <- subset(d, (d$att_2 == 6 & d$att_3 == 4 & d$att_4 == 1))
dim(d) # number of participants should decrease after attention exclusions
n_original <- dim(d)[1]; n_original
mean(d$age, trim = 0, na.rm = TRUE) # mean age for original dataset
table(d$gender)[2]/sum(table(d$gender)) # female percentage for original dataset

#remove incomplete response
# Remove rows with missing values for 'var1' and 'var2'
d <- d[complete.cases(d[, c("ai_knowledge_1")]), ]

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp_1 == 1 & d$comp_2 == 2 & d$comp_3 == 2))
dim(d) # number of participants should decrease after comprehension exclusions
##clean

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
d_subset <- array(dim=c(dim(d)[1], 4))
colnames(d_subset) <- c('cond','pref', 'safety', 'trust')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract data of interest from middle part of raw data:
for(i in 1:dim(d)[1]) {
  curr <- d[i,23:24][!is.na(d[i,23:24])] # for a given row, get only the non-NA values
  d_subset[i,2] <- as.numeric(curr[curr!= ""]) # and only the non-empty values
  curr1 <- d[i,29:30][!is.na(d[i,29:30])] # for a given row, get only the non-NA values
  d_subset[i,3] <- as.numeric(curr1[curr1!= ""])
  curr2 <- d[i,31:32][!is.na(d[i,31:32])] # for a given row, get only the non-NA values
  d_subset[i,4] <- as.numeric(curr2[curr2!= ""])
  d_subset[i,1] <- d[i,45][!is.na(d[i,45])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,33:44])
d_merged$ss <- 1:dim(d_merged)[1]

# make pref_you as grp 1 and pref_other as grp 2
str(d_merged)
d_merged$cond <- as.factor(d_merged$cond)
d_merged$cond <- factor(d_merged$cond, levels = c('pref_you','pref_others'))
levels(d_merged$cond)

#check for correlation between trust and safety
cronbach.alpha(d_merged[,c("safety", "trust")])

# cronbach's alpha = 0.863 so measures can be averaged
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
barplot(table(d$gender), main = "Bar Chart for Gender", xlab = "Gender", names.arg = c("Male","Female"))

## av knowledge
mean(d$ai_knowledge_1, trim = 0, na.rm = TRUE) ## mean av knowledge 
hist(d$ai_knowledge, main = "Histogram for AV Knowledge", xlab = "AV Knowledge", ylab = "Frequency")

## license
table(d$license)[1]/sum(table(d$license)) ## percentage with driver's license
table(d$license)[2]/sum(table(d$license)) ## percentage without driver's license
barplot(table(d$license), main = "Bar Chart for License", xlab = "License", names.arg = c("Yes","No"))

#### by condition
table(d$cond)
table_gen <- table(d_merged$gender, d_merged$cond)
gen2_self <- table_gen[2,1]/sum(table_gen[,1])*100; gen2_self #percent female in you condition
gen2_others <- table_gen[2,2]/sum(table_gen[,2])*100; gen2_others #percent female in others condition
aggregate(age ~ cond, data = d_merged, mean)

## ================================================================================================================

##AGE
median(d_merged$age)

##GENDER
## 1 Male
## 2 Female
t_gen <- prop.table(table(d_merged$gender))
t_gen #proportion of males and females 

##INCOME
## 1	Less than $14,999
## 2	$15,000 to $19,999
## 3	$20,000 to $24,999
## 4	$25,000 to $29,999
## 5	$30,000 to $34,999
## 6	$35,000 to $39,999
## 7	$40,000 to $44,999
## 8	$45,000 to $49,999
## 9  $50,000 to $54,999
## 10	$55,000 to $59,999
## 11	$60,000 to $64,999
## 12	$65,000 to $69,999
## 13	$70,000 to $74,999
## 14	$75,000 to $79,999
## 15	$80,000 to $84,999
## 16	$85,000 to $89,999
## 17	$90,000 to $94,999
## 18	$95,000 to $99,999
## 19	$100,000 to $124,999
## 20	$125,000 to $149,999
## 21	$150,000 to $174,999
## 22	$175,000 to $199,999
## 23	$200,000 to $249,999
## 24	$250,000 and above
## -3105 Prefer not to answer
t_hhi <- prop.table(table(d$hhi)) # proportion of participants with different income levels
t_hhi[2] #less than 15,000
t_hhi[3] + t_hhi[4] # 15,000 to 24,999
t_hhi[5] + t_hhi[6] # 25,000 to 34,999
t_hhi[7] + t_hhi[8] + t_hhi[9] # 35,000 to 49,999
t_hhi[10] + t_hhi[11] + t_hhi[12] + t_hhi[13] + t_hhi[14] # 50,000 to 74,999
t_hhi[15] + t_hhi[16] + t_hhi[17] + t_hhi[18] + t_hhi[19] # 75,000 to 99,999
t_hhi[20] + t_hhi[21] # 100,000 to 149,999
t_hhi[22] + t_hhi[23] # 150,000 to 199,999
t_hhi[24] + t_hhi[25] # 200k and above

##ETHNICITY
## 1	White 
## 2	Black, or African American
## 3	American Indian or Alaska Native 
## 4	Asian *** Asian Indian 
## 5	Asian *** Chinese 
## 6	Asian *** Filipino 
## 7	Asian *** Japanese 
## 8	Asian *** Korean 
## 9	Asian *** Vietnamese 
## 10	Asian *** Other 
## 11	Pacific Islander *** Native Hawaiian 
## 12	Pacific Islander *** Guamanian 
## 13	Pacific Islander *** Samoan 
## 14	Pacific Islander *** Other Pacific Islander
## 15	Some other race 
## 16	Prefer not to answer 
t_eth <- prop.table(table(d$ethnicity)) # proportion of participants with different ethnicities
t_eth
t_eth[4] + t_eth[5] + t_eth[6] + t_eth[7] + t_eth[8] + t_eth[9] + t_eth[10] #proportion of asians

##HISPANIC
## 1	No , not of Hispanic, Latino, or Spanish origin
## 2	Yes, Mexican, Mexican American, Chicano
## 3	Yes, Cuban
## 4	Yes, another Hispanic, Latino, or Spanish origin *** Argentina 
## 5	Yes, another Hispanic, Latino, or Spanish origin *** Colombia 
## 6	Yes, another Hispanic, Latino, or Spanish origin *** Ecuador 
## 7	Yes, another Hispanic, Latino, or Spanish origin *** El Salvadore 
## 8	Yes, another Hispanic, Latino, or Spanish origin *** Guatemala 
## 9	Yes, another Hispanic, Latino, or Spanish origin *** Nicaragua 
## 10	Yes, another Hispanic, Latino, or Spanish origin *** Panama 
## 11	Yes, another Hispanic, Latino, or Spanish origin *** Peru 
## 12	Yes, another Hispanic, Latino, or Spanish origin *** Spain 
## 13	Yes, another Hispanic, Latino, or Spanish origin *** Venezuela 
## 14	Yes, another Hispanic, Latino, or Spanish origin *** Other Country
## 15	Prefer not to answer
t_hisc <- prop.table(table(d$hispanic)) # proportion of participants that are hispanic or not hispanic
t_hisc

## EDUCATION
## 1	Some high school or less 
## 2	High school graduate
## 3	Other post high school vocational training
## 4	Completed some college, but no degree
## 5	Associate's degree
## 6	Bachelor's degree 
## 7	Master's or professional degree
## 8	Doctorate degree
## -3105	None of the above
#bachelor or higher
prop.table(table(d$education)) # proportion of participants with different levels of education
prop.table(table(d_merged[d_merged$age > 25, "education"] == 1)) #less than high school
prop.table(table(d_merged[d_merged$age > 25, "education"] == 2)) # high school
prop.table(table(d_merged[d_merged$age > 25, "education"] == 4)) # some college 
prop.table(table(d_merged[d_merged$age > 25, "education"] == 6)) # bachelor's degree
prop.table(table(d_merged[d_merged$age > 25, "education"] == 7)) +
prop.table(table(d_merged[d_merged$age > 25, "education"] == 8)) # advanced degree

##POLITICAL PARTY
## 1	Strong Democrat
## 2	Not very strong Democrat
## 3	Independent Democrat
## 4	Independent - neither
## 5	Independent Republican
## 6	Other - leaning Democrat
## 7	Other - neither
## 8	Other - leaning Republican
## 9	Not very strong Republican
## 10	Strong Republican
t_pp <- prop.table(table(d_merged$political_party)) # proportion of participants with different political identities
t_pp[1] + t_pp[2] #democrats
t_pp[3] + t_pp[4] + t_pp[5] #independent
t_pp[9] + t_pp[10] #republican
t_pp[6] + t_pp[7] + t_pp[8] #other

##REGION
## 1	Northeast
## 2	Midwest
## 3	South
## 4	West
t_reg <- prop.table(table(d_merged$region)) 
t_reg #proportion of participants in different regions

## ================================================================================================================
##                                              DATA EXPLORATION - DISTRIBUTIONS                
## ================================================================================================================

## PREFERENCE
summary(d_merged$pref)

## check distribution
hist(d_merged$pref, 
     main="Automation Preference", 
     xlab="Automation Levels", 
     border="black", 
     col="light blue")

## check for normal distribution (shapiro-wilk normality test)
shapiro.test(d_merged$pref)
mean(d_merged$trust, trim = 0, na.rm = TRUE)
var(d_merged$trust, na.rm = TRUE)

## ================================================================================================================
##                                             DATA ANALYSIS - T-TESTS            
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
wilcox.test(pref ~ cond, data = d_merged) 

## linear models 1 and 2
d_merged$pref2 <- factor(d_merged$pref, ordered = TRUE) #ORDINAL VARIABLE NEEDS TO BE FACTORED FOR CLM
pref_lm1 <- clm(pref2 ~ cond + avgtrust, data = d_merged)  #appendix model 1
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
          m =c("trust","safety"), model = 4, effsize = 1, total = 1, stand = 1, 
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
  geom_signif(comparisons = list(c("pref_others", "pref_you")),map_signif_level=TRUE, y_position = c(6.5), textsize = 5.5)

p1

p1 <- p1 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Automation Preference") +
  xlab ("") + ylab ("Preferred level of automation") +
  theme_classic() +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p1 

## ================================================================================================================

p1_1 <- ggplot(d_merged, aes(x = factor(cond), y = pref1)) +
  coord_cartesian(ylim=c(0,5)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+ 
  geom_bar(stat = "summary", fun = "mean", fill = c("#56B4E9","#009E73"), color = 'black', alpha = 0.7, width = 0.3) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  geom_signif(comparisons = list(c("pref_you", "pref_others")),map_signif_level=TRUE, y_position = c(4.5), textsize = 25)+
  theme_classic() +
  labs(title = "Automation Preference", x = "", y = "Automation Preference") +
  scale_x_discrete(labels=t_names) +
  theme(plot.title = element_text(size=65, hjust=0.5, face = "bold")) +
  theme(axis.text = element_text(size = 50), axis.title = element_text(size = 50))

# Display the plot
p1_1

## ================================================================================================================

ggplot(d_merged, aes(x=pref, fill=cond)) + 
  geom_histogram(position="identity", alpha=0.5, bins=10) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  labs(title="Histogram Split Down the Middle", x="Preferred level of automation", y="Count") +
  theme_minimal()

## ================================================================================================================

## (2) AVERAGED TRUST-SAFETY
p2 <- ggplot(d_merged,aes(x=factor(cond),y=avgtrust)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_others", "pref_you")),map_signif_level=TRUE, y_position = c(100), textsize = 3.5)

p2 <- p2 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Averaged Trust-Safety") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +  
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p2

## ================================================================================================================

p2_1 <- ggplot(d_merged, aes(x = factor(cond), y = avgtrust)) +
  coord_cartesian(ylim=c(0,110)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("#56B4E9","#009E73"), color = 'black', alpha = 0.7, width = 0.3) +
  geom_point(aes(x = factor(cond), y = avgtrust), data = d_merged, color = "grey40", size = 1, position = 'identity', alpha = 0.5) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  geom_signif(comparisons = list(c("pref_you", "pref_others")),map_signif_level=TRUE, y_position = c(100), textsize = 25)+
  theme_classic() +
  labs(title = "Safety-Trust Index", x = "", y = "Safety-Trust in driving skills relative to AVs") +
  scale_x_discrete(labels=t_names) +
  theme(plot.title = element_text(size= 65, hjust=0.5, face = "bold")) +
  theme(axis.text = element_text(size = 50), axis.title = element_text(size = 50))

# Display the plot
p2_1

## ================================================================================================================

## (3) TRUST
p3 <- ggplot(d_merged,aes(x=factor(cond),y=trust)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_others", "pref_you")),map_signif_level=TRUE, y_position = c(100), textsize = 3.5)

p3 <- p3 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Trust") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +  
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p3

## ================================================================================================================

p3_1 <- ggplot(d_merged, aes(x = factor(cond), y = trust)) +
  coord_cartesian(ylim=c(5.2,110)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("white","grey"), color = 'black', alpha = 0.7, width = 0.3) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  geom_signif(comparisons = list(c("pref_others", "pref_you")),map_signif_level=TRUE, y_position = c(100), textsize = 5.5)+
  theme_classic() +
  labs(title = "Trust", x = "", y = "Trust in driving skills relative to AVs") +
  scale_x_discrete(labels=t_names) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

# Scatter plot
p3_1 <- p3_1 + geom_point(aes(x = factor(cond), y = trust), data = d_merged, color = "black", size = 1, position = 'identity', alpha = 0.5)

# Display the plot
p3_1

## ================================================================================================================

## (4) SAFETY
p4 <- ggplot(d_merged,aes(x=factor(cond),y=safety)) +  
  theme_bw() +coord_cartesian(ylim=c(0,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_others", "pref_you")),map_signif_level=TRUE, textsize = 5.5)

p4 <- p4 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Safety") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +  
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p4

## ================================================================================================================

p4_1 <- ggplot(d_merged, aes(x = factor(cond), y = safety)) +
  coord_cartesian(ylim=c(5.2,110)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("white","grey"), color = 'black', alpha = 0.7, width = 0.3) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  geom_signif(comparisons = list(c("pref_others", "pref_you")),map_signif_level=TRUE, y_position = c(100), textsize = 5.5)+
  theme_classic() +
  labs(title = "Safety", x = "", y = "Safety in driving skills relative to AVs") +
  scale_x_discrete(labels=t_names) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

# Scatter plot
p4_1 <- p4_1 + geom_point(aes(x = factor(cond), y = safety), data = d_merged, color = "black", size = 1, position = 'identity', alpha = 0.5)

# Display the plot
p4_1

## ================================================================================================================

## Saving figures
dev.new(width=13,height=12,noRStudioGD = TRUE)

png(filename = "avp_e6_figure2.png", width = 1000,height = 750, res = 120)

figure <- ggarrange(p1_1, nrow=1,ncol=1,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean", color="black", face ="plain",size=18, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=18)) 

dev.off()

## ================================================================================================================

dev.new(width=13,height=12,noRStudioGD = TRUE)

png(filename = "avp_e6_figure3.png", width = 1000,height = 750, res = 120)

figure <- ggarrange(p2_1, p3_1, nrow=1,ncol=2,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean", color="black", face ="plain",size=18, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=18)) 
dev.off()
## ================================================================================================================

dev.new(width=13,height=12,noRStudioGD = TRUE)

pdf("avp_e6_fig1.pdf", width = 35, height = 20)

figure <- ggarrange(p1_1, p2_1, nrow=1,ncol=2,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean", color="black", face ="plain",size=55, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=55))  
dev.off()

## ================================================================================================================

## (4) ALL FIGURES
dev.new(width=13,height=12,noRStudioGD = TRUE)

png(filename = "avp_e6_figure1.png", width = 2000,height = 1500, res = 120)

figure <- ggarrange(p1, p2, p3, nrow=1,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean", color="black", face ="plain",size=18, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=18)) 

dev.off()


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
pref_lm2 <- clm(pref2 ~ cond + ai_knowledge_1, data = d_merged)  #model 2
summary(pref_lm2)

#Control for familiarity with AVs with safety-trust index
pref_lm3 <- clm(pref2 ~ cond + avgtrust + ai_knowledge_1, data = d_merged) #model 3
summary(pref_lm3) 

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================

