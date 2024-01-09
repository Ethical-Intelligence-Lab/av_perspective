## =======================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## =======================================================================================================================
##                                DATA ANALYSIS | AV PERSPECTIVE STUDY | EXPERIMENT 10               
## =======================================================================================================================
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
## =======================================================================================================================
##                                                  PRE-PROCESSING                 
## =======================================================================================================================

## read in data: 
# set working directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('avp_e2.csv') 

## explore data frame: 
head(d)
str(d)
dim(d) # dimensions of data frame by row [1] and column [2]
colnames(d) # all column names
summary(d)

## rename variables:
names(d)[names(d) == 'FL_8_DO'] <- 'cond'
names(d)[names(d) == 'comp_0'] <- 'comp_1'

## change condition entries
d$cond[d$cond == "FL_53"] <- "pref_other"
d$cond[d$cond == "FL_51"] <- "pref_you"

## subjects randomized:
table(d$cond)

##age to numeric
d$age = as.numeric(d$age)

## =======================================================================================================================
##                                                   EXCLUSIONS                
## =======================================================================================================================
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
d <- subset(d, (d$comp_1 == 1 & d$comp_2 == 2 & d$comp_3 == 4))
dim(d) # number of participants should decrease after comprehension exclusions

## number of participants AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final 
percent_excluded <- (n_original - n_final)/n_original 
percent_excluded
table(d$cond)

## =======================================================================================================================
##                                                    SUBSETTING                 
## =======================================================================================================================
colnames(d)
d <- d %>% relocate(pref_others, .after = pref_you)
d <- d %>% relocate(purchase_others_5, .after = purchase_you_5)
d <- d %>% relocate(safety_others_1, .after = safety_you_1)
d <- d %>% relocate(safety_AV_2_1, .after = safety_AV_1_1)
d <- d %>% relocate(trust_others_1, .after = trust_you_1)
d <- d %>% relocate(trust_AV_2_1, .after = trust_AV_1_1)
d <- d %>% relocate(identity_others_1, .after = identity_you_1)
d <- d %>% relocate(skills_others_1, .after = skills_you_1)
d <- d %>% relocate(control_others_1, .after = control_you_1)

## new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 10))
colnames(d_subset) <- c('cond','pref','purchase', 'safety_agent', 'safety_AV', 'trust_agent','trust_AV','identity','skills','control')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract data of interest from middle part of raw data:
for(i in 1:dim(d)[1]) {
  curr <- d[i,22:23][!is.na(d[i,22:23])] # for a given row, get only the non-NA values
  d_subset[i,2] <- as.numeric(curr[curr!= ""]) # and only the non-empty values
  curr1 <- d[i,25:26][!is.na(d[i,25:26])] # for a given row, get only the non-NA values
  d_subset[i,3] <- as.numeric(curr1[curr1!= ""])
  curr2 <- d[i,29:30][!is.na(d[i,29:30])] # for a given row, get only the non-NA values
  d_subset[i,4] <- as.numeric(curr2[curr2!= ""])
  curr3 <- d[i,31:32][!is.na(d[i,31:32])] # for a given row, get only the non-NA values
  d_subset[i,5] <- as.numeric(curr3[curr3!= ""])
  curr4 <- d[i,33:34][!is.na(d[i,33:34])] # for a given row, get only the non-NA values
  d_subset[i,6] <- as.numeric(curr4[curr4!= ""])
  curr5 <- d[i,35:36][!is.na(d[i,35:36])] # for a given row, get only the non-NA values
  d_subset[i,7] <- as.numeric(curr5[curr5!= ""])
  curr6 <- d[i,37:38][!is.na(d[i,37:38])] # for a given row, get only the non-NA values
  d_subset[i,8] <- as.numeric(curr6[curr6!= ""])
  curr7 <- d[i,39:40][!is.na(d[i,39:40])] # for a given row, get only the non-NA values
  d_subset[i,9] <- as.numeric(curr7[curr7!= ""])
  curr8 <- d[i,41:42][!is.na(d[i,41:42])] # for a given row, get only the non-NA values
  d_subset[i,10] <- as.numeric(curr8[curr8!= ""])
  d_subset[i,1] <- d[i,57][!is.na(d[i,57])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,44:56])
d_merged$ss <- 1:dim(d_merged)[1]

# average trust and safety measures
cronbach.alpha(d_merged[,c("safety_agent", "trust_agent")])
cronbach.alpha(d_merged[,c("safety_AV", "trust_AV")])
d_merged <- d_merged %>%
  mutate(avgtrust_agent = (safety_agent + trust_agent) / 2)
d_merged <- d_merged %>%
  mutate(avgtrust_av = (safety_AV + trust_AV) / 2)

# create variable for relative safety and trust
d_merged <- d_merged %>%
  mutate(safety = (safety_agent - safety_AV))
d_merged <- d_merged %>%
  mutate(trust = (trust_agent - trust_AV))

# average relative safety and trust
cronbach.alpha(d_merged[,c("safety", "trust")])
d_merged <- d_merged %>%
  mutate(avgtrust = (safety + trust) / 2)

# reorder groups
str(d_merged)
d_merged$cond <- as.factor(d_merged$cond)
d_merged$cond <- factor(d_merged$cond, levels = c('pref_you', 'pref_other'))
levels(d_merged$cond)

## =======================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## =======================================================================================================================

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

#### by condition
table(d_merged$cond)
table_gen <- table(d_merged$gender, d_merged$cond)
gen2_self <- table_gen[2,1]/sum(table_gen[,1])*100 #percent female in you condition
gen2_others <- table_gen[2,2]/sum(table_gen[,2])*100 #percent female in others condition
aggregate(age ~ cond, data = d_merged, mean)

## =======================================================================================================================
##                                              DATA EXPLORATION - DISTRIBUTIONS                
## =======================================================================================================================

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
leveneTest(pref ~ cond, d_merged)

## ======================================================================================================================
##                                              PLOTTING MAIN FIGURES                 
## ======================================================================================================================
t_names <- c("Self", "Others")
d_merged$pref1 <- d_merged$pref - 1 # only for the purpose of the plot, to match automation levels to SAE's official levels

## (1) PREFERENCE
p1 <- ggplot(d_merged,aes(x=factor(cond),y=pref1)) +  
  theme_bw() +coord_cartesian(ylim=c(1,7))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(6.5), textsize = 3.5)

p1

p1 <- p1 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Automation Preference") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               linewidth=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p1 

## ======================================================================================================================
p1_1 <- ggplot(d_merged, aes(x = factor(cond), y = pref1)) +
  coord_cartesian(ylim=c(0,5)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+ 
  geom_bar(stat = "summary", fun = "mean", fill = c("#56B4E9","#009E73"), color = 'black', alpha = 0.7, width = 0.3) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(4), textsize = 25)+
  theme_classic() +
  labs(title = "Automation Preference", x = "", y = "Automation Preference") +
  scale_x_discrete(labels=t_names) +
  theme(plot.title = element_text(size=65, hjust=0.5, face = "bold")) +
  theme(axis.text = element_text(size = 50), axis.title = element_text(size = 50))

# Display the plot
p1_1

## ======================================================================================================================

## (2) PURCHASE
p2 <- ggplot(d_merged,aes(x=factor(cond),y=purchase)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(105), textsize = 3.5)

p2

p2 <- p2 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Purchase Level 6") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               linewidth=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p2 

## ======================================================================================================================

p2_1 <- ggplot(d_merged, aes(x = factor(cond), y = purchase)) +
  coord_cartesian(ylim=c(0.26,110)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+ 
  geom_bar(stat = "summary", fun = "mean", fill = c("#56B4E9","#009E73"), color = 'black', alpha = 0.7, width = 0.3) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(80), textsize = 25)+
  theme_classic() +
  labs(title = "Purchase Preference", x = "", y = "Preference for Level 6 automation") +
  scale_x_discrete(labels=t_names) +
  theme(plot.title = element_text(size=65, hjust=0.5, face = "bold")) +
  theme(axis.text = element_text(size = 50), axis.title = element_text(size = 50))

# Display the plot
p2_1

## ================================================================================================================

## (3) AVERAGE TRUST
p3 <- ggplot(d_merged,aes(x=factor(cond),y=avgtrust)) +  
  theme_bw() +coord_cartesian(ylim=c(-102,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(105), textsize = 3.5)

p3

p3 <- p3 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Safety - Trust Index") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               linewidth=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p3 
## ======================================================================================================================

p3_1 <- ggplot(d_merged, aes(x = factor(cond), y = avgtrust)) +
  coord_cartesian(ylim=c(-100,110)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("#56B4E9","#009E73"), color = 'black', alpha = 0.7, width = 0.3) +
  geom_point(aes(x = factor(cond), y = avgtrust), data = d_merged, color = "grey40", size = 1, position = 'identity', alpha = 0.5) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(93), textsize = 25)+
  theme_classic() +
  labs(title = "Safety-Trust Index", x = "", y = "Safety-Trust in driving skills relative to AVs") +
  scale_x_discrete(labels=t_names) +
  theme(plot.title = element_text(size= 65, hjust=0.5, face = "bold")) +
  theme(axis.text = element_text(size = 50), axis.title = element_text(size = 50))

# Display the plot
p3_1

## ======================================================================================================================

## (4) AVERAGE TRUST AVs
p4 <- ggplot(d_merged,aes(x=factor(cond),y=avgtrust_av)) +  
  theme_bw() +coord_cartesian(ylim=c(0,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(105), textsize = 3.5)

p4

p4 <- p4 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Safety - Trust Index for AVs") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               linewidth=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p4 
## ======================================================================================================================
p4_1 <- ggplot(d_merged, aes(x = factor(cond), y = avgtrust_av)) +
  coord_cartesian(ylim=c(0,110)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("#56B4E9","#009E73"), color = 'black', alpha = 0.7, width = 0.3) +
  geom_point(aes(x = factor(cond), y = avgtrust_av), data = d_merged, color = "grey40", size = 1, position = 'identity', alpha = 0.5) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(100), textsize = 25)+
  theme_classic() +
  labs(title = "Safety-Trust in AVs", x = "", y = "Safety-Trust in driving skills for AVs") +
  scale_x_discrete(labels=t_names) +
  theme(plot.title = element_text(size=65, hjust=0.5, face = "bold")) +
  theme(axis.text = element_text(size = 50), axis.title = element_text(size = 50))

# Display the plot
p4_1
## ======================================================================================================================

## (5) AVERAGE TRUST AGENT
p5 <- ggplot(d_merged,aes(x=factor(cond),y=avgtrust_agent)) +  
  theme_bw() +coord_cartesian(ylim=c(0,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(105), textsize = 3.5)

p5

p5 <- p5 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Safety - Trust Index for Agent") +
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5, face = "bold")) +
  geom_violin(width=0.9, alpha=0.38, size=0.75) +
  geom_sina(alpha=0.6, size=0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black", 
               linewidth=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p5
## ===================================================================================================================================

p5_1 <- ggplot(d_merged, aes(x = factor(cond), y = avgtrust_agent)) +
  coord_cartesian(ylim=c(0,110)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("#56B4E9","#009E73"), color = 'black', alpha = 0.7, width = 0.3) +
  geom_point(aes(x = factor(cond), y = avgtrust_agent), data = d_merged, color = "grey40", size = 1, position = 'identity', alpha = 0.5) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  geom_signif(comparisons = list(c("pref_you", "pref_other")),map_signif_level=TRUE, y_position = c(100), textsize = 25)+
  theme_classic() +
  labs(title = "Safety-Trust in Agent", x = "", y = "Safety-Trust in driving skills for Agent") +
  scale_x_discrete(labels=t_names) +
  theme(plot.title = element_text(size=65, hjust=0.5, face = "bold")) +
  theme(axis.text = element_text(size = 50), axis.title = element_text(size = 50))

# Display the plot
p5_1

## =======================================================================================================================
##                                               DATA ANALYSIS- T TESTS             
## =======================================================================================================================

tapply(d_merged$pref, d_merged$cond, mean)
tapply(d_merged$purchase, d_merged$cond, mean)
tapply(d_merged$avgtrust, d_merged$cond, mean)
tapply(d_merged$avgtrust_av, d_merged$cond, mean)
tapply(d_merged$avgtrust_agent, d_merged$cond, mean)

## (2) T-TESTS
t.test(pref ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(purchase ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(avgtrust ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(avgtrust_av ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(avgtrust_agent ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(identity ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(skills ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(control ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(safety ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)
t.test(trust ~ cond, data = d_merged, paired = FALSE, var.equal = TRUE)

## effect size
d_merged %>% cohens_d(pref ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(purchase ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(avgtrust ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(avgtrust_av ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(avgtrust_agent ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(identity ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(skills ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(control ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(safety ~ cond, paired = FALSE, var.equal = TRUE)
d_merged %>% cohens_d(trust ~ cond, paired = FALSE, var.equal = TRUE)

## non-parametric t-test
wilcox.test(pref ~ cond, data = d_merged, paired = FALSE)

## linear models for pref
d_merged$pref2 <- factor(d_merged$pref, ordered = TRUE) #ORDINAL VARIABLE NEEDS TO BE FACTORED FOR CLM
pref_lm1 <- clm(pref2 ~ cond + avgtrust, data = d_merged)  #appendix model 1a
summary(pref_lm1)

pref_lm2 <- clm(pref2 ~ cond + avgtrust + identity + skills + control, data = d_merged, family = cumulative()) #appenidx model 2a
summary(pref_lm2)

## linear models for purchase
pur_lm1 <- glm(purchase ~ cond + avgtrust, data = d_merged) #appendix model 1b
summary(pur_lm1)

pur_lm2 <- glm(purchase ~ cond + avgtrust + identity + skills + control, data = d_merged) #appendix model 2b
summary(pur_lm2)

## ======================================================================================================================
##                                            DATA ANALYSIS - MEDIATION                
## ======================================================================================================================
source('process.r')

d_merged$cond = as.numeric(d_merged$cond)

## PARALLEL MEDIATION:
process(data = d_merged, y = "pref", x = "cond", 
        m =c("avgtrust", "identity","skills","control"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "purchase", x = "cond", 
        m =c("avgtrust", "identity","skills","control"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

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
t.test(purchase ~ cond, data = d_merged2, paired = FALSE, var.equal = TRUE)
t.test(avgtrust ~ cond, data = d_merged2, paired = FALSE, var.equal = TRUE)

d_merged2 %>% cohens_d(pref ~ cond, paired = FALSE, var.equal = FALSE)
d_merged2 %>% cohens_d(purchase ~ cond, paired = FALSE, var.equal = FALSE)
d_merged2 %>% cohens_d(avgtrust ~ cond, paired = FALSE, var.equal = FALSE)

process(data = d_merged2, y = "pref", x = "cond", 
        m =c("avgtrust", "identity","skills","control"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged2, y = "purchase", x = "cond", 
        m =c("avgtrust", "identity","skills","control"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## RESULTS REMAIN UNCHANGED IF TESTED FOR PEOPLE WHO HAVE A DRIVERS LICENSE

##subset population that do not have a drivers license
d_merged3 <-  d_merged[d_merged$license == "2", ]
dim(d_merged3)

##perform t-tests on subset
t.test(pref ~ cond, data = d_merged3, paired = FALSE, var.equal = TRUE)
t.test(avgtrust_av ~ cond, data = d_merged3, paired = FALSE, var.equal = TRUE)

#### MAIN EFFECT DISSAPEARS IF TESTED FOR PEOPLE WHO DO NOT HAVE A DRIVERS LICENSE

## Control for familiarity with AVs and driving xp and safety-trust
pref_lm3 <- clm(pref2 ~ cond + avgtrust + ai_knowledge_1 + dr_history_1, data = d_merged) #appendix model 3a
summary(pref_lm3)

## Control for familiarity with AVs and driving xp and safety-trust
pur_lm3 <- lm(purchase ~ cond + avgtrust + ai_knowledge_1 + dr_history_1, data = d_merged) # appendix model 3b
summary(pur_lm3)

## ===================================================================================================================================

## (6) ALL FIGURES
dev.new(width=13,height=12,noRStudioGD = TRUE)

png(filename = "avp_e12_figure1.png", width = 3000,height = 1500, res = 120)

figure <- ggarrange(p1, p2, p3, p4, p5, nrow=2,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean", color="black", face ="plain",size=18, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=18)) 

dev.off()
## ================================================================================================================
## (7) ALL FIGURES 2
dev.new(width=13,height=12,noRStudioGD = TRUE)

pdf("avp_e12_fig1.pdf", width = 35, height = 20)

figure <- ggarrange(p1_1, p2_1, nrow=1,ncol=2,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean", color="black", face ="plain",size=55, rot=90))

dev.off()
## ================================================================================================================
## (8) ALL FIGURES 3
dev.new(width=13,height=12,noRStudioGD = TRUE)

pdf("avp_e12_fig2.pdf", width = 35, height = 20)

figure <- ggarrange(p5_1, p4_1, p3_1, nrow=1,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean", color="black", face ="plain",size=55, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=55))  
dev.off()

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================