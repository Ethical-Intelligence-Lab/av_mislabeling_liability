## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV CULPABILITY STUDY | EXPERIMENT 1               
## ================================================================================================================
## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
library(dplyr)
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
               'effects'
)
## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# set working directory to current directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('./avc_e1.csv') 

## explore data frame: 
head(d)
str(d)
dim(d) # dimensions of data frame by row [1] and column [2]
colnames(d) # all column names
summary(d)

## rename variables:
names(d)[names(d) == 'FL_12_DO'] <- 'cond'

## change condition entries
d$cond[d$cond == "FL_35"] <- "auto"
d$cond[d$cond == "FL_36"] <- "co"
d$cond[d$cond == "FL_37"] <- "dless"

## subjects randomized:
table(d$cond)
## ================================================================================================================
##                                                   EXCLUSIONS                
## ================================================================================================================

## number of participants BEFORE exclusions: 
dim(d)[1] # extracting number of rows only, not columns

## attention exclusions: 
# remove responses from data frame that failed attention checks
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
dim(d) # number of participants should decrease after attention exclusions
n_original <- dim(d)[1] # extracting number of rows only, not columns
n_original

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp_1 == 2 & d$comp_2 == 4))
dim(d) # number of participants should decrease after comprehension exclusions

## number of participants AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final 
percent_excluded <- (n_original - n_final)/n_original 
percent_excluded
table(d$cond)


## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

##re-arrange data
d <- d %>% relocate(co_1, .after = auto_1)
d <- d %>% relocate(dr_1, .after = co_1)
d <- d %>% relocate(use_2_1_1, .after = use_1_1_1)
d <- d %>% relocate(use_3_1_1, .after = use_2_1_1)
d <- d %>% relocate(use_2_2_1, .after = use_1_2_1)
d <- d %>% relocate(use_3_2_1, .after = use_2_2_1)
d <- d %>% relocate(use_2_3_1, .after = use_1_3_1)
d <- d %>% relocate(use_3_3_1, .after = use_2_3_1)
d <- d %>% relocate(use_2_4_1, .after = use_1_4_1)
d <- d %>% relocate(use_3_4_1, .after = use_2_4_1)
d <- d %>% relocate(value_2_1_1, .after = value_1_1_1)
d <- d %>% relocate(value_3_1_1, .after = value_2_1_1)
d <- d %>% relocate(value_2_2_1, .after = value_1_2_1)
d <- d %>% relocate(value_3_2_1, .after = value_2_2_1)
d <- d %>% relocate(value_2_3_1, .after = value_1_3_1)
d <- d %>% relocate(value_3_3_1, .after = value_2_3_1)
d <- d %>% relocate(value_2_4, .after = value_1_4)
d <- d %>% relocate(value_3_4, .after = value_2_4)

colnames(d)

## new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 10))
colnames(d_subset) <- c('cond','auto','use1','use2','use3','use4','value1','value2','value3','value4')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract data of interest from middle part of raw data:
for(i in 1:dim(d)[1]) {
  curr <- d[i,24:26][!is.na(d[i,24:26])] # for a given row, get only the non-NA values
  d_subset[i,2] <- as.numeric(curr[curr!= ""]) # and only the non-empty values
  u1 <- d[i,27:29][!is.na(d[i,27:29])]
  d_subset[i,3] <- as.numeric(u1[u1!= ""])
  u2 <- d[i,30:32][!is.na(d[i,30:32])]
  d_subset[i,4] <- as.numeric(u2[u2!= ""])
  u3 <- d[i,33:35][!is.na(d[i,33:35])]
  d_subset[i,5] <- as.numeric(u3[u3!= ""])
  u4 <- d[i,36:38][!is.na(d[i,36:38])]
  d_subset[i,6] <- as.numeric(u4[u4!= ""])
  v1 <- d[i,39:41][!is.na(d[i,39:41])]
  d_subset[i,7] <- as.numeric(v1[v1!= ""])
  v2 <- d[i,42:44][!is.na(d[i,42:44])]
  d_subset[i,8] <- as.numeric(v2[v2!= ""])
  v3 <- d[i,45:47][!is.na(d[i,45:47])]
  d_subset[i,9] <- as.numeric(v3[v3!= ""])
  v4 <- d[i,48:50][!is.na(d[i,48:50])]
  d_subset[i,10] <- as.numeric(v4[v4!= ""])
  d_subset[i,1] <- d[i,61][!is.na(d[i,61])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,51:60])
d_merged$ss <- 1:dim(d_merged)[1]

colnames(d_merged)

## cleaning up extreme prices of $30, $50 and $480,000
d_merged$value4
d_merged <- d_merged[-98,] #50
d_merged <- d_merged[-102,] #30
d_merged <- d_merged[-60,] #480k


## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(d_merged$age, trim = 0, na.rm = TRUE) ## mean age 
hist(d_merged$age)

## gender
table(d_merged$gender)[1]/sum(table(d$gender)) ## percentage of males
table(d_merged$gender)[2]/sum(table(d$gender)) ## percentage of females

## av knowledge
mean(d_merged$ai_knowledge_1, trim = 0, na.rm = TRUE) ## mean av knowledge 
hist(d$ai_knowledge)

## license
table(d$license)[1]/sum(table(d$license)) ## percentage with driver's license
table(d$license)[2]/sum(table(d$license)) ## percentage without driver's license

## ================================================================================================================
##                                              DATA ANALYSIS - MEASURES                
## ================================================================================================================

## (1) PERCEIVED USE
#reverse score items: 4
d_merged$reverseCode_use4 <- 100 - d_merged$use4

cronbach.alpha(d_merged[,c("use1", "use2", "use3", "reverseCode_use4")], na.rm = TRUE)
d_merged$use <- rowMeans(d_merged[,c("use1", "use2", "use3", "reverseCode_use4")], na.rm = TRUE)

mean(d_merged$use, trim = 0, na.rm = TRUE)
var(d_merged$use, na.rm = TRUE)

## (2) PERCEIVED VALUE
#excluded: value 4 because it is a price scale

cronbach.alpha(d_merged[,c("value1","value2","value3")], na.rm = TRUE)
d_merged$value <- rowMeans(d_merged[,c("value1","value2","value3")], na.rm = TRUE)

mean(d_merged$value, trim = 0, na.rm = TRUE)
var(d_merged$value, na.rm = TRUE)

## ================================================================================================================
##                                              DATA EXPLORATION - DISTRIBUTIONS                
## ================================================================================================================

## PREFERENCE
summary(d_merged$auto)
tapply(d_merged$auto, d_merged$cond, mean)

tapply(d_merged$use, d_merged$cond, mean)
tapply(d_merged$value, d_merged$cond, mean)
tapply(d_merged$value4, d_merged$cond, mean)

## T-TESTS
pairwise_t_test(d_merged, auto ~ cond)
pairwise_t_test(d_merged, use ~ cond)
pairwise_t_test(d_merged, value ~ cond)
pairwise_t_test(d_merged, value4 ~ cond)

## T.test
d1 <- subset(d_merged, d_merged$cond != 'dless')
t.test(auto ~ cond, data = d1)
d2 <- subset(d_merged, d_merged$cond != 'co')
t.test(auto ~ cond, data = d2)
d3 <- subset(d_merged, d_merged$cond != 'auto')
t.test(auto ~ cond, data = d3)
## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                 
## ================================================================================================================

t_names <- c("Autopilot", "Copilot", "Driverless")

##(1) AUTOMATION LEVEL

p1 <- ggplot(d_merged,aes(x=factor(cond),y=auto)) +  
  theme_bw() +coord_cartesian(ylim=c(1,6.8))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(6.5, 6, 6), textsize = 3.5)

p1

p1 <- p1 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names) +
  ggtitle("Perceived Level of Automation") +
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
p1 

## (2) Perceived Use

p2 <- ggplot(d_merged,aes(x=factor(cond),y=use)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE,y_position = c(107, 100, 100), textsize = 3.5)

p2 <- p2 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Ease of Use") +
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

## (3) Use 1- Rate how easy it is to learn how to drive
p3 <- ggplot(d_merged,aes(x=factor(cond),y=use1)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(107, 100, 100), textsize = 3.5)

p3 <- p3 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Learn to Drive") +
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

## (4) Use 2- Rate how easy it is to control
p4 <- ggplot(d_merged,aes(x=factor(cond),y=use2)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(107, 100, 100), textsize = 3.5)

p4 <- p4 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Easy to Control") +
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

## (5) Use 3- Rate how easy it is to understand
p5 <- ggplot(d_merged,aes(x=factor(cond),y=use3)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(107, 100, 100), textsize = 3.5)

p5 <- p5 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Easy to Understand") +
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
p5

## (6) Use 4- Rate how easy it is to misunderstand/misinterpret
p6 <- ggplot(d_merged,aes(x=factor(cond),y=use4)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(107, 100, 100), textsize = 3.5)

p6 <- p6 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Easy to Misunderstand") +
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
p6

## (7) Perceived value
p7 <- ggplot(d_merged,aes(x=factor(cond),y=value)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(107, 100, 100), textsize = 3.5)

p7 <- p7 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Perceived Value") +
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
p7

## (8) Value 1- Rate the quality
p8 <- ggplot(d_merged,aes(x=factor(cond),y=value1)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(107, 100, 100), textsize = 3.5)

p8 <- p8 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Quality") +
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
p8

## (9) Value 2- Rate how much you would enjoy driving
p9 <- ggplot(d_merged,aes(x=factor(cond),y=value2)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(107, 100, 100), textsize = 3.5)

p9 <- p9 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Enjoyment") +
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
p9

## (10) Value 3- Rate how much it would improve your social standing/impression
p10 <- ggplot(d_merged,aes(x=factor(cond),y=value3)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(107, 100, 100), textsize = 3.5)

p10 <- p10 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Social Standing Improvement") +
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
p10

## (11) Value 4- Indicate what you think is a fair price
##d_merged$logvalue4 <- log(d_merged$value4)

p11 <- ggplot(d_merged,aes(x=factor(cond),y=value4)) +  
  theme_bw() + coord_cartesian(ylim=c(10000,115000))+scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + geom_signif(comparisons = list(c("auto", "dless"),c("auto", "co"),c("co","dless")),map_signif_level=TRUE, y_position = c(107000, 100000, 100000), textsize = 3.5)

p11 <- p11 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Fair Price") +
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
p11

## ================================================================================================================

dev.new(width=13,height=12,noRStudioGD = TRUE)

png(filename = "culpability_figure1.png", width = 2000,height = 1500, res = 120)

figure1 <- ggarrange(p1, p2, p3, p4, p5, p6, nrow=2,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure1,left = text_grob("Mean", color="black", face ="plain",size=18, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=18)) 
dev.off()

dev.new(width=13,height=12,noRStudioGD = TRUE)

png(filename = "culpability_figure2.png", width = 2000,height = 1500, res = 120)

figure2 <- ggarrange(p7, p8, p9, p10, p11, nrow=2,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure2,left = text_grob("Mean", color="black", face ="plain",size=18, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=18))

dev.off()
## ================================================================================================================

## export merged data frame
write.csv(d_merged,"avc_e1_merged.csv")

# BAR PLOTS
d_merged |>
  select(cond, auto, use1, use2, use3, use4, value1, value2, value3, value4) -> d_plot

colnames(d_plot) <- c("Condition", "Perceived Automation", "Learn", "Control", "Understand", "Misinterpret", "Quality", "Enjoyment", "Social Standing", "Price")

d_plot |>
  gather(key = "Measure", value = "Response" , 2:10) |>
  mutate( 
    Condition = case_when(
      Condition == "co" ~ "Copilot",
      Condition == "auto" ~ "Autopilot",
      Condition == "dless" ~ "Driverless",
    )
    ) |>
  group_by(Condition, Measure) |>
  summarize(
    Mean = mean(Response),
    SE = sd(Response)/sqrt(n())
  ) -> d_plot

dv <- "Perceived Automation"
y_pos <- c(6, 6, 6.5)
signif <- c("*","*","*")
titulo <- "Perceived Automation"
  

plot_bar <- function(df=d_plot, dv, y_pos, signif=c("*","*","*"), titulo) {
  
  d_plot <- d_plot |>
    filter(Measure == dv)
  
  ggplot(data = d_plot, aes(x=Condition, y=Mean)) +
    geom_bar(stat="identity", alpha=.75) +
    geom_point(size=.75, color="black") +
    geom_errorbar(aes(ymin=Mean-(SE*se_width), ymax=Mean+(SE*se_width)), position = "dodge", 
                  size=.25, color="black", width=.75) +
    geom_signif(
      y_position = y_pos, xmin = c("Autopilot", "Copilot", "Autopilot"), xmax = c("Copilot", "Driverless", "Driverless"),
      annotation = signif, tip_length = 0.1, color='black', size = .25, textsize = 2
    ) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    ggtitle(titulo) -> p
  
  return(p)
}


## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================
