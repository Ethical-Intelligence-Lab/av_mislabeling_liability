## ================================================================================================================
##                                 Harvard Business School, Ethical Iopelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV RESPONSIBILITY STUDY | EXPERIMEop 5               
## ================================================================================================================
## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
library(dplyr)
library(sjstats)
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
               'emmeans',         # cooprast analysis for regression models
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
               'anova_stats'
)

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# set working directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('avc_e6.csv') 

## explore data frame: 
head(d)
str(d)
dim(d) # dimensions of data frame by row [1] and column [2]
colnames(d) # all column names
summary(d)

## rename variables:
names(d)[names(d) == 'FL_12_DO'] <- 'cond'
names(d)[names(d) == 'resp_human1_10'] <- 'auto_op_human_r'
names(d)[names(d) == 'resp_software1_10'] <- 'auto_op_firm_r'
names(d)[names(d) == 'resp_human2_10'] <- 'co_op_human_r'
names(d)[names(d) == 'resp_software2_10'] <- 'co_op_firm_r'
names(d)[names(d) == 'liable_human1_1'] <- 'auto_op_human_l'
names(d)[names(d) == 'liable_firm1_1'] <- 'auto_op_firm_l'
names(d)[names(d) == 'liab_human2_1'] <- 'co_op_human_l'
names(d)[names(d) == 'liab_firm2_1'] <- 'co_op_firm_l'
names(d)[names(d) == 'resp_human3_10'] <- 'auto_ft_human_r'
names(d)[names(d) == 'resp_software3_10'] <- 'auto_ft_firm_r'
names(d)[names(d) == 'resp_human4_10'] <- 'co_ft_human_r'
names(d)[names(d) == 'resp_software4_10'] <- 'co_ft_firm_r'
names(d)[names(d) == 'liab_human3_1'] <- 'auto_ft_human_l'
names(d)[names(d) == 'liab_soft3_1'] <- 'auto_ft_firm_l'
names(d)[names(d) == 'liab_human4_1'] <- 'co_ft_human_l'
names(d)[names(d) == 'liab_software4_1'] <- 'co_ft_firm_l'

## change condition eopries
d$cond[d$cond == "FL_35"] <- "auto_op"
d$cond[d$cond == "FL_36"] <- "co_op"
d$cond[d$cond == "FL_50"] <- "auto_ft"
d$cond[d$cond == "FL_54"] <- "co_ft"

## subjects randomized:
table(d$cond)

## ================================================================================================================
##                                                   EXCLUSIONS                
## ================================================================================================================

## number of participaops BEFORE exclusions: 
dim(d)[1] # extracting number of rows only, not columns

## atteopion exclusions: 
# remove responses from data frame that failed atteopion checks
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
dim(d) # number of participaops should decrease after atteopion exclusions
n_original <- dim(d)[1]

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp_1 == 2 & d$comp_2 == 4))
dim(d) # number of participaops should decrease after comprehension exclusions
d <- subset(d, (d$comp_3 == 1))
d <- subset(d, (d$cond == 'auto_op' & d$comp_4 == 2 | d$cond == 'auto_ft' & d$comp_4 == 3 | d$cond == 'co_op' & d$comp_4 == 2 | d$cond == 'co_ft' & d$comp_4 == 3))
dim(d)

## number of participaops AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final 
perceop_excluded <- (n_original - n_final)/n_original 
perceop_excluded
table(d$cond)

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

colnames(d)
d <- d %>% relocate(co_1, .after = auto_1)
d <- d %>% relocate(auto_2, .after = co_1)
d <- d %>% relocate(co_2, .after = auto_2)
d <- d %>% relocate(co_op_firm_r, .after = auto_op_firm_r)
d <- d %>% relocate(auto_ft_firm_r, .after = co_op_firm_r)
d <- d %>% relocate(co_ft_firm_r, .after = auto_ft_firm_r)

d <- d %>% relocate(co_op_human_r, .after = auto_op_human_r)
d <- d %>% relocate(auto_ft_human_r, .after = co_op_human_r)
d <- d %>% relocate(co_ft_human_r, .after = auto_ft_human_r)

d <- d %>% relocate(co_op_firm_l, .after = auto_op_firm_l)
d <- d %>% relocate(auto_ft_firm_l, .after = co_op_firm_l)
d <- d %>% relocate(co_ft_firm_l, .after = auto_ft_firm_l)

d <- d %>% relocate(co_op_human_l, .after = auto_op_human_l)
d <- d %>% relocate(auto_ft_human_l, .after = co_op_human_l)
d <- d %>% relocate(co_ft_human_l, .after = auto_ft_human_l)

## new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 6))
colnames(d_subset) <- c('cond','automation','firm_responsibility','human_responsibility','firm_liability','human_liability')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract data of interest from middle part of raw data:
for(i in 1:dim(d)[1]) {
  pref1 <- d[i,24:27][!is.na(d[i,24:27])] # for a given row, get only the non-NA values
  d_subset[i,2] <- as.numeric(pref1[pref1!= ""]) # and only the non-empty values
  resp1 <- d[i,28:31][!is.na(d[i,28:31])]
  d_subset[i,3] <- as.numeric(resp1[resp1!= ""])
  resp2 <- d[i,32:35][!is.na(d[i,32:35])]
  d_subset[i,4] <- as.numeric(resp2[resp2!= ""])
  liab1 <- d[i,36:39][!is.na(d[i,36:39])]
  d_subset[i,5] <- as.numeric(liab1[liab1!= ""])
  liab2 <- d[i,40:43][!is.na(d[i,40:43])]
  d_subset[i,6] <- as.numeric(liab2[liab2!= ""])
  d_subset[i,1] <- d[i,60][!is.na(d[i,60])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,50:59])
d_merged$ss <- 1:dim(d_merged)[1]
colnames(d_merged)

## add columns for label and transparency condition entries
d_merged$label <- ""
d_merged$transparency <- ""
d_merged$label[d_merged$cond == 'auto_op'] <- 'auto'
d_merged$label[d_merged$cond == 'auto_ft'] <- 'auto'
d_merged$label[d_merged$cond == 'co_op'] <- 'co'
d_merged$label[d_merged$cond == 'co_ft'] <- 'co'
d_merged$transparency[d_merged$cond == 'auto_op'] <- 'no'
d_merged$transparency[d_merged$cond == 'auto_ft'] <- 'yes'
d_merged$transparency[d_merged$cond == 'co_op'] <- 'no'
d_merged$transparency[d_merged$cond == 'co_ft'] <- 'yes'

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(d_merged$age, trim = 0, na.rm = TRUE) ## mean age 
hist(d_merged$age, main = "Histogram of Age", xlab = "Age")

## gender
table(d_merged$gender)[1]/sum(table(d$gender)) ## percentage of males
table(d_merged$gender)[2]/sum(table(d$gender)) ## percentage of females
barplot(table(d_merged$gender), main = "BarPlot for Gender", names.arg = c("Male","Female","Undisclosed","Other"))

## av knowledge
mean(d_merged$ai_knowledge_1, trim = 0, na.rm = TRUE) ## mean av knowledge 
hist(d$ai_knowledge, xlab = 'AI Knowledge', main = 'Histogram of AI Knowledge')

## license
table(d$license)[1]/sum(table(d$license)) ## percentage with driver's license
table(d$license)[2]/sum(table(d$license)) ## percentage without driver's license
barplot(table(d_merged$license), main = "BarPlot for License", names.arg = c("Yes","No"))

## ================================================================================================================
##                                              DATA EXPLORATION - DISTRIBUTIONS                
## ================================================================================================================

d_merged$cond = as.factor(d_merged$cond) # turn into factor

## (1) Summary Stats

## AUTOMATION PERCEPTION
summary(d_merged$automation)
tapply(d_merged$automation, d_merged$cond, mean)

## Responsibility
## Software
summary(d_merged$firm_responsibility)
tapply(d_merged$firm_responsibility, d_merged$cond, mean)

## Human
summary(d_merged$human_responsibility)
tapply(d_merged$human_responsibility, d_merged$cond, mean)

## Liability
## Firm
summary(d_merged$firm_liability)
tapply(d_merged$firm_liability, d_merged$cond, mean)

## Human
summary(d_merged$human_liability)
tapply(d_merged$human_liability, d_merged$cond, mean)

## Summary table across all conditions
d_merged %>%
  group_by(cond) %>%
  summarise(across(automation:human_liability, mean, na.rm= TRUE))

## (2) T-TESTS
t.test(firm_liability ~ transparency, data = d_merged)
t.test(firm_liability ~ label, data = d_merged)
t.test(firm_responsibility ~ label, data = d_merged)
t.test(firm_responsibility ~ transparency, data = d_merged)
pairwise_t_test(d_merged, automation ~ cond)
pairwise_t_test(d_merged, firm_responsibility ~ cond)
pairwise_t_test(d_merged, human_responsibility ~ cond)
pairwise_t_test(d_merged, firm_liability ~ cond)
pairwise_t_test(d_merged, human_liability ~ cond)

### T-test for automation by label by transparency
t1 <- t.test(d_merged$automation[d_merged$transparency == 'yes'& d_merged$label == 'auto'],
             d_merged$automation[d_merged$transparency == 'yes'& d_merged$label == 'co'], paired = FALSE)
t1
t2 <- t.test(d_merged$automation[d_merged$transparency == 'no'& d_merged$label == 'auto'],
             d_merged$automation[d_merged$transparency == 'no'& d_merged$label == 'co'], paired = FALSE)
t2

### T-test for software responsibility by label by transparency
t3 <- t.test(d_merged$firm_responsibility[d_merged$transparency == 'yes'& d_merged$label == 'auto'],
             d_merged$firm_responsibility[d_merged$transparency == 'yes'& d_merged$label == 'co'], paired = FALSE)
t3
t4 <- t.test(d_merged$firm_responsibility[d_merged$transparency == 'no'& d_merged$label == 'auto'],
             d_merged$firm_responsibility[d_merged$transparency == 'no'& d_merged$label == 'co'], paired = FALSE)
t4

### T-test for human responsibility by label by transparency
t5 <- t.test(d_merged$human_responsibility[d_merged$transparency == 'yes'& d_merged$label == 'auto'],
             d_merged$human_responsibility[d_merged$transparency == 'yes'& d_merged$label == 'co'], paired = FALSE)
t5
t6 <- t.test(d_merged$human_responsibility[d_merged$transparency == 'no'& d_merged$label == 'auto'],
             d_merged$human_responsibility[d_merged$transparency == 'no'& d_merged$label == 'co'], paired = FALSE)
t6

### T-test for firm liability by label by transparency
t7 <- t.test(d_merged$firm_liability[d_merged$transparency == 'yes'& d_merged$label == 'auto'],
             d_merged$firm_liability[d_merged$transparency == 'yes'& d_merged$label == 'co'], paired = FALSE)
t7
t8 <- t.test(d_merged$firm_liability[d_merged$transparency == 'no'& d_merged$label == 'auto'],
             d_merged$firm_liability[d_merged$transparency == 'no'& d_merged$label == 'co'], paired = FALSE)
t8

### T-test for human liability by label by transparency
t9 <- t.test(d_merged$human_liability[d_merged$transparency == 'yes'& d_merged$label == 'auto'],
             d_merged$human_liability[d_merged$transparency == 'yes'& d_merged$label == 'co'], paired = FALSE)
t9
t10 <- t.test(d_merged$human_liability[d_merged$transparency == 'no'& d_merged$label == 'auto'],
              d_merged$human_liability[d_merged$transparency == 'no'& d_merged$label == 'co'], paired = FALSE)
t10

## (3) ANOVA

## software responsibility
firmresp_mod <- aov(firm_responsibility ~ as.factor(label) * as.factor(transparency), data = d_merged)
summary(firmresp_mod)
anova_stats(firmresp_mod)

## human responsibility
humanresp_mod <- aov(human_responsibility ~ as.factor(label) * as.factor(transparency), data = d_merged)
summary(humanresp_mod)
anova_stats(humanresp_mod)

## firm liability
firmliab_mod <- aov(firm_liability ~ as.factor(label) * as.factor(transparency), data = d_merged)
summary(firmliab_mod)
anova_stats(firmliab_mod)

## human liability
humaliab_mod <- aov(human_liability ~ as.factor(label) * as.factor(transparency), data = d_merged)
summary(humaliab_mod)
anova_stats(humaliab_mod)

## linear model
pref_lm1 <- lm(automation ~ cond + label + transparency, data = d_merged)  
summary(pref_lm1)

## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                
## ================================================================================================================
a_names <- c('Auto_FT', 'Auto_OP', 'Co_FT', 'Co_OP')
t_names <- c("Autopilot", "Copilot")
transparency_conds <- c('Absent', 'Present')

##(1) AUTOMATION LEVEL

p1 <- ggplot(d_merged,aes(x=cond,y=automation)) +  
  theme_bw() + coord_cartesian(ylim=c(1,7.5)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) + 
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_op", "co_op"), c("auto_ft", "co_op"), c("auto_ft", "auto_op"),c("auto_op", "co_ft"), c("co_ft", "co_op")),
              test = "wilcox.test", test.args = list(exact = FALSE, alternative = "two.sided"), 
              map_signif_level=TRUE,y_position = c(7.25,7,6.75,6.5,6.25,6), textsize = 3.5)

p1 <- p1 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=a_names) +
  ggtitle("Perceived Level of Automation") +
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

##-----------------------------------------------------------------------------------------------------------------
p1_1 <- ggplot(d_merged, aes(x = factor(label), y = automation, fill = factor(transparency)), color = factor(transparency_conds)) +
  theme_bw() +
  coord_cartesian(ylim = c(1, 7.5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p1_1

p1_1 <- p1_1 +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = t_names) +
  ggtitle("Perceived Level of Automation") +
  scale_fill_manual(values = c("#cccccc", "#333333"), name = "Transparency:",
                    labels = transparency_conds, guide = guide_legend(reverse = FALSE)) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  geom_violin(width = 0.9, alpha = 0.38, size = 0.75) +
  geom_sina(alpha = 0.6, size = 0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black",
               size = 0.4, fun.args = list(mult = 1),
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black",
               fun.args = list(mult = 1),
               position = position_dodge(width = 0.9),
               geom = "errorbar", width = 0.2)
p1_1

##(2) SOFTWARE RESPONSIBILITY 

p2 <- ggplot(d_merged,aes(x=factor(cond),y=firm_responsibility)) +  
  theme_bw() +coord_cartesian(ylim=c(1,130))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_op", "co_op"), c("auto_ft", "co_op"), c("auto_ft", "auto_op"),c("auto_op", "co_ft"), c("co_ft", "co_op")),
              test = "wilcox.test", test.args = list(exact = FALSE, alternative = "two.sided"), 
              map_signif_level=TRUE,y_position = c(125,120,115,110,105, 100), textsize = 3.5)

p2 <- p2 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=a_names)+
  ggtitle("Perceived Software Responsibility") +
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

##-----------------------------------------------------------------------------------------------------------------
p2_1 <- ggplot(d_merged, aes(x = factor(label), y = firm_responsibility, fill = factor(transparency)), color = factor(transparency_conds)) +
  theme_bw() +
  coord_cartesian(ylim = c(1, 105)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p2_1

p2_1 <- p2_1 +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = t_names) +
  ggtitle("Perceived Software Responsibility") +
  scale_fill_manual(values = c("#cccccc", "#333333"), name = "Transparency:",
                    labels = transparency_conds, guide = guide_legend(reverse = FALSE)) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  geom_violin(width = 0.9, alpha = 0.38, size = 0.75) +
  geom_sina(alpha = 0.6, size = 0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black",
               size = 0.4, fun.args = list(mult = 1),
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black",
               fun.args = list(mult = 1),
               position = position_dodge(width = 0.9),
               geom = "errorbar", width = 0.2)
p2_1

##(3) HUMAN RESPONSIBILITY 

p3 <- ggplot(d_merged,aes(x=factor(cond),y=human_responsibility)) +  
  theme_bw() +coord_cartesian(ylim=c(1,130))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_op", "co_op"), c("auto_ft", "co_op"), c("auto_ft", "auto_op"),c("auto_op", "co_ft"), c("co_ft", "co_op")),
              test = "wilcox.test", test.args = list(exact = FALSE, alternative = "two.sided"), 
              map_signif_level=TRUE,y_position = c(125,120,115,110,105, 100), textsize = 3.5)

p3 <- p3 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=a_names)+
  ggtitle("Perceived Human Responsibility") +
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

##-----------------------------------------------------------------------------------------------------------------
p3_1 <- ggplot(d_merged, aes(x = factor(label), y = human_responsibility, fill = factor(transparency)), color = factor(transparency_conds)) +
  theme_bw() +
  coord_cartesian(ylim = c(1, 105)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p3_1

p3_1 <- p3_1 +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = t_names) +
  ggtitle("Perceived Human Responsibility") +
  scale_fill_manual(values = c("#cccccc", "#333333"), name = "Transparency:",
                    labels = transparency_conds, guide = guide_legend(reverse = FALSE)) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  geom_violin(width = 0.9, alpha = 0.38, size = 0.75) +
  geom_sina(alpha = 0.6, size = 0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black",
               size = 0.4, fun.args = list(mult = 1),
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black",
               fun.args = list(mult = 1),
               position = position_dodge(width = 0.9),
               geom = "errorbar", width = 0.2)
p3_1

##(4) Firm Liability

p4 <- ggplot(d_merged,aes(x=factor(cond),y=firm_liability)) +  
  theme_bw() +coord_cartesian(ylim=c(1,130))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_op", "co_op"), c("auto_ft", "co_op"), c("auto_ft", "auto_op"),c("auto_op", "co_ft"), c("co_ft", "co_op")),
              test = "wilcox.test", test.args = list(exact = FALSE, alternative = "two.sided"), 
              map_signif_level=TRUE,y_position = c(125,120,115,110,105, 100), textsize = 3.5)

p4 <- p4 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=a_names)+
  ggtitle("Perceived Firm Liability") +
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

##-----------------------------------------------------------------------------------------------------------------
p4_1 <- ggplot(d_merged, aes(x = factor(label), y = firm_liability, fill = factor(transparency)), color = factor(transparency_conds)) +
  theme_bw() +
  coord_cartesian(ylim = c(1, 105)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p4_1

p4_1 <- p4_1 +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = t_names) +
  ggtitle("Perceived Firm Liability") +
  scale_fill_manual(values = c("#cccccc", "#333333"), name = "Transparency:",
                    labels = transparency_conds, guide = guide_legend(reverse = FALSE)) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  geom_violin(width = 0.9, alpha = 0.38, size = 0.75) +
  geom_sina(alpha = 0.6, size = 0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black",
               size = 0.4, fun.args = list(mult = 1),
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black",
               fun.args = list(mult = 1),
               position = position_dodge(width = 0.9),
               geom = "errorbar", width = 0.2)
p4_1

##(5) Human Liability

p5 <- ggplot(d_merged,aes(x=factor(cond),y=human_liability)) +  
  theme_bw() +coord_cartesian(ylim=c(1,130))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_op", "co_op"), c("auto_ft", "co_op"), c("auto_ft", "auto_op"),c("auto_op", "co_ft"), c("co_ft", "co_op")),
              test = "wilcox.test", test.args = list(exact = FALSE, alternative = "two.sided"), 
              map_signif_level=TRUE,y_position = c(125,120,115,110,105, 100), textsize = 3.5)

p5 <- p5 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=a_names)+
  ggtitle("Perceived Human Liability") +
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

##-----------------------------------------------------------------------------------------------------------------
p5_1 <- ggplot(d_merged, aes(x = factor(label), y = human_liability, fill = factor(transparency)), color = factor(transparency_conds)) +
  theme_bw() +
  coord_cartesian(ylim = c(1, 105)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p5_1

p5_1 <- p5_1 +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = t_names) +
  ggtitle("Perceived Human Liability") +
  scale_fill_manual(values = c("#cccccc", "#333333"), name = "Transparency:",
                    labels = transparency_conds, guide = guide_legend(reverse = FALSE)) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  geom_violin(width = 0.9, alpha = 0.38, size = 0.75) +
  geom_sina(alpha = 0.6, size = 0.95, color = "#999999") +
  stat_summary(fun.data = "mean_se", color = "black",
               size = 0.4, fun.args = list(mult = 1),
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black",
               fun.args = list(mult = 1),
               position = position_dodge(width = 0.9),
               geom = "errorbar", width = 0.2)
p5_1

## ================================================================================================================

dev.new(width=13,height=12,noRStudioGD = TRUE)

png(filename = "culpability_figure5.png", width = 2000,height = 1500, res = 120)

figure1 <- ggarrange(p1, p2, p3, p4, p5, nrow=2,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure1,left = text_grob("Mean", color="black", face ="plain",size=18, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=18)) 
dev.off()

dev.new(width=13,height=12,noRStudioGD = TRUE)

png(filename = "culpability_figure6.png", width = 2000,height = 1500, res = 120)

figure1 <- ggarrange(p1_1, p2_1, p3_1, p4_1, p5_1, nrow=2,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure1,left = text_grob("Mean", color="black", face ="plain",size=18, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=18)) 
dev.off()

## ================================================================================================================
##                                              DATA ANALYSIS - MEDIATION                
## ================================================================================================================
source('../e2_liability/process.R')
d_merged$cond = as.factor(d_merged$cond)
d_merged$cond = as.numeric(d_merged$cond)

##(1) SOFTWARE RESPONSBIBILITY

process(data = d_merged, y = "firm_responsibility", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

##(1) HUMAN RESPONSBIBILITY

process(data = d_merged, y = "human_responsibility", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

##(1) FIRM LIABILITY

process(data = d_merged, y = "firm_liability", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

##(1) HUMAN LIABILITY

process(data = d_merged, y = "human_liability", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## ================================================================================================================
##                                        DATA ANALYSIS - MODERATED MEDIATION                
## ================================================================================================================
str(d_merged)
d_merged$cond <- as.numeric(d_merged$cond)
d_merged$transparency <- as.factor(d_merged$transparency)
d_merged$transparency <- as.numeric(d_merged$transparency)
process(data = d_merged, y = "firm_liability", x = "cond", 
        m =c("automation"), w = "transparency", model = 8, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## ================================================================================================================

## export merged data frame
write.csv(d_merged,"avc_e6_merged.csv")

#=================================================================================
# On the interaction between the perceived capabilities and the transparency
# on the effects of responses to Human/Software Liability/Responsibility
#=================================================================================

library(tidyverse)
library(stargazer)

df <- read_csv("avc_e6.csv")

# Perform Exclusions
df |>
  filter(
    att_1 == 2,
    att_2 == 2,
    comp_1 == 2,
    comp_2 == 4,
    comp_3 == 1
  ) -> df

# Rename column and labels
df$cond <- df$FL_12_DO
df$cond[df$cond == "FL_35"] <- "auto_op"
df$cond[df$cond == "FL_36"] <- "co_op"
df$cond[df$cond == "FL_50"] <- "auto_ft"
df$cond[df$cond == "FL_54"] <- "co_ft"

# Exclude more (based on Stuti's code)
df |>
  filter((cond == 'auto_op' & comp_4 == 2 |
            cond == 'auto_ft' & comp_4 == 3 |
            cond == 'co_op' & comp_4 == 2   | 
            cond == 'co_ft' & comp_4 == 3     )) -> df

# Identify transparency and labels
df |>
  mutate(
    transparency = ifelse(grepl("op", cond), "no", "yes"),
    label = ifelse(grepl("auto", cond), "auto", "co")
  ) -> df

# Get data for different groups
# Elongate dataset for visualization and regression purposes

std_colnames <- c("id", "cond", "transparency", "label", "resp_soft", "resp_human", 
                  "liable_soft", "liable_human", "capability")

auto_op <- df |>
  filter(cond == "auto_op") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}1_[0-9]{1,}"), auto_1)
colnames(auto_op) <- std_colnames

auto_ft <- df |>
  filter(cond == "auto_ft") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}3_[0-9]{1,}"), auto_2)
colnames(auto_ft) <- std_colnames

co_op <- df |>
  filter(cond == "co_op") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}2_[0-9]{1,}"), co_1)
colnames(co_op) <- std_colnames

co_ft <- df |>
  filter(cond == "co_ft") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}4_[0-9]{1,}"), co_2)
colnames(co_ft) <- std_colnames

df_comb <- rbind(auto_ft, co_ft, co_op, auto_op)

#=================================================================================
# Individual plots for different dependant vars
#=================================================================================

# Defined capable as a binary variable with the threshold at 4
df_comb |>
  mutate(capable = ifelse(capability < 4, "no", "yes")) -> df_comb

rm(auto_ft, co_ft, co_op, auto_op)

# Plot capability, Human Responsible, transparency and label
hr <- ggplot(df_comb, aes(x=transparency, y=resp_human, color=capable, fill=capable))+
  geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
  ylab("Human Responsible") +
  theme_light()

# Plot capability, Software Responsible, transparency and label
sr <- ggplot(df_comb, aes(x=transparency, y=resp_soft, color=capable, fill=capable))+
  geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
  ylab("Software Responsible") +
  theme_light()

# Plot capability, Human Liable, transparency and label
hl <- ggplot(df_comb, aes(x=transparency, y=liable_human, color=capable, fill=capable))+
  geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
  ylab("Human Liable") +
  theme_light()

# Plot capability, Software Liable, transparency and label
sl <- ggplot(df_comb, aes(x=transparency, y=liable_soft, color=capable, fill=capable))+
  geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
  ylab("Software Liable") +
  theme_light()

#=================================================================================
# Gather and facet wrap plots
#=================================================================================

df_comb |>
  gather(key = "DV", value = "Rating", resp_soft, resp_human, liable_soft, liable_human) |>
  mutate(
    DV = case_when( DV == "liable_human" ~ "Human is Liable",
                    DV == "liable_soft" ~ "Software is Liable",
                    DV == "resp_human" ~ "Human is Responsible",
                    DV == "resp_soft" ~ " Software is Responsible",)
  ) |>
  group_by(capable, DV) |>
  mutate(
    mean = mean(Rating),
    std = sd(Rating)
  ) -> df2

sd_width <- 1

plot <- ggplot(df2, aes(x=transparency, y=Rating, color=capable, fill=capable))+
  geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
  geom_errorbar(aes(ymin=mean-(std*sd_width), ymax=mean+(std*sd_width)), position = "dodge", size=.25) +
  geom_point(aes(y=mean),position=position_dodge(width = .9), size=.75) +
  facet_wrap(vars(DV), nrow = 2) + 
  theme_light()

plot

#===============================================================================
# Regressions
#================================================================================
# Capability as a continuous variable
reg1 <- lm(resp_soft ~ transparency*capability, df_comb)
summary(reg1)

reg2 <- lm(resp_human ~ transparency*capability, df_comb)
summary(reg2)

reg3 <- lm(liable_soft ~ transparency*capability, df_comb)
summary(reg3)

reg4 <- lm(liable_human ~ transparency*capability, df_comb)
summary(reg4)

stargazer(reg1, reg2, reg3, reg4)

# Only transparency dummy
reg1a <- lm(resp_soft ~ transparency, df_comb)
summary(reg1a)

reg2a <- lm(resp_human ~ transparency, df_comb)
summary(reg2a)

reg3a <- lm(liable_soft ~ transparency, df_comb)
summary(reg3a)

reg4a <- lm(liable_human ~ transparency, df_comb)
summary(reg4a)

stargazer(reg1a, reg2a, reg3a, reg4a)

# capability dummy interaction
reg1b <- lm(resp_soft ~ transparency*as.factor(capability), df_comb)
summary(reg1b)

reg2b <- lm(resp_human ~ transparency*as.factor(capability), df_comb)
summary(reg2b)

reg3b <- lm(liable_soft ~ transparency*as.factor(capability), df_comb)
summary(reg3b)

reg4b <- lm(liable_human ~ transparency*as.factor(capability), df_comb)
summary(reg4b)

stargazer(reg1b, reg2b, reg3b, reg4b)


#===============================================================================
# ANOVA
#================================================================================
anova_rh <- aov(resp_human ~ capability * as.factor(transparency), data = df_comb)
summary(anova_rh)
anova_stats(anova_rh)

anova_lh <- aov(liable_human ~ capability * as.factor(transparency), data = df_comb)
summary(anova_lh)
anova_stats(anova_lh)

anova_rs <- aov(resp_soft ~ capability * as.factor(transparency), data = df_comb)
summary(anova_rs)
anova_stats(anova_rs)

anova_ls <- aov(liable_soft ~ capability * as.factor(transparency), data = df_comb)
summary(anova_ls)
anova_stats(anova_ls)

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================

