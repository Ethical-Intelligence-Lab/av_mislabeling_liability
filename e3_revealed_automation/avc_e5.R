## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV RESPONSIBILITY STUDY | EXPERIMENT 5               
## ================================================================================================================
## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
library(dplyr)
library(sjstats)
library(ggpubr)
library(grid)
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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('avc_e5.csv') 

## explore data frame: 
head(d)
str(d)
dim(d) # dimensions of data frame by row [1] and column [2]
colnames(d) # all column names
summary(d)

## rename variables:
names(d)[names(d) == 'FL_12_DO'] <- 'cond'
names(d)[names(d) == 'resp_human1_10'] <- 'auto_nt_human_r'
names(d)[names(d) == 'resp_software1_10'] <- 'auto_nt_firm_r'
names(d)[names(d) == 'resp_human2_10'] <- 'co_nt_human_r'
names(d)[names(d) == 'resp_software2_10'] <- 'co_nt_firm_r'
names(d)[names(d) == 'liable_human1_1'] <- 'auto_nt_human_l'
names(d)[names(d) == 'liable_firm1_1'] <- 'auto_nt_firm_l'
names(d)[names(d) == 'liab_human2_1'] <- 'co_nt_human_l'
names(d)[names(d) == 'liab_firm2_1'] <- 'co_nt_firm_l'
names(d)[names(d) == 'resp_human3_10'] <- 'auto_ft_human_r'
names(d)[names(d) == 'resp_software3_10'] <- 'auto_ft_firm_r'
names(d)[names(d) == 'resp_human4_10'] <- 'co_ft_human_r'
names(d)[names(d) == 'resp_software4_10'] <- 'co_ft_firm_r'
names(d)[names(d) == 'liab_human3_1'] <- 'auto_ft_human_l'
names(d)[names(d) == 'liab_soft3_1'] <- 'auto_ft_firm_l'
names(d)[names(d) == 'liab_human4_1'] <- 'co_ft_human_l'
names(d)[names(d) == 'liab_software4_1'] <- 'co_ft_firm_l'

## change condition entries
d$cond[d$cond == "FL_35"] <- "auto_nt"
d$cond[d$cond == "FL_36"] <- "co_nt"
d$cond[d$cond == "FL_50"] <- "auto_ft"
d$cond[d$cond == "FL_54"] <- "co_ft"

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
n_original <- dim(d)[1]

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp_1 == 2 & d$comp_2 == 4))
dim(d) # number of participants should decrease after comprehension exclusions
d <- subset(d, (d$comp_3 == 2 | d$comp_4 == 1 | d$comp_5 == 2 | d$comp_6 == 1 | d$comp_7 == 1 | d$comp_8 == 1))
dim(d)

## incomplete responses
d <- subset(d, (d$Finished == 1))
dim(d)

## number of participants AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final 
percent_excluded <- (n_original - n_final)/n_original 
percent_excluded
table(d$cond)

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

colnames(d)
d <- d %>% relocate(co_1, .after = auto_1)
d <- d %>% relocate(auto_2, .after = co_1)
d <- d %>% relocate(co_2, .after = auto_2)
d <- d %>% relocate(co_nt_firm_r, .after = auto_nt_firm_r)
d <- d %>% relocate(auto_ft_firm_r, .after = co_nt_firm_r)
d <- d %>% relocate(co_ft_firm_r, .after = auto_ft_firm_r)

d <- d %>% relocate(co_nt_human_r, .after = auto_nt_human_r)
d <- d %>% relocate(auto_ft_human_r, .after = co_nt_human_r)
d <- d %>% relocate(co_ft_human_r, .after = auto_ft_human_r)

d <- d %>% relocate(co_nt_firm_l, .after = auto_nt_firm_l)
d <- d %>% relocate(auto_ft_firm_l, .after = co_nt_firm_l)
d <- d %>% relocate(co_ft_firm_l, .after = auto_ft_firm_l)

d <- d %>% relocate(co_nt_human_l, .after = auto_nt_human_l)
d <- d %>% relocate(auto_ft_human_l, .after = co_nt_human_l)
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
  d_subset[i,1] <- d[i,69][!is.na(d[i,69])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,52:68])
d_merged$ss <- 1:dim(d_merged)[1]
colnames(d_merged)

## add columns for label and transparency condition entries
d_merged$label <- ""
d_merged$transparency <- ""
d_merged$label[d_merged$cond == 'auto_nt'] <- 'auto'
d_merged$label[d_merged$cond == 'auto_ft'] <- 'auto'
d_merged$label[d_merged$cond == 'co_nt'] <- 'co'
d_merged$label[d_merged$cond == 'co_ft'] <- 'co'
d_merged$transparency[d_merged$cond == 'auto_nt'] <- 'no'
d_merged$transparency[d_merged$cond == 'auto_ft'] <- 'yes'
d_merged$transparency[d_merged$cond == 'co_nt'] <- 'no'
d_merged$transparency[d_merged$cond == 'co_ft'] <- 'yes'

## ================================================================================================================
##                                            DATA ANALYSIS- MEASURES                 
## ================================================================================================================

# (1) Ethics/Morality Norms for Misinformation

cronbach.alpha(d_merged[,c("norm_1_1", "norm_1_2", "norm_1_3", "norm_1_4", "norm_1_5")], na.rm = TRUE)
d_merged$ethics <- rowMeans(d_merged[,c("norm_1_1", "norm_1_2", "norm_1_3", "norm_1_4", "norm_1_5")], na.rm = TRUE)

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
var.test(d_merged$firm_liability[d_merged$transparency == 'no'& d_merged$label == 'auto'],
         d_merged$firm_liability[d_merged$transparency == 'no'& d_merged$label == 'co'])

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

## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                
## ================================================================================================================
a_names <- c('Auto_FT, Auto_NT,Co_FT,Co_NT')
t_names <- c("Autopilot", "Copilot")
transparency_conds <- c('Absent', 'Present')

##(1) AUTOMATION LEVEL

p1 <- ggplot(d_merged,aes(x=cond,y=automation)) +  
  theme_bw() + coord_cartesian(ylim=c(1,7.5)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) + 
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_nt", "co_nt"), c("auto_ft", "co_nt"), c("auto_ft", "auto_nt"),c("auto_nt", "co_ft"), c("co_ft", "co_nt")),
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
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_nt", "co_nt"), c("auto_ft", "co_nt"), c("auto_ft", "auto_nt"),c("auto_nt", "co_ft"), c("co_ft", "co_nt")),
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

d_merged |>
  group_by(cond) |>
  summarise( mean_firm = mean(`firm_responsibility`))

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
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_nt", "co_nt"), c("auto_ft", "co_nt"), c("auto_ft", "auto_nt"),c("auto_nt", "co_ft"), c("co_ft", "co_nt")),
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
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_nt", "co_nt"), c("auto_ft", "co_nt"), c("auto_ft", "auto_nt"),c("auto_nt", "co_ft"), c("co_ft", "co_nt")),
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

ggarrange(p2_1, p4_1, common.legend = T)

##(5) Human Liability

p5 <- ggplot(d_merged,aes(x=factor(cond),y=human_liability)) +  
  theme_bw() +coord_cartesian(ylim=c(1,130))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto_ft", "co_ft"), c("auto_nt", "co_nt"), c("auto_ft", "co_nt"), c("auto_ft", "auto_nt"),c("auto_nt", "co_ft"), c("co_ft", "co_nt")),
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
source('process.R')
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

process(data = d_merged, y = "human_liability", x = "cond", 
        m =c("automation"), w = "ethics", model = 8, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

#=================================================================================
# PLOTS LABEL-DISCLOSURE
#=================================================================================
cronbach.alpha(d_subset[, c("firm_responsibility","firm_liability")])
cronbach.alpha(d_subset[, c("human_responsibility","human_liability")])

std.error <- function(x) sd(x)/sqrt(length(x))

d_subset |>
  gather(key = "DV", value = "Value",
         firm_responsibility, human_responsibility,
         firm_liability, human_liability) |>
  mutate(
    DV = case_when( DV == "human_liability" ~ "Human Driver Liability",
                    DV == "firm_liability" ~ "Firm Liability",
                    DV == "human_responsibility" ~ "Human Driver Responsibility",
                    DV == "firm_responsibility" ~ "AV Software Responsibility",),
    `Marketing Label` = ifelse(grepl("auto", cond), "Autopilot", "Copilot"),
    Disclosure = ifelse(grepl("ft", cond), "Disclosed", "Not Disclosed")
  ) |>
  group_by(`Marketing Label`, Disclosure, DV) |>
  summarize( 
    mean = mean(Value),
    se = std.error(Value) 
  ) -> d_plot

plot_did <- function(df=d_plot, dv, signif=c("*","*","*"), yaxis=TRUE, ypos=c(100, 100, 114)) {
  
  d_plot <- df |>
    filter(DV == dv)
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=Disclosure, y=mean, fill=`Marketing Label`)) +
    geom_bar(stat="identity", position="dodge", alpha=.75) +
    geom_errorbar(aes(ymin=mean-(se*se_width), ymax=mean+(se*se_width)), position = position_dodge(width=.9), 
                  size=.25, color="black", width=.5) +
    geom_point(aes(y=mean),position=position_dodge(width = .9), size=.75, color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold", size=10)) +
    geom_signif(
      y_position = c(50, 85, 125, 95, 75, 105, 115), xmin = c(0.8, 1.2, 1.0, 0.8, 1.8, 1.2, 0.8), 
      xmax = c(1.2, 1.8, 2.0, 1.8, 2.2, 2.2, 2.2), annotation = signif, tip_length = 0.07, 
      color='black', size = .25, textsize = 3
    ) +
    scale_fill_grey() +
    scale_color_grey() +
    ggtitle(dv) +
    xlab("Disclosure") +
    ylab("Response") +
    scale_y_continuous(limits = c(0,125), breaks = c(0,20,40,60,80,100)) -> p
  
  if(!yaxis) {
    p <- p +
      theme( axis.line.y = element_line(color = "white"),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())
  }
  
  return(p)
}

plot_did(dv = "AV Software Responsibility", signif = c("+", "***", "ns", "***", "**", "***", "***"), ypos = c(75,75,90), yaxis=F) -> p4
p4

cond1 <- "auto_ft"
cond2 <- "co_nt"
t.test(d_merged[d_merged == cond1,]$firm_responsibility,
       d_merged[d_merged == cond2,]$firm_responsibility)

plot_did(dv = "Firm Liability", signif = c("*", "***", "ns", "***", "ns", "***", "***"), ypos = c(75,75,90)) -> p3
p3

cond1 <- "co_nt"
cond2 <- "auto_ft"
t.test(d_merged[d_merged == cond1,]$firm_liability,
       d_merged[d_merged == cond2,]$firm_liability)

ggarrange(p3,p4, common.legend = T)

plot_did(dv = "Human Driver Liability", signif = c("ns", "ns", "ns")) -> p1
plot_did(dv = "Firm Liability", signif = c("**", "ns", "ns"), yaxis=F, ypos = c(75,75,90))  -> p2
plot_did(dv = "Human Driver Responsibility", signif = c("ns", "ns", "ns"))  -> p3


ggarrange(p1 + rremove("ylab") + rremove("xlab"),
          p2 + rremove("ylab") + rremove("xlab"), 
          p3 + rremove("ylab") + rremove("xlab"), 
          p4 + rremove("ylab") + rremove("xlab"),
          ncol = 2, nrow = 2, common.legend = TRUE) |>
  annotate_figure( left = textGrob("Mean Rating", rot = 90, vjust = 1, gp = gpar(cex = .8)),
                   bottom = textGrob("Disclosure of True Level of Automation", gp = gpar(cex = .8)))

rm(p1, p2, p3, p4)

