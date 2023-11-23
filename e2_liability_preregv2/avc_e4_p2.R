## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV responsibility STUDY | EXPERIMENT 4               
## ================================================================================================================
## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
library(dplyr)
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
d <- read.csv('avc_e4_p2.csv') 
source("process.r")

## explore data frame: 
head(d)
str(d)
dim(d) # dimensions of data frame by row [1] and column [2]
colnames(d) # all column names
summary(d)

## rename variables:
names(d)[names(d) == 'FL_12_DO'] <- 'cond'
names(d)[names(d) == 'resp_human1_10'] <- 'auto_resp_human'
names(d)[names(d) == 'resp_software1_10'] <- 'auto_resp_software'
names(d)[names(d) == 'resp_human2_10'] <- 'co_resp_human'
names(d)[names(d) == 'resp_software2_10'] <- 'co_resp_software'
names(d)[names(d) == 'liable_human1_1'] <- 'auto_liab_human'
names(d)[names(d) == 'liable_firm1_1'] <- 'auto_liab_firm'
names(d)[names(d) == 'liab_human2_1'] <- 'co_liab_human'
names(d)[names(d) == 'liab_firm2_1'] <- 'co_liab_firm'

## change condition entries
d$cond[d$cond == "FL_35"] <- "auto"
d$cond[d$cond == "FL_36"] <- "co"

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
d <- subset(d, (d$comp_3 == 2 | d$comp_4 == 1))
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
d <- d %>% relocate(co_resp_software, .after = auto_resp_software)
d <- d %>% relocate(co_resp_human, .after = auto_resp_human)
d <- d %>% relocate(co_liab_firm, .after = auto_liab_firm)
d <- d %>% relocate(co_liab_human, .after = auto_liab_human)

## new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 6))
colnames(d_subset) <- c('cond','automation','software responsibility','human responsibility','firm liability','human liability')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract data of interest from middle part of raw data:
for(i in 1:dim(d)[1]) {
  pref1 <- d[i,23:24][!is.na(d[i,23:24])] # for a given row, get only the non-NA values
  d_subset[i,2] <- as.numeric(pref1[pref1!= ""]) # and only the non-empty values
  resp1 <- d[i,25:26][!is.na(d[i,25:26])]
  d_subset[i,3] <- as.numeric(resp1[resp1!= ""])
  resp2 <- d[i,27:28][!is.na(d[i,27:28])]
  d_subset[i,4] <- as.numeric(resp2[resp2!= ""])
  liab1 <- d[i,29:30][!is.na(d[i,29:30])]
  d_subset[i,5] <- as.numeric(liab1[liab1!= ""])
  liab2 <- d[i,31:32][!is.na(d[i,31:32])]
  d_subset[i,6] <- as.numeric(liab2[liab2!= ""])
  d_subset[i,1] <- d[i,49][!is.na(d[i,49])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,38:46])
d_merged$ss <- 1:dim(d_merged)[1]
colnames(d_merged)

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(d_merged$age, trim = 0, na.rm = TRUE) ## mean age 
hist(d_merged$age, main = "Histogram of Age", xlab = "Age")

## gender
gender <- ifelse(d$gender == 1, "Male", "Female")
g_table <- table(gender)
g_table

prop_gtable <- prop.table(g_table)
prop_gtable

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

t = as.factor(d_merged$cond) # turn into factor

## AUTOMATION PERCEPTION
summary(d_merged$automation)
tapply(d_merged$automation, d_merged$cond, mean)

## responsibility
## Software
summary(d_merged$`software responsibility`)
tapply(d_merged$`software responsibility`, d_merged$cond, mean)

## Human
summary(d_merged$`human responsibility`)
tapply(d_merged$`human responsibility`, d_merged$cond, mean)

## LIABILITY
## Firm
summary(d_merged$`firm liability`)
tapply(d_merged$`firm liability`, d_merged$cond, mean)

## Firm
summary(d_merged$`human liability`)
tapply(d_merged$`human liability`, d_merged$cond, mean)

## T-TESTS

t.test(automation ~ cond, data = d_merged) ## Perceived Automation
t.test(`software responsibility` ~ cond, data = d_merged) ## Perceived Software Responsibility
t.test(`human responsibility` ~ cond, data = d_merged)
t.test(`firm liability` ~ cond, data = d_merged)
t.test(`human liability` ~ cond, data = d_merged)
 

## ================================================================================================================
##                                              PLOTTING MAIN FIGURES                
## ================================================================================================================
t_names <- c("Autopilot", "Copilot")

##(1) AUTOMATION LEVEL

p1 <- ggplot(d_merged,aes(x=factor(cond),y=automation)) +  
  theme_bw() + coord_cartesian(ylim=c(1,6.8)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) + 
  geom_signif(comparisons = list(c("auto", "co")), map_signif_level=TRUE, y_position = c(6.5, 6, 6), textsize = 3.5)

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
               linewidth=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p1 

##(2) SOFTWARE RESPONSIBILITY 
 
p2 <- ggplot(d_merged,aes(x=factor(cond),y=`software responsibility`)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "co")),map_signif_level=TRUE,y_position = c(103, 100), textsize = 3.5)

p2 <- p2 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Perceived Software responsibility") +
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

##(3) HUMAN RESPONSIBILITY 

p3 <- ggplot(d_merged,aes(x=factor(cond),y=`human responsibility`)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "co")),map_signif_level=TRUE,y_position = c(103, 100), textsize = 3.5)

p3 <- p3 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
  ggtitle("Perceived Human responsibility") +
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

##(4) Firm Liability

p4 <- ggplot(d_merged,aes(x=factor(cond),y=`firm liability`)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "co")),map_signif_level=TRUE,y_position = c(103, 100), textsize = 3.5)

p4 <- p4 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
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

##(5) Human Liability

p5 <- ggplot(d_merged,aes(x=factor(cond),y=`human liability`)) +  
  theme_bw() +coord_cartesian(ylim=c(1,110))+scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+ 
  geom_signif(comparisons = list(c("auto", "co")),map_signif_level=TRUE,y_position = c(103, 100), textsize = 3.5)

p5 <- p5 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_x_discrete(labels=t_names)+
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

## ================================================================================================================

dev.new(width=13,height=12,noRStudioGD = TRUE)

png(filename = "culpability_figure4.png", width = 2000,height = 1500, res = 120)

figure1 <- ggarrange(p1, p2, p3, p4, p5, nrow=2,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure1,left = text_grob("Mean", color="black", face ="plain",size=18, rot=90),
                bottom = text_grob("Condition", color="black", face ="plain",size=18)) 
dev.off()

## ================================================================================================================
##                                            DATA ANALYSIS - MEDIATION                
## ================================================================================================================
d_merged$cond <- as.numeric(t)

d_merged |>
  mutate(
    firm = (`software responsibility` + `firm liability`) / 2,
    human = (`human responsibility` + `human liability`) / 2
  ) -> d_merged

## PARALLEL MEDIATION:
process(data = d_merged, y = "software responsibility", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "human responsibility", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "firm liability", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "human liability", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "human", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "firm", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## ================================================================================================================
## ================================================================================================================

## ================================================================================================================
##                                            VIZ - BARPLOTS                
## ================================================================================================================
cronbach.alpha(d_merged[,c("software responsibility", "firm liability")])
cronbach.alpha(d_merged[,c("human responsibility", "human liability")])

d_merged |>
  select(cond, automation, `software responsibility`, `firm liability`, `human responsibility`, `human liability`) |>
  mutate(
    `Marketing Label` = ifelse(cond == "auto", "Autopilot", "Copilot"),
    `Firm` = (`software responsibility` + `firm liability`) / 2,
    Human = (`human responsibility` + `human liability`) / 2
  ) |>
  select(`Marketing Label`, Firm, Human) -> d_plot

t.test(Firm ~ `Marketing Label`, d_plot)
t.test(Human ~ `Marketing Label`, d_plot)

summary(lm(Firm ~ `Marketing Label`, d_plot))
summary(lm(Human ~ `Marketing Label`, d_plot))

d_plot |>
  group_by(`Marketing Label`) |>
  summarize(
    avg_F = mean(Firm),
    avg_H = mean(Human),
    se_F = sd(Firm)/sqrt(n()),
    se_H = sd(Human)/sqrt(n())
  ) -> d_plot

se_width <- 1.96

ggplot(data = d_plot, aes(x=factor(`Marketing Label`, level = c("Autopilot", "Copilot")), y=avg_F)) +
  geom_bar(stat="identity", alpha=.75, width=.75) +
  geom_point(size=.5, color="black") +
  geom_errorbar(aes(ymin=avg_F-(se_F*se_width), ymax=avg_F+(se_F*se_width)), position = "dodge", 
                size=1, color="black", width=.5) +
  geom_signif(
    y_position = c(90), xmin = c("Autopilot"), xmax = c( "Copilot"),
    annotation = c("***"), tip_length = 0.1, color='black', size = 1, textsize = 10
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=30,face="bold"),) +
  ylab("Firm Liability") +
  xlab("Marketing Labels") +
  ylim(0,100)  -> p1

p1 +
  theme(text = element_text(face = "bold", size=30))

ggplot(data = d_plot, aes(x=factor(`Marketing Label`, level = c("Autopilot", "Copilot")), y=avg_H)) +
  geom_bar(stat="identity", alpha=.75) +
  geom_point(size=.75, color="black") +
  geom_errorbar(aes(ymin=avg_H-(se_H*se_width), ymax=avg_H+(se_H*se_width)), position = "dodge", 
                size=.25, color="black", width=.75) +
  geom_signif(
    y_position = c(80), xmin = c("Autopilot"), xmax = c( "Copilot"),
    annotation = c("***"), tip_length = 0.1, color='black', size = .5, textsize = 3.5
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12),
        axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ylab("") +
  xlab("") +
  ylim(0,100) +
  ggtitle("Human Liability") -> p2

p2

ggarrange(p1,p2) |>
  annotate_figure(bottom = textGrob("Marketing Label", gp = gpar(cex = 1, fontsize=10, fontface="bold")))

## export merged data frame
write.csv(d_merged,"avc_e4_merged.csv")

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================
