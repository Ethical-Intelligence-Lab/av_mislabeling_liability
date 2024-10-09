## clear workspace
rm(list = ls()) 

## install packages
library(dplyr)
library(grid)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('tidyverse',       
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',           
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
               'Hmisc',            # get p values for mixed effect model
               'DescTools',        # get Cramer's V
               'rstatix',
               'effects'
)

# PROCESS Analysis (Set TRUE if you wish to run PROCESS code)
mediation <- T
if(mediation) {
  source("../process.R")
}

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data
d <- read_csv('data.csv')
d <- d[-c(1,2),]

d |>
  mutate_if(all.is.numeric, as.numeric) -> d

## Attention Check
d |>
  filter(att_1 == 2 & att_2 == 5) -> d

## Comp Check
d |>
  filter(comp_1 == 1 & comp_2 == 1) -> d

nrow(d)

## ================================================================================================================
##                                                 DEMOGRAPHICS                 
## ================================================================================================================

prop.table(table(d[d$gender == 1 | d$gender == 2,]$gender))[[1]]
mean(d$age)

table(d$label)

## ================================================================================================================
##                                                 ANALYSIS                
## ================================================================================================================

t.test(d[d$label == "Autopilot",]$automation_1,
       d[d$label == "Copilot",]$automation_1)

sd(d[d$label == "Autopilot",]$automation_1)
sd(d[d$label == "Copilot",]$automation_1)

cohen.d(d[d$label == "Autopilot",]$automation_1,
        d[d$label == "Copilot",]$automation_1)

cronbach.alpha(d[,c("software_liable_1", "software_resp_1")])
cronbach.alpha(d[,c("human_liable_1", "human_resp_1" )])

d$human <- (d$human_liable_1 + d$human_resp_1) / 2
d$firm <-(d$software_liable_1 + d$software_resp_1) / 2

### Firm Liability

t.test(d[d$label == "Autopilot",]$firm,
       d[d$label == "Copilot",]$firm)

sd(d[d$label == "Autopilot",]$firm)
sd(d[d$label == "Copilot",]$firm)

cohen.d(d[d$label == "Autopilot",]$firm,
        d[d$label == "Copilot",]$firm)

### Human Liability 

t.test(d[d$label == "Autopilot",]$human,
       d[d$label == "Copilot",]$human)

sd(d[d$label == "Autopilot",]$human)
sd(d[d$label == "Copilot",]$human)

cohen.d(d[d$label == "Autopilot",]$human,
        d[d$label == "Copilot",]$human)

## ================================================================================================================
##                                              PROCESS             
## ================================================================================================================


d$cond <- as.numeric(as.factor(d$label))

if(mediation) {
  process(data = d, y = "firm", x = "cond", 
          m =c("automation_1"), model = 4, effsize = 1, total = 1, stand = 1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
  
  process(data = d, y = "human", x = "cond", 
          m =c("automation_1"), model = 4, effsize = 1, total = 1, stand = 1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}

## ================================================================================================================
##                                              DATA VIZUALIZATION              
## ================================================================================================================

# Renaming and labeling for plots
d |>
  select(label, automation_1, firm, human) |>
  mutate(
    `Marketing Label` = ifelse(label == "Autopilot", "Autopilot", "Copilot"),
    Firm = firm,
    Human = human,
    Capability = automation_1
  ) |>
  select(`Marketing Label`, Firm, Human, Capability) -> d_plot

# Obtain mean and standard errors for condition and measure
d_plot |>
  group_by(`Marketing Label`) |>
  summarise(
    avg_F = mean(Firm),
    avg_H = mean(Human),
    se_F = sd(Firm)/sqrt(n()),
    se_H = sd(Human)/sqrt(n()),
    avg_C = mean(Capability),
    se_C = sd(Capability)/sqrt(n())
  ) -> d_plot

se_width <- 1.96

# Plot Firm Liability
ggplot(data = d_plot, aes(x=factor(`Marketing Label`), y=avg_F)) +
  geom_bar(stat="identity", alpha=.75) +
  geom_point(size=.75, color="black") +
  geom_errorbar(aes(ymin=avg_F-(se_F*se_width), ymax=avg_F+(se_F*se_width)), position = "dodge", 
                size=.25, color="black", width=.75) +
  geom_signif(
    y_position = c(75), xmin = c("Autopilot"), xmax = c("Copilot"),
    annotation = c("***"), tip_length = 0.1, color='black', size = .5, textsize = 3.5
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold")) +
  ylab("Mean Ratings") +
  xlab("") +
  ggtitle("Firm Liability") +
  scale_y_continuous(limits = c(0,100)) -> p1

p1

# Plot Human Liability
ggplot(data = d_plot, aes(x=factor(`Marketing Label`), y=avg_H)) +
  geom_bar(stat="identity", alpha=.75) +
  geom_point(size=.75, color="black") +
  geom_errorbar(aes(ymin=avg_H-(se_H*se_width), ymax=avg_H+(se_H*se_width)), position = "dodge", 
                size=.25, color="black", width=.75) +
  geom_signif(
    y_position = c(90), xmin = c("Autopilot"), xmax = c("Copilot"),
    annotation = c("***"), tip_length = 0.1, color='black', size = .5, textsize = 3.5
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12),
        axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ylab("") +
  xlab("") +
  ggtitle("Human Liability") +
  scale_y_continuous(limits = c(0,100)) -> p2

p2

# Combine both figures
ggarrange(p1,p2) |>
  annotate_figure(bottom = textGrob("Marketing Label", gp = gpar(cex = 1, fontsize=10, fontface="bold")))

ggsave("liability.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

# Plot Level of Automation
ggplot(data = d_plot, aes(x=factor(`Marketing Label`), y=avg_C)) +
  geom_bar(stat="identity", alpha=.75) +
  geom_point(size=.75, color="black") +
  geom_errorbar(aes(ymin=avg_C-(se_C*se_width), ymax=avg_C+(se_C*se_width)), position = "dodge", 
                size=.25, color="black", width=.75) +
  geom_signif(
    y_position = c(90), xmin = c("Autopilot"), xmax = c("Copilot"),
    annotation = c("***"), tip_length = 0.1, color='black', size = .5, textsize = 3.5
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold")) +
  ylab("Mean Ratings") +
  xlab("") +
  ggtitle("Level of Automation") +
  scale_y_continuous(limits = c(0,100)) -> p3

p3

ggsave("level_of_automation.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

