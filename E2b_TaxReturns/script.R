## clear workspace
rm(list = ls()) 

## install packages
library(dplyr)
library(grid)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
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
               'nlme',            # get p values for mixed effect model
               'DescTools',        # get Cramer's V
               'rstatix',
               'effects'
)

# PROCESS Analysis (Set TRUE if you wish to run PROCESS code)
mediation <- F
if(mediation) {
  source("../process.R")
}

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read.csv('data.csv') 
d <- d[-c(1,2),]

d |>
  filter(att_1 == 2, att_2 == 5) -> d

nrow(d)

d |>
  filter(comp_1 == 2, comp_2 == 4) -> d

nrow(d)

d |>
  mutate(
    label = Tax.Auto.Prepare.Tax.Co.Prepare,
    age = as.numeric(age),
    automation_1 = as.numeric(automation_1),
    software_resp_1 = as.numeric(software_resp_1),
    software_liable_1 = as.numeric(software_liable_1),
    human_resp_1 = as.numeric(human_resp_1),
    human_liable_1 = as.numeric(human_liable_1)
  ) -> d

## ================================================================================================================
##                                            DEMOGRAPHICS                
## ================================================================================================================

mean(d$age, na.rm = T)

prop.table(table(d$gender))[[1]]

## ================================================================================================================
##                                            ANALYSES                
## ================================================================================================================

cronbach.alpha(d[,c("software_resp_1", "software_liable_1")])
cronbach.alpha(d[,c("human_resp_1", "human_liable_1")])

d |>
  mutate(
    firm_liability = (software_resp_1 + software_liable_1) / 2,
    human_liability = (human_resp_1 + human_liable_1) / 2,
  ) -> d

## Capability
t.test(d[d$label == "Tax Auto-Prepare",]$automation_1, 
       d[d$label == "Tax Co-Prepare",]$automation_1)

sd(d[d$label == "Tax Auto-Prepare",]$automation_1)
sd(d[d$label == "Tax Co-Prepare",]$automation_1)

cohen.d(d[d$label == "Tax Auto-Prepare",]$automation_1, 
       d[d$label == "Tax Co-Prepare",]$automation_1)

## Firm Liability
t.test(d[d$label == "Tax Auto-Prepare",]$firm_liability, 
       d[d$label == "Tax Co-Prepare",]$firm_liability)

sd(d[d$label == "Tax Auto-Prepare",]$firm_liability)
sd(d[d$label == "Tax Co-Prepare",]$firm_liability)

cohen.d(d[d$label == "Tax Auto-Prepare",]$firm_liability, 
        d[d$label == "Tax Co-Prepare",]$firm_liability)

## Human Liability
t.test(d[d$label == "Tax Auto-Prepare",]$human_liability, 
       d[d$label == "Tax Co-Prepare",]$human_liability)

sd(d[d$label == "Tax Auto-Prepare",]$human_liability)
sd(d[d$label == "Tax Co-Prepare",]$human_liability)

cohen.d(d[d$label == "Tax Auto-Prepare",]$human_liability, 
        d[d$label == "Tax Co-Prepare",]$human_liability)


## ================================================================================================================
##                                              DATA VIZUALIZATION              
## ================================================================================================================

# Renaming and labeling for plots
d |>
  select(label, automation_1, firm_liability, human_liability) |>
  mutate(
    `Marketing Label` = ifelse(label == "Tax Auto-Prepare", "Tax Auto-Prepare", "Tax Co-Prepare"),
    Firm = firm_liability,
    Human = human_liability,
    Capability = automation_1
  ) |>
  select(`Marketing Label`, Firm, Human, Capability) -> d_plot

# Obtain mean and standard errors for condition and measure
d_plot |>
  group_by(`Marketing Label`) |>
  summarize(
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
    y_position = c(100), xmin = c("Tax Auto-Prepare"), xmax = c("Tax Co-Prepare"),
    annotation = c("ns"), tip_length = 0.1, color='black', size = .5, textsize = 3.5
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
    y_position = c(70), xmin = c("Tax Auto-Prepare"), xmax = c("Tax Co-Prepare"),
    annotation = c("ns"), tip_length = 0.1, color='black', size = .5, textsize = 3.5
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

ggsave("liability_ascriptions.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

# Plot Level of Automation
ggplot(data = d_plot, aes(x=factor(`Marketing Label`), y=avg_C)) +
  geom_bar(stat="identity", alpha=.75) +
  geom_point(size=.75, color="black") +
  geom_errorbar(aes(ymin=avg_C-(se_C*se_width), ymax=avg_C+(se_C*se_width)), position = "dodge", 
                size=.25, color="black", width=.75) +
  geom_signif(
    y_position = c(100), xmin = c("Tax Auto-Prepare"), xmax = c("Tax Co-Prepare"),
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

