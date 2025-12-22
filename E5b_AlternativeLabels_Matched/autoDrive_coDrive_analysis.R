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
               'effects',
               'lavaan',
               'semTools'
)

library(diagram)

# PROCESS Analysis (Set TRUE if you wish to run PROCESS code)
mediation <- FALSE
if(mediation) {
  source("../process.R")
}

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read_csv('AutoDrive vs CoDrive full data.csv')

d |>
  mutate_if(all.is.numeric, as.numeric) -> d

## Attention Check
d |>
  filter(att_1 == "Paul" & att_2 == "Neither Agree nor Disagree") -> d

n_recruited <- nrow(d); n_recruited

## Comp Check
d |>
  filter(comp_1 == "Different types of vehicles based on how many driving tasks are controlled by humans vs. machines" & comp_2 == "HUMAN, HUMAN, HUMAN, HUMAN" & comp_3 == "The ${e://Field/label} system was 'switched on' at the time of the accident") -> d

## Exclude previews if they ended up here
d <- d[d$prolific_id != "5ebdf7e133dc3114decd0739", ]
d <- d[d$Finished == TRUE, ]

n_final <- nrow(d); n_final
n_excluded <- n_recruited - n_final; n_excluded

## ================================================================================================================
##                                                 DEMOGRAPHICS                 
## ================================================================================================================

mean(as.numeric(d$age), na.rm = T)
prop.table(table(d[d$gender == "Male" | d$gender == "Female",]$gender))[[1]]


table(d$label)

## ================================================================================================================
##                                                 ANALYSIS                
## ================================================================================================================

treatment = "AutoDrive"
control = "CoDrive"

## Capability 
t.test(d[d$label == treatment,]$automation,
       d[d$label == control,]$automation)

sd(d[d$label == treatment,]$automation)
sd(d[d$label == control,]$automation)

cohen.d(d[d$label == treatment,]$automation,
        d[d$label == control,]$automation)

cronbach.alpha(d[,c("firm_resp_1", "firm_liable_1")])
cronbach.alpha(d[,c("human_liable_1", "human_resp_1" )])

## Discriminant Validity
## Reverse Coding Human
d |> mutate(
  hr = -(`human_resp_1` - 100),
  hl = -(`human_liable_1` - 100),
  fr = `firm_resp_1`,
  fl = `firm_liable_1`
) -> d

countf.model <- ' firm   =~ fr + fl
                  human  =~ hr + hl '

htmt(countf.model, d)

## Covariance Matrix
countf.cov <- cov(d[, c("fr", "fl", "hr", "hl")])

## HTMT using arithmetic mean
htmt(countf.model, sample.cov = countf.cov, htmt2 = FALSE)

d$human <- (d$human_liable_1 + d$human_resp_1) / 2
d$firm <-(d$firm_liable_1 + d$firm_resp_1) / 2

### Firm Liability

t.test(d[d$label == treatment,]$firm,
       d[d$label == control,]$firm)

sd(d[d$label == treatment,]$firm)
sd(d[d$label == control,]$firm)

cohen.d(d[d$label == treatment,]$firm,
        d[d$label == control,]$firm)

firm_mod <- lm(firm ~ label + ai_knowledge_1, data = d)
summary(firm_mod)


### Human Liability 

t.test(d[d$label == treatment,]$human,
       d[d$label == control,]$human)

sd(d[d$label == treatment,]$human)
sd(d[d$label == control,]$human)

cohen.d(d[d$label == treatment,]$human,
        d[d$label == control,]$human)

human_mod <- lm(human ~ label + ai_knowledge_1, data = d)
summary(human_mod)

## ================================================================================================================
##                                              PROCESS             
## ================================================================================================================
d$label <- as.factor(d$label)
d$label <- relevel(d$label, ref = "CoDrive")

d$cond <- as.numeric(as.factor(d$label))


firm_res <- process(data = d, y = "firm", x = "cond",
          m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1,
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321, conf = 97.06, save = 2)

human_res <- process(data = d, y = "human", x = "cond",
          m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1,
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321, conf = 97.06, save = 2)



## ================================================================================================================
##                                              DATA VIZUALIZATION              
## ================================================================================================================

# Renaming and labeling for plots
d |>
  select(cond,label, automation, firm, human) |>
  mutate(
    `Label` = ifelse(cond == 1, control, treatment),
    `Firm Liability` = firm,
    `Human Liability` = human
  ) |>
  select(`Label`, `Firm Liability`, `Human Liability`) |>
  gather(key = "DV", value = "Value", `Firm Liability`, `Human Liability`) -> d_plot

# Obtain mean and standard errors for condition and measure
d_plot |>
  dplyr::group_by(`Label`, DV) |>
  dplyr::summarize(
    avg_value = mean(Value),
    se_value = sd(Value)/sqrt(n())
  ) -> d_plot

se_width <- 1.96

# Plot Firm Liability
ggplot(data = d_plot, aes(fill=`Label`, y=avg_value, x = DV)) +
  geom_bar(stat="identity", position="dodge", alpha=.75, width=.6) +
  geom_point(position=position_dodge(width = .6), size=.5, color="black") +
  geom_errorbar(aes(ymin=avg_value-(se_value*se_width), ymax=avg_value+(se_value*se_width)), position = position_dodge(width=.6), 
                size=.25, color="black", width=.25) +
  geom_signif(
    y_position = c(95), xmin = c(0.85, 1.85), xmax = c(1.15, 2.15),
    annotation = c("***","***"), tip_length = 0.1, color='black', size = .25, textsize = 3.5 
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold"), legend.position = "top") +
  ylab("Mean Ratings") +
  xlab("") +
  ggtitle("") +
  scale_fill_grey() +
  scale_color_grey() +
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) -> p1

p1

ggsave("liability.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")


##======================================================
##                    Covariates
##======================================================

## DV: Firm
summary(lm(firm ~ label + gender, d))
summary(lm(firm ~ label + as.factor(ethnicity), d))
summary(lm(firm ~ label + education, d))
summary(lm(firm ~ label + ai_knowledge_1, d))
summary(lm(firm ~ label + age, d))

## DV: Human
summary(lm(human ~ label + gender, d))
summary(lm(human ~ label + as.factor(ethnicity), d))
summary(lm(human ~ label + education, d))
summary(lm(human ~ label + ai_knowledge_1, d))
summary(lm(human ~ label + age, d))


