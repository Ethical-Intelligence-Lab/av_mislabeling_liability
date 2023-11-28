## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV RESPONSIBILITY STUDY | EXPERIMENT 6              
## ================================================================================================================

rm(list = ls()) 
library(sjstats)
library(tidyverse)
library(ggpubr)
library(ggsignif)
library(grid)
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
source('../e2_liability/process.R')

#==============================================================
# PRE-PROCESSING
#==============================================================

# Read full dataset
df <- read_csv("avc_e6_full.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]


# Rename column and labels
df$cond <- df$FL_12_DO
df$cond[df$cond == "FL_35"] <- "auto_op"
df$cond[df$cond == "FL_36"] <- "co_op"
df$cond[df$cond == "FL_50"] <- "auto_ft"
df$cond[df$cond == "FL_54"] <- "co_ft"

#==============================================================
# EXCLUSIONS
#==============================================================
df |>
  filter(
    att_1 == 2,
    att_2 == 2 ) -> df

recruited_participants <- dim(df)[1]

df |>
  filter(
    comp_1 == 2,
    comp_2 == 4,
    comp_3 == 1
  ) -> df

# Exclude more (based on Stuti's code)
df |>
  filter((cond == 'auto_op' & comp_4 == 2 |
            cond == 'auto_ft' & comp_4 == 3 |
            cond == 'co_op' & comp_4 == 2   | 
            cond == 'co_ft' & comp_4 == 3     )) -> df

final_n <- dim(df)[1]

excluded <- recruited_participants - final_n

# Identify transparency and labels
df |>
  mutate(
    transparency = ifelse(grepl("op", cond), "no", "yes"),
    label = ifelse(grepl("auto", cond), "auto", "co"),
  ) -> df

# Get data for different groups
# Elongate dataset for visualization and regression purposes

std_colnames <- c("id", "cond", "transparency", "label", "resp_soft", "resp_human", 
                  "liable_firm", "liable_human", "capability","age", "gender", "ai_knowledge", "license")

auto_op <- df |>
  filter(cond == "auto_op") |>
  dplyr::select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}1_[0-9]{1,}"), auto_1, age, gender, ai_knowledge_1, license)
colnames(auto_op) <- std_colnames

auto_ft <- df |>
  filter(cond == "auto_ft") |>
  dplyr::select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}3_[0-9]{1,}"), auto_2, age, gender, ai_knowledge_1, license)

colnames(auto_ft) <- std_colnames

co_op <- df |>
  filter(cond == "co_op") |>
  dplyr::select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}2_[0-9]{1,}"), co_1, age, gender, ai_knowledge_1, license)
colnames(co_op) <- std_colnames

co_ft <- df |>
  filter(cond == "co_ft") |>
  dplyr::select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}4_[0-9]{1,}"), co_2, age, gender, ai_knowledge_1, license)
colnames(co_ft) <- std_colnames

d <- rbind(auto_ft, co_ft, co_op, auto_op)
rm(auto_ft, co_ft, co_op, auto_op)

d |>
  mutate_at(
    c("resp_soft", "resp_human", "liable_firm", "liable_human", "capability",
      "age", "gender", "license", "ai_knowledge"),
    as.numeric
  ) -> d

cronbach.alpha(d[,c("resp_human", "liable_human")])
cronbach.alpha(d[,c("resp_soft", "liable_firm")])

d |>
  mutate(
    human = (resp_human + liable_human)/2,
    firm = (resp_soft + liable_firm)/2
  ) -> d

t.test(d[d$transparency == "no" & d$label == "auto",]$firm, 
       d[d$transparency == "no" & d$label == "co",]$firm)

#=================================================================================
# PARTICIPANT CHARACTERISTICS
#=================================================================================
# AGE
mean(d[d$age < 150,]$age) # filtering the ones who put year
hist(d[d$age < 150,]$age, main = "Histogram of Age", xlab = "Age")

# GENDER
d |>
  filter( gender < 3 ) |>
  mutate( 
    gender_lab = ifelse(gender == 1, "male", "female"),
    is_male = ifelse(gender == 1, 1, 0)
    ) -> gender
mean(gender$is_male)
barplot(table(gender$gender_lab), main="Participants' Gender")
rm(gender)

## AV KNOWLEDGE
mean(d$ai_knowledge) 
hist(d$ai_knowledge, xlab = 'AI Knowledge', main = 'Histogram of AI Knowledge')

#=================================================================================
# LABEL-TRANSPARENCY INTERACTION
#=================================================================================

## CAPABILITY
### ANOVA
cap_anova <- aov(capability ~ as.factor(label) * as.factor(transparency), data = d)
summary(cap_anova)
anova_stats(cap_anova)

rm(cap_anova)

## SOFTWARE RESPONSIBILITY
### ANOVA
rs_anova <- aov(resp_soft ~ as.factor(label) * as.factor(transparency), data = d)
summary(rs_anova)
anova_stats(rs_anova)

rm(rs_anova)

## HUMAN RESPONSIBILITY
### ANOVA
rh_anova <- aov(resp_human ~ as.factor(label) * as.factor(transparency), data = d)
summary(rh_anova)
anova_stats(rh_anova)

### t-tests
rh_t1 <- t.test(d[d$transparency == 'yes'& d$label == 'auto',]$resp_human,
                d[d$transparency == 'yes'& d$label == 'co',]$resp_human, paired = FALSE)
rh_t1
rh_t2 <- t.test(d[d$transparency == 'no' & d$label == 'auto',]$resp_human,
                d[d$transparency == 'no' & d$label == 'co',]$resp_human, paired = FALSE)
rh_t2

rm(rh_anova, rh_t1, rh_t2)

## FIRM LIABILITY
### ANOVA
fl_anova <- aov(liable_firm ~ as.factor(label) * as.factor(transparency), data = d)
summary(fl_anova)
anova_stats(fl_anova)

### t-tests
fl_t1 <- t.test(d[d$transparency == 'yes'& d$label == 'auto',]$liable_firm,
                d[d$transparency == 'yes'& d$label == 'co',]$liable_firm, paired = FALSE)
fl_t1
fl_t2 <- t.test(d[d$transparency == 'no' & d$label == 'auto',]$liable_firm,
                d[d$transparency == 'no' & d$label == 'co',]$liable_firm, paired = FALSE)
fl_t2

rm(fl_anova, fl_t1, fl_t2)

## HUMAN LIABILITY
### ANOVA
hl_anova <- aov(liable_human ~ as.factor(label) * as.factor(transparency), data = d)
summary(hl_anova)
anova_stats(hl_anova)

### t-tests
hl_t1 <- t.test(d[d$transparency == 'yes'& d$label == 'auto',]$liable_human,
                d[d$transparency == 'yes'& d$label == 'co',]$liable_human, paired = FALSE)
hl_t1
hl_t2 <- t.test(d[d$transparency == 'no' & d$label == 'auto',]$liable_human,
                d[d$transparency == 'no' & d$label == 'co',]$liable_human, paired = FALSE)
hl_t2

rm(hl_anova, hl_t1, hl_t2)

## HUMAN COMBINED
### ANOVA
h_anova <- aov(human ~ as.factor(label) * as.factor(transparency), data = d)
summary(h_anova)
anova_stats(h_anova)

### t-tests
h_t1 <- t.test(d[d$transparency == 'yes'& d$label == 'auto',]$human,
                d[d$transparency == 'yes'& d$label == 'co',]$human, paired = FALSE)
h_t1

cohen.d(d[d$transparency == 'yes'& d$label == 'auto',]$human,
       d[d$transparency == 'yes'& d$label == 'co',]$human)

h_t2 <- t.test(d[d$transparency == 'no' & d$label == 'auto',]$human,
                d[d$transparency == 'no' & d$label == 'co',]$human, paired = FALSE)
h_t2

cohen.d(d[d$transparency == 'no'& d$label == 'auto',]$human,
        d[d$transparency == 'no'& d$label == 'co',]$human)

## FIRM COMBINED
### ANOVA
f_anova <- aov(firm ~ as.factor(label) * as.factor(transparency), data = d)
summary(f_anova)
anova_stats(f_anova)

### t-tests
f_t1 <- t.test(d[d$transparency == 'yes'& d$label == 'auto',]$firm,
               d[d$transparency == 'yes'& d$label == 'co',]$firm, paired = FALSE)
f_t1

cohen.d(d[d$transparency == 'yes'& d$label == 'auto',]$firm,
       d[d$transparency == 'yes'& d$label == 'co',]$firm)
       
f_t2 <- t.test(d[d$transparency == 'no' & d$label == 'auto',]$firm,
               d[d$transparency == 'no' & d$label == 'co',]$firm, paired = FALSE)
f_t2

cohen.d(d[d$transparency == 'no'& d$label == 'auto',]$firm,
        d[d$transparency == 'no'& d$label == 'co',]$firm)

d |>
  group_by(transparency, label) |>
  summarize(
    sd_human = sd(human),
    sd_firm = sd(firm),
    m_human = mean(human),
    m_firm = mean(firm)
  ) -> a
#=================================================================================
# PLOTS LABEL-TRANSPARENCY
#=================================================================================
std.error <- function(x) sd(x)/sqrt(length(x))

d |>
  gather(key = "DV", value = "Value", 
         firm, human) |>
  mutate(
    DV = ifelse( DV == "firm", "Firm Liability", "Human Liability"),
    `Marketing Label` = case_when(
                    label == "auto" ~ "Autopilot",
                    label == "co" ~ "Copilot"
    ),
    Transparency = case_when(
                    transparency == "no" ~ "Not Transparent",
                    transparency == "yes" ~ "Transparent"
    )
  ) |>
  group_by(`Marketing Label`, Transparency, DV) |>
  summarise( 
    mean = mean(Value),
    se = std.error(Value) 
    ) -> d_plot

plot_did <- function(df=d_plot, dv, signif=c("*","*","*"), yaxis=TRUE, ypos=c(100, 100, 114)) {
  
  d_plot <- df |>
    filter(DV == dv)
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=Transparency, y=mean, fill=`Marketing Label`)) +
    geom_bar(stat="identity", position="dodge", alpha=.75) +
    geom_errorbar(aes(ymin=mean-(se*se_width), ymax=mean+(se*se_width)), position = position_dodge(width=.9), 
                  size=.25, color="black", width=.5) +
    geom_point(aes(y=mean),position=position_dodge(width = .9), size=.5, color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold", size=10)
          ) +
    geom_signif(
      y_position = ypos, xmin = c(0.8, 1.8, 1.0), xmax = c(1.2, 2.2, 2.0),
      annotation = signif, tip_length = 0.1, color='black', size = .25, textsize = 3.5 
    ) +
    scale_fill_grey() +
    scale_color_grey() +
    ggtitle(dv) +
    xlab("Transparency") +
    ylab("Response") +
    scale_y_continuous(limits = c(0,118), breaks = c(0,20,40,60,80,100)) -> p
  
  if(!yaxis) {
    p <- p +
      theme( axis.line.y = element_line(color = "white"),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())
  }
  
  return(p)
}

plot_did(dv = "Human Liability", signif = c("*", "ns", "*"), yaxis=F) -> p1
plot_did(dv = "Firm Liability", signif = c("*", "ns", "+"), ypos = c(80,80,94))  -> p2
p2 
plot_did(dv = "Human Driver Responsibility", signif = c("*", "ns", "*"))  -> p3
plot_did(dv = "AV Software Responsibility", signif = c("+", "ns", "ns"), yaxis=F, ypos = c(60,60,74)) -> p4

p2
ggarrange(p2 + rremove("ylab") + rremove("xlab"),
          p1 + rremove("ylab") + rremove("xlab"),
          ncol = 2, common.legend = TRUE) |>
          annotate_figure( left = textGrob("Mean Ratings", rot = 90, vjust = 1, gp = gpar(cex = .8, fontface = "bold")),
                           bottom = textGrob("Firm Transparency Condition", gp = gpar(cex = .8, fontface = "bold"))) -> p
p

ggsave("firm_transparency.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

rm(p1, p2, p3, p4)

#=================================================================================
# MEDIATION of Perceived Capability on DV
#=================================================================================
d |>
  mutate_at( c("transparency", "label"), as.factor) |>
  mutate_at( c("transparency", "label"), as.numeric) -> d_process

## NOTE
### - transparency no ~ 1, yes ~ 2
### - label auto ~ 1, co ~ 2

# SOFTWARE RESPONSBIBILITY
process(data = d_process, y = "resp_soft", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN RESPONSBIBILITY
process(data = d_process, y = "resp_human", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# FIRM LIABILITY
process(data = d_process, y = "liable_firm", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN LIABILITY
process(data = d_process, y = "liable_human", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# FIRM COMBINED
process(data = d_process, y = "firm", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN COMBINED
process(data = d_process, y = "human", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

#=================================================================================
# MODERATED (transparency) MEDIATION (capability) MODEL 14
#=================================================================================
# SOFTWARE RESPONSBIBILITY
process(data = d_process, y = "resp_soft", x = "label", 
        m =c("capability"), w="transparency", model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN RESPONSBIBILITY
process(data = d_process, y = "resp_human", x = "label", 
        m =c("capability"), w="transparency", model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# FIRM LIABILITY
process(data = d_process, y = "liable_firm", x = "label", 
        m =c("capability"), w="transparency", model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN LIABILITY
process(data = d_process, y = "liable_human", x = "label", 
        m =c("capability"), w="transparency", model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# FIRM COMBINED
process(data = d_process, y = "firm", x = "label", 
        m =c("capability"), w="transparency", model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN COMBINED
process(data = d_process, y = "human", x = "label", 
        m =c("capability"), w="transparency", model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

#=================================================================================
# CAPABILITY-TRANSPARENCY INTERACTION
#=================================================================================
# HUMAN RESPONSIBILITY
anova_rh <- aov(resp_human ~ capability * as.factor(transparency), data = d)
summary(anova_rh)
anova_stats(anova_rh)

# HUMAN LIABILITY
anova_lh <- aov(liable_human ~ capability * as.factor(transparency), data = d)
summary(anova_lh)
anova_stats(anova_lh)

# SOFTWARE RESPONSIBILITY
anova_rs <- aov(resp_soft ~ capability * as.factor(transparency), data = d)
summary(anova_rs)
anova_stats(anova_rs)

# FIRM LIABILITY
anova_lf <- aov(liable_firm ~ capability * as.factor(transparency), data = d)
summary(anova_lf)
anova_stats(anova_lf)

# FIRM COMBINED
anova_rs <- aov(firm ~ capability * as.factor(transparency), data = d)
summary(anova_rs)
anova_stats(anova_rs)

# HUMAN COMBINED
anova_lf <- aov(human ~ capability * as.factor(transparency), data = d)
summary(anova_lf)
anova_stats(anova_lf)

#=================================================================================
# PLOTS CAPABLE (binarized capability) - TRANSPARENCY
#=================================================================================
d$capable <- ifelse(d$capability < 4, "no", "yes")

d |>
  gather(key = "DV", value = "Value", 
         resp_soft, resp_human, liable_firm, liable_human) |>
  mutate(
    DV = case_when( DV == "liable_human" ~ "Human is Liable",
                    DV == "liable_firm" ~ "Firm is Liable",
                    DV == "resp_human" ~ "Human is Responsible",
                    DV == "resp_soft" ~ "Software is Responsible",),
    Capable = capable,
    Transparency = case_when(
      transparency == "no" ~ "Not Transparent",
      transparency == "yes" ~ "Transparent"
    )
  ) |>
  group_by(Capable, Transparency, DV) |>
  summarize( 
    mean = mean(Value),
    se = std.error(Value) 
  ) -> d_plot

plot_did <- function(df=d_plot, dv, signif=c("*","*","*")) {
  
  d_plot <- df |>
    filter(DV == dv)
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=Transparency, y=mean, fill=Capable, color=Capable)) +
    geom_bar(stat="identity", position="dodge", alpha=.75) +
    geom_errorbar(aes(ymin=mean-(se*se_width), ymax=mean+(se*se_width)), position = "dodge", 
                  size=.25, color="black", width=.9) +
    geom_point(aes(y=mean),position=position_dodge(width = .9), size=.75, color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    geom_signif(
      y_position = c(100, 100, 110), xmin = c(0.8, 1.8, 1.0), xmax = c(1.2, 2.2, 2.0),
      annotation = signif, tip_length = 0.1, color='black', size = .25, textsize = 3
    ) +
    scale_fill_grey() +
    scale_color_grey() +
    ggtitle(dv) +
    xlab("Transparency") +
    ylab("Response") -> p
  
  return(p)
}

plot_did(dv = "Human is Liable", signif = c("*", "ns", ".")) -> p1
plot_did(dv = "Firm is Liable", signif = c("*", "*", "*"))  -> p2
plot_did(dv = "Human is Responsible", signif = c("*", "ns", "."))  -> p3
plot_did(dv = "Software is Responsible", signif = c("ns", "ns", "*")) -> p4

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
rm(p1, p2, p3, p4)


#=================================================================================
# MODERATED (transparency) MEDIATION (capability) MODEL 15
#=================================================================================
# SOFTWARE RESPONSBIBILITY
process(data = d_process, y = "resp_soft", x = "label", 
        m =c("capability"), w="transparency", model = 15, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN RESPONSBIBILITY
process(data = d_process, y = "resp_human", x = "label", 
        m =c("capability"), w="transparency", model = 15, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# FIRM LIABILITY
process(data = d_process, y = "liable_firm", x = "label", 
        m =c("capability"), w="transparency", model = 15, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN LIABILITY
process(data = d_process, y = "liable_human", x = "label", 
        m =c("capability"), w="transparency", model = 15, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# FIRM COMBINED
process(data = d_process, y = "firm", x = "label", 
        m =c("capability"), w="transparency", model = 15, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN COMBINED
process(data = d_process, y = "human", x = "label", 
        m =c("capability"), w="transparency", model = 15, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

