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
    att_2 == 2,
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
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}1_[0-9]{1,}"), auto_1, age, gender, ai_knowledge_1, license)
colnames(auto_op) <- std_colnames

auto_ft <- df |>
  filter(cond == "auto_ft") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}3_[0-9]{1,}"), auto_2, age, gender, ai_knowledge_1, license)
colnames(auto_ft) <- std_colnames

co_op <- df |>
  filter(cond == "co_op") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}2_[0-9]{1,}"), co_1, age, gender, ai_knowledge_1, license)
colnames(co_op) <- std_colnames

co_ft <- df |>
  filter(cond == "co_ft") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}4_[0-9]{1,}"), co_2, age, gender, ai_knowledge_1, license)
colnames(co_ft) <- std_colnames

d <- rbind(auto_ft, co_ft, co_op, auto_op)
rm(auto_ft, co_ft, co_op, auto_op)

d |>
  mutate_at(
    c("resp_soft", "resp_human", "liable_firm", "liable_human", "capability",
      "age", "gender", "license", "ai_knowledge"),
    as.numeric
  ) -> d

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

### t-tests
cap_t1 <- t.test(d[d$transparency == 'yes'& d$label == 'auto',]$capability,
                d[d$transparency == 'yes'& d$label == 'co',]$capability, paired = FALSE)
cap_t1
cap_t2 <- t.test(d[d$transparency == 'no' & d$label == 'auto',]$capability,
                d[d$transparency == 'no' & d$label == 'co',]$capability, paired = FALSE)
cap_t2

rm(cap_anova, cap_t1, cap_t2)

## SOFTWARE RESPONSIBILITY
### ANOVA
rs_anova <- aov(resp_soft ~ as.factor(label) * as.factor(transparency), data = d)
summary(rs_anova)
anova_stats(rs_anova)

### t-tests
rs_t1 <- t.test(d[d$transparency == 'yes'& d$label == 'auto',]$resp_soft,
             d[d$transparency == 'yes'& d$label == 'co',]$resp_soft, paired = FALSE)
rs_t1
rs_t2 <- t.test(d[d$transparency == 'no' & d$label == 'auto',]$resp_soft,
                d[d$transparency == 'no' & d$label == 'co',]$resp_soft, paired = FALSE)
rs_t2

rm(rs_anova, rs_t1, rs_t2)

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

#=================================================================================
# PLOTS LABEL-TRANSPARENCY
#=================================================================================
std.error <- function(x) sd(x)/sqrt(length(x))

d |>
  gather(key = "DV", value = "Value", 
         resp_soft, resp_human, liable_firm, liable_human) |>
  mutate(
    DV = case_when( DV == "liable_human" ~ "Human is Liable",
                    DV == "liable_firm" ~ "Firm is Liable",
                    DV == "resp_human" ~ "Human is Responsible",
                    DV == "resp_soft" ~ "Software is Responsible",)
  ) |>
  group_by(label, transparency, DV) |>
  summarize( 
    mean = mean(Value),
    se = std.error(Value) 
    ) -> d_plot

plot_did <- function(df=d_plot, dv) {
  
  d_plot <- df |>
    filter(DV == dv)
  
  E_no <- round(d_plot[d_plot$transparency == "no" & d_plot$label == "auto",]$mean[1] - 
                  d_plot[d_plot$transparency == "no" & d_plot$label == "co",]$mean[1], 3)
  
  E_yes <- round(d_plot[d_plot$transparency == "yes" & d_plot$label == "auto",]$mean[1] - 
                   d_plot[d_plot$transparency == "yes" & d_plot$label == "co",]$mean[1], 3)
  
  DiD <- E_no - E_yes
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=transparency, y=mean, fill=label, color=label)) +
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin=mean-(se*se_width), ymax=mean+(se*se_width)), position = "dodge", size=.25) +
    geom_point(aes(y=mean),position=position_dodge(width = .9), size=.75) +
    theme_light() +
    geom_signif(
      y_position = c(100, 100, 110), xmin = c(0.8, 1.8, 1.0), xmax = c(1.2, 2.2, 2.0),
      annotation = c(E_no, E_yes, DiD), tip_length = 0.1, color='black', size = .25, textsize = 2
    ) +
    scale_fill_grey() +
    scale_color_grey() +
    ggtitle(dv) +
    ylab("response") -> p
  
  return(p)
}

plot_did(dv = "Human is Liable")          -> p1
plot_did(dv = "Firm is Liable")           -> p2
plot_did(dv = "Human is Responsible")     -> p3
plot_did(dv = "Software is Responsible")  -> p4

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
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

#=================================================================================
# MODERATED (transparency) MEDIATION (capability) MODEL 8
#=================================================================================
# SOFTWARE RESPONSBIBILITY
process(data = d_process, y = "resp_soft", x = "label", 
        m =c("capability"), w="transparency", model = 8, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN RESPONSBIBILITY
process(data = d_process, y = "resp_human", x = "label", 
        m =c("capability"), w="transparency", model = 8, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# FIRM LIABILITY
process(data = d_process, y = "liable_firm", x = "label", 
        m =c("capability"), w="transparency", model = 8, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

# HUMAN LIABILITY
process(data = d_process, y = "liable_human", x = "label", 
        m =c("capability"), w="transparency", model = 8, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

#=================================================================================
# LABEL-TRANSPARENCY INTERACTION
#=================================================================================
anova_rh <- aov(resp_human ~ capability * as.factor(transparency), data = d)
summary(anova_rh)
anova_stats(anova_rh)

anova_lh <- aov(liable_human ~ capability * as.factor(transparency), data = d)
summary(anova_lh)
anova_stats(anova_lh)

anova_rs <- aov(resp_soft ~ capability * as.factor(transparency), data = d)
summary(anova_rs)
anova_stats(anova_rs)

anova_lf <- aov(liable_firm ~ capability * as.factor(transparency), data = d)
summary(anova_lf)
anova_stats(anova_lf)
