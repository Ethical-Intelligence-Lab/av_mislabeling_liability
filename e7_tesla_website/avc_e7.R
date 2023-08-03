## ================================================================================================================
##                                 Harvard Business School, Ethical Iopelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV RESPONSIBILITY STUDY | EXPERIMEop 5               
## ================================================================================================================
## clear workspace
rm(list = ls())

# libraries
library(sjstats)
library(tidyverse)
library(ggpubr)
library(ggsignif)
library(knitr)
library(pander)
#source('../e2_liability/process.R')

# Read full dataset
df <- read_csv("AV_Culpability_E7.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

#==============================================================
# EXCLUSIONS & DATA CLEANING
#==============================================================
## Failed Attention Checks
df |>
  filter(
    att_1 == 2,
    att_2 == 2) -> df

n_original <- length(df$StartDate)

## Comprehension Checks
df |>
  filter(
    comp_1 == 2,
    comp_2 == 4
  ) -> df

n_excluded <- n_original - length(df$StartDate)

prop_excluded <- n_excluded / n_original

# Relevant Columns and Elongate Data
rel_col <- c("level_1", "level_2", "hard_1_1", "info_text", "level_3", "gender", "age", "license", "ai_knowledge_1")
df <- df[,rel_col]

df |>
  filter(level_1 == 1) |>
  select(level_1, level_2, hard_1_1, gender, age, license, ai_knowledge_1) -> found

colnames(found) <- c("found", "auto_level", "difficulty", "gender", "age", "license", "ai_knowledge")

df |>
  filter(level_1 == 2) |>
  select(level_1, level_3, hard_1_1, gender, age, license, ai_knowledge_1) -> not_found

colnames(not_found) <- colnames(found)

df <- rbind(found, not_found)

rm(found, not_found)

df |>
  summarize_all(as.numeric) |>
  mutate( found = ifelse(found == 1, TRUE, FALSE ))-> df

#==============================================================
# DEMOGRAPHICS
#==============================================================
## Age
mean(df$age, rm.na = T)
hist(df$age, main = "Age Distribution")

## Gender
n_male <- length(df[df$gender == 1,]$gender)
n_female <- length(df[df$gender == 2,]$gender)
prop_female <- n_female / (n_male + n_female)
rm(n_male, n_female, prop_female)

## AI Knowledge
mean(df$ai_knowledge, rm.na = T)
hist(df$ai_knowledge, main = "Distribution of AI Knowledge")

## License
prop.table(table(df$license))

#==============================================================
# Proportion Correct
#==============================================================
df$is_correct <- df$auto_level == 2

prop_correct <- mean(df$is_correct)
prop_correct

prop_found <- mean(df$found)
prop_found

df$found_label <- ifelse(df$found, "Found", "Not Found")
df$is_correct_label <- ifelse(df$is_correct, "Correct", "Wrong")
# prop correct by if found
tabulate <- table(df$found_label, df$is_correct_label)
tabulate

kable(tabulate, format = "latex", booktabs=T) 

kable(round(prop.table(tabulate),3), format = "latex", booktabs=T)

kable(round(prop.table(tabulate, margin = 1),3), format = "latex", booktabs=T)

# t-test for those who found vs not found
found_auto_level <- df[df$found,]$auto_level
notfound_auto_level <- df[!df$found,]$auto_level

t.test(found_auto_level, notfound_auto_level)

ggplot(data = df, aes(found, auto_level)) +
  geom_violin() +
  theme_classic() +
  xlab("Found Automation Level of Tesla") +
  ylab("Perceived Level of Automation")

rm(found_auto_level, notfound_auto_level)

# Difficulty == 50? 
difficulty <- df[!is.na(df$difficulty),]$difficulty 
t.test(difficulty, mu = 50)

#==============================================================
# ANOVA & T-TESTS
#==============================================================

