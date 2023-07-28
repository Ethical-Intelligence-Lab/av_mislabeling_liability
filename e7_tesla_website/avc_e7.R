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
#source('../e2_liability/process.R')

# Read full dataset
df <- read_csv("e7_simulated.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

#==============================================================
# EXCLUSIONS & DATA CLEANING
#==============================================================
## Failed Attention Checks
df |>
  filter(
    att_1 == "Paul",
    att_2 == "Green") -> df

## Comprehension Checks
df |>
  filter(
    comp_1 == "Different types of vehicles based on how many driving tasks are controlled by humans vs. machines",
    comp_2 == "HUMAN, HUMAN, HUMAN, HUMAN"
  ) -> df

## TODO: get prop is excluded

### TODO: REMOVE THIS FOR NON SIMULATED
df <- read_csv("e7_simulated.csv")
df <- df[-c(1,2),]

# Relevant Columns and Elongate Data
rel_col <- c("level_1", "level_2", "hard_1_1", "info_text", "level_3", "gender", "age", "license", "ai_knowledge_1")
df <- df[,rel_col]

df |>
  filter(level_1 == "Yes") |>
  select(level_1, level_2, hard_1_1, gender, age, license, ai_knowledge_1) -> found

colnames(found) <- c("found", "auto_level", "difficulty", "gender", "age", "license", "ai_knowledge")

df |>
  filter(level_1 == "No") |>
  select(level_1, level_3, hard_1_1, gender, age, license, ai_knowledge_1) -> not_found

colnames(not_found) <- colnames(found)

df <- rbind(found, not_found)
rm(found, not_found)

# TODO: Remove this lol
n <- length(df$age)
df$age <- sample(18:65, n, replace = T)

# But not this
df$auto_level <- as.numeric(gsub("Level ", "", df$auto_level))
df$difficulty <- as.numeric(df$difficulty)
df$ai_knowledge <- as.numeric(df$ai_knowledge)

#==============================================================
# DEMOGRAPHICS
#==============================================================
## Age
mean(df$age, rm.na = T)
hist(df$age, main = "Age Distribution")

## Gender
n_male <- length(df[df$gender == "Male",]$gender)
n_female <- length(df[df$gender == "Female",]$gender)
prop_female <- n_female / (n_male + n_female)
rm(n_male, n_female, prop_female)

## AI Knowledge
mean(df$ai_knowledge, rm.na = T)
hist(df$ai_knowledge, main = "Distribution of AI Knowledge")

## License
prop.table(table(df$license))

#==============================================================
# ANOVA & T-TESTS
#==============================================================
