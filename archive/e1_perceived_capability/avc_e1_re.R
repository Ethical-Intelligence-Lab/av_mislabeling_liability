## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV CULPABILITY STUDY | EXPERIMENT 1               
## ================================================================================================================
## clear workspace
rm(list = ls()) 

library(ggpubr)
library(tidyverse)

d <- read.csv('./avc_e1.csv') 

d |>
  rename(cond = FL_12_DO) |>
  mutate(
    case_match(cond,
               "FL_35" ~ "auto",
               "FL_36" ~ "co",
               "FL_37" ~ "dless")
  ) -> d

## subjects randomized:
table(d$cond)

## ================================================================================================================
##                                                   EXCLUSIONS                
## ================================================================================================================



## number of participants BEFORE exclusions: 
n_before_exclusions <- nrow(d) # extracting number of rows only, not columns
n_before_exclusions

## attention exclusions: 
# remove responses from data frame that failed attention checks
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
n_original <- nrow(d) # number of participants should decrease after attention exclusions
n_original

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp_1 == 2 & d$comp_2 == 4))
n_final <- nrow(d)
n_final 

## number of participants AFTER exclusions: 
percent_excluded <- (n_original - n_final)/n_original 
percent_excluded
table(d$cond)

## ================================================================================================================
##                                                    Subsetting                 
## ================================================================================================================
relevant_columns <- colnames(d)[grepl('use_|value_|auto_|co_|dr_', colnames(d))]
relevant_columns <- relevant_columns[!grepl("av_frame", relevant_columns)]
relevant_columns <- c(relevant_columns)

d <- d[, relevant_columns]

new_colnames <- c('auto','use1','use2','use3','use4','value1','value2','value3','value4', 'cond')

# Getting autopilot
auto <- d[, grepl("auto|use_1|value_1", relevant_columns)] |>
  drop_na() |> 
  mutate(
    cond = "auto"
  )
colnames(auto) <- new_colnames

# Getting copilot
co <- d[, grepl("co|use_2|value_2", relevant_columns)] |>
  drop_na() |> 
  mutate(
    cond = "co"
  )
colnames(co) <- new_colnames

# Getting driverless
dless <- d[, grepl("dr|use_3|value_3", relevant_columns)] |>
  drop_na() |> 
  mutate(
    cond = "dless"
  )
colnames(dless) <- new_colnames

d_merged <- rbind(auto, co, dless)

rm(dless, auto, co)

# Filtering extreme price suggestions ???
d_merged |>
  filter(value4 != 30, value4 != 50, value4 != 480000) -> d_merged

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(d$age, na.rm = TRUE) ## mean age 
hist(d$age)

## gender
table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
table(d$gender)[2]/sum(table(d$gender)) ## percentage of females
prop.table

## av knowledge
mean(d$ai_knowledge_1, trim = 0, na.rm = TRUE) ## mean av knowledge 
hist(d$ai_knowledge)

## license
table(d$license)[1]/sum(table(d$license)) ## percentage with driver's license
table(d$license)[2]/sum(table(d$license)) ## percentage without driver's license
