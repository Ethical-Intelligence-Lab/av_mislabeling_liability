## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV CULPABILITY STUDY | EXPERIMENT 1               
## ================================================================================================================
## clear workspace
rm(list = ls()) 

# Import libraries required
library(tidyverse)
library(readxl)

# =====================================================================================
# Read XLSX files
# =====================================================================================

nyt <- read_xlsx("./data/ArticleCoding - New York Times.xlsx", sheet = "Articles", skip = 2)
usa <- read_xlsx("./data/ArticleCoding - USA Today.xlsx", sheet = 2, skip = 3)
wsj <- read_xlsx("./data/ArticleCoding - Wall Street Journal.xlsx", sheet = 2, skip = 2)
wapo <- read_xlsx("./data/ArticleCoding - Washington Post.xlsx", sheet = 1, skip = 2)
nyp <- read_xlsx("./data/ArticleCoding - New York Post.xlsx", sheet = 1, skip = 2)

# =====================================================================================
# Get Relevant Columns and Bind them!
# =====================================================================================
relevant_col <- c("Journal", "Categorization for Coder 1", "Categorization for Coder 2")

nyt <- nyt[,relevant_col]
usa <- usa[, relevant_col]
wapo <- wapo[, relevant_col]
wsj <- wsj[, c(4,7,8)]
nyp <- nyp[, c(5,8,9)]
colnames(wsj) <- relevant_col 
colnames(nyp) <- relevant_col

df <- rbind(nyt, usa, wapo, wsj, nyp)

# =====================================================================================
# Calculate Cronbach Alpha
# =====================================================================================

cronbach.alpha(df[df$Journal == "New York Times", c(2,3)])
cronbach.alpha(df[df$Journal == "USA Today", c(2,3)])
cronbach.alpha(df[df$Journal == "Washington Post", c(2,3)])
cronbach.alpha(df[df$Journal == "Wall Street Journal", c(2,3)])
cronbach.alpha(df[df$Journal == "New York Post", c(2,3)])


# =====================================================================================
# Proportion of Mismarketing
# =====================================================================================
df |>
  filter(`Categorization for Coder 1` == `Categorization for Coder 2`) |>
  mutate(
    mismarketing = ifelse(`Categorization for Coder 1` == 1, TRUE, FALSE)
  ) |>
  group_by(Journal) |>
  summarize( prop = mean(mismarketing),
             count = sum(mismarketing)) -> prop
