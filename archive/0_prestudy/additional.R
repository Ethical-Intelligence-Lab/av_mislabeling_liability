# Import libraries required
library(tidyverse)
library(readxl)
library(fuzzyjoin)

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
relevant_col <- c("Journal", "Article Title", "Categorization for Coder 1", "Categorization for Coder 2")

nyt <- nyt[,relevant_col]
usa <- usa[, relevant_col]
wapo <- wapo[, relevant_col]
wsj <- wsj[, c(4,2,7,8)]
nyp <- nyp[, c(5,,8,9)]
colnames(wsj) <- relevant_col 
colnames(nyp) <- relevant_col

df <- rbind(nyt, usa, wapo, wsj, nyp) |>
  distinct()

d <-  read_csv("./data/Tesla CSV Files/combined.csv") |>
  distinct()


df |>
  fuzzy_left_join(d, by = c("Article Title" = "HD")) -> a

setdiff(d$HD, df$`Article Title`)
