## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV LABEL STUDY | EXPERIMENT 6              
## ================================================================================================================

## clear workspace
rm(list = ls()) 

# options(download.file.method="libcurl")

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('tidyverse',       # most stuff
               'ggsignif',
               'grid', # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # Cronbach Alpha
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
               'effects',
               "Hmisc", 
               "sjstats"
)

## ================================================================================================================
##                                Pre-Processing              
## ================================================================================================================

# Read full dataset
df <- read_csv("data.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

df |>
  mutate_if(all.is.numeric, as.numeric) -> df

## ================================================================================================================
##                                ANALYSIS             
## ================================================================================================================


# Consider the labels
sum(!is.na(df$risk_1))/length(df$risk_1)
df$`Consider Label` <- ifelse(df$risk_1 == 1, "Yes", "No")
table(df$`Consider Label`)
prop.table(table(df$`Consider Label`))

chisq.test(table(df$`Consider Label`), p = c(.5,.5))

# Adjust Risk Estimates
sum(!is.na(df$adjust_4))/length(df$adjust_4)
t.test(df$adjust_4, mu = 50)
sd(df$adjust_4, na.rm = T)

# Increase/Decrease Risk Estimates
sum(!is.na(df$risk_4))/length(df$risk_4)
t.test(df$risk_4, mu = 50)
sd(df$risk_4, na.rm = T)

# Premiums
sum(!is.na(df$premiums_4))/length(df$premiums_4)
#t.test(df$premiums_4, mu = 50)
#sd(df$premiums_4, na.rm = T)

# Advise
sum(!is.na(df$advise_4))/length(df$advise_4)
#t.test(df$advise_4, mu = 50)
#sd(df$advise_4, na.rm = T)

# Reasoning
# minus 2 for n/a and - in response
(sum(!is.na(df$reasoning)) - 2)/length(df$reasoning)

## ================================================================================================================
##                                CODED DATA              
## ================================================================================================================

coded <- read_csv('coded_responses.csv')

coded <- coded[!is.na(coded$code),]

c <- paste(coded$code, collapse = ",")

c <- strsplit(c, ",")

table(c)