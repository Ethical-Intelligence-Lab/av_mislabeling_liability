## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV RESPONSIBILITY STUDY | EXPERIMENT 6              
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

#==============================================================
# PRE-PROCESSING
#==============================================================

# Read full dataset
df <- read_csv("data.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

df |>
  mutate_if(all.is.numeric, as.numeric) -> df

#==================================================================
# Analysis
#==================================================================

# Consider the labels
df$`Consider Label` <- ifelse(df$risk_1 == 1, "Yes", "No")
table(df$`Consider Label`)
prop.table(table(df$`Consider Label`))

# Adjust Risk Estimates
t.test(df$adjust_4, mu = 50)
sd(df$adjust_4, na.rm = T)

# Increase/Decrease Risk Estimates
t.test(df$risk_4, mu = 50)
sd(df$risk_4, na.rm = T)

# Premiums
t.test(df$premiums_4, mu = 50)
sd(df$premiums_4, na.rm = T)

# Advise
t.test(df$advise_4, mu = 50)
sd(df$advise_4, na.rm = T)

#==================================================================
# Visualization
#==================================================================

df |>
  mutate( `Consider Label` = ifelse(risk_1 == "Yes", "Yes", "No") ) |>
  group_by(`Consider Label`) |>
  summarise(count = n()) |>
  drop_na() -> df_plot

ggplot(data = df_plot, aes(x=`Consider Label`, y=count)) +
  geom_bar(stat="identity", position="dodge", alpha = 1) +
  scale_fill_grey() +
  scale_color_grey() +
  theme_classic() +
  xlab("Consider Marketing Label") +
  ylab("Number of Response")

t.test(df$adjust_4, mu = 50)
sd(df$adjust_4, na.rm = T)

t.test(df$risk_4, mu = 50)

sd(df$risk_4, na.rm = T)

ggplot(data = df, aes(x=adjust_4)) +
  geom_histogram(binwidth = 10, alpha = 1) +
  scale_fill_grey() +
  scale_color_grey() +
  theme_classic() +
  xlab("Ratings") +
  ylab("Number of Response") +
  ggtitle("Reponses on Adjusting Risk Estimates")

ggplot(data = df, aes(x=risk_4)) +
  geom_histogram(binwidth = 10, alpha = 1) +
  scale_fill_grey() +
  scale_color_grey() +
  theme_classic() +
  xlab("Ratings") +
  ylab("Number of Response") +
  ggtitle("Reponses on Changing Risk Estimates")



t.test(df$risk_4, mu = 50)
