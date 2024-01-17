## ================================================================================================================
##                                 Harvard Business School, Ethical Intelligence Lab
## ================================================================================================================
##                                DATA ANALYSIS | AV LABEL STUDY | EXPERIMENT 4b               
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

# Read full dataset
df <- read_csv("data.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

## ================================================================================================================
##                                Exclusions              
## ================================================================================================================
## Failed Attention Checks
df |>
  filter(
    att_1 == 2,
    att_2 == 2) -> df

n_original <- nrow(df)

## Comprehension Checks
df |>
  filter(
    comp_1 == 2,
    comp_2 == 4
  ) -> df

n_excluded <- n_original - nrow(df)

prop_excluded <- n_excluded / n_original

percentage_clicked <- mean(as.numeric(df$clicked))
percentage_clicked

df <- df[df$clicked == 1,]

word_cloud <- file("wordCloud.txt", "wb")
writeBin( paste(df$words_1, collapse="\n"), word_cloud ) 
close(word_cloud)

# Relevant Columns and Elongate Data
rel_col <- c("level_1", "level_2", "hard_1_1", "info_text", "level_3", "gender", "age", "license", "ai_knowledge_1")
df <- df[,rel_col]

df |>
  filter(level_1 == 1) |>
  dplyr::select(level_1, level_2, hard_1_1, gender, age, license, ai_knowledge_1) -> found

colnames(found) <- c("found", "auto_level", "difficulty", "gender", "age", "license", "ai_knowledge")

df |>
  filter(level_1 == 2) |>
  dplyr::select(level_1, level_3, hard_1_1, gender, age, license, ai_knowledge_1) -> not_found

colnames(not_found) <- colnames(found)

df <- rbind(found, not_found)

rm(found, not_found)

df |>
  dplyr::summarize_all(as.numeric) |>
  mutate( found = ifelse(found == 1, TRUE, FALSE ))-> df

## ================================================================================================================
##                                Participant Characteristics               
## ================================================================================================================
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

## ================================================================================================================
##                                      Analysis               
## ================================================================================================================
df$is_correct <- df$auto_level == 2

mean(df$auto_level > 3)

prop_correct <- mean(df$is_correct)
prop_correct

prop_found <- mean(df$found)
prop_found

df$found_label <- ifelse(df$found, "Found", "Not Found")
df$is_correct_label <- ifelse(df$is_correct, "Correct", "Wrong")

# prop correct by if found
tabulate <- table(df$found_label, df$is_correct_label)
tabulate

# t-test for those who found vs not found
found_auto_level <- df[df$found,]$auto_level
notfound_auto_level <- df[!df$found,]$auto_level

sd(found_auto_level)
sd(notfound_auto_level)
t.test(found_auto_level, notfound_auto_level)

# Difficulty == 50? 
difficulty <- df[!is.na(df$difficulty),]$difficulty 

sd(difficulty)
t.test(difficulty, mu = 50)

## ================================================================================================================
##                                VISUALIZATION               
## ================================================================================================================

df |>
  group_by(auto_level, found) |>
  summarize( count = n()) -> df_plot

df_plot <- as.data.frame(df_plot)
df_plot[nrow(df_plot) + 1,] <- c(3, TRUE, 0)
df_plot[nrow(df_plot) + 1,] <- c(1, TRUE, 0)
df_plot[nrow(df_plot) + 1,] <- c(1, FALSE, 0)

df |>
  mutate(high = ifelse(auto_level > 3, T, F)) |>
  group_by(high) |>
  summarize(count = n())

df_plot |>
  mutate( Found = ifelse(found == 1, "TRUE", FALSE),
          `Level of Automation` = auto_level) -> df_plot

ggplot(data = df_plot, aes(x=`Level of Automation`, y = count, fill = Found)) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  scale_fill_grey() +
  scale_color_grey() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10), legend.position = "top",
        axis.title = element_text(face="bold"), text = element_text(face="bold", size=10)) +
  scale_x_discrete(name ="Level of Automation", 
                   limits = 1:6) +
  scale_y_discrete(
    name = "Number of Responses",
    limits = 0:10
  ) -> p

p

ggsave("participants_responses.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")


wc <- read_csv("word_count.csv")
wc |>
  arrange(desc(count)) -> wc

ggplot(wc, aes( x=count, y = reorder(word, (count)))) +
  geom_bar(stat="identity") +
  theme_classic() +
  xlab("Count") +
  ylab("Keywords") + 
  theme(axis.title = element_text(face="bold", size=10), axis.text = element_text(face="bold", size=10))

ggsave("word_count.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
