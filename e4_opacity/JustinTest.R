#=================================================================================
# On the interaction between the perceived capabilities and the transparency
# on the effects of responses to Human/Software Liability/Responsibility
#=================================================================================

library(tidyverse)

df <- read_csv("avc_e6.csv")

# Perform Exclusions
df |>
  filter(
    att_1 == 2,
    att_2 == 2,
    comp_1 == 2,
    comp_2 == 4,
    comp_3 == 1
  ) -> df

# Rename column and labels
df$cond <- df$FL_12_DO
df$cond[df$cond == "FL_35"] <- "auto_op"
df$cond[df$cond == "FL_36"] <- "co_op"
df$cond[df$cond == "FL_50"] <- "auto_ft"
df$cond[df$cond == "FL_54"] <- "co_ft"

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
    label = ifelse(grepl("auto", cond), "auto", "co")
    ) -> df

# Get data for different groups
# Elongate dataset for visualization and regression purposes

std_colnames <- c("id", "cond", "transparency", "label", "resp_soft", "resp_human", 
                  "liable_soft", "liable_human", "capability")

auto_op <- df |>
  filter(cond == "auto_op") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}1_[0-9]{1,}"), auto_1)
colnames(auto_op) <- std_colnames

auto_ft <- df |>
  filter(cond == "auto_ft") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}3_[0-9]{1,}"), auto_2)
colnames(auto_ft) <- std_colnames

co_op <- df |>
  filter(cond == "co_op") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}2_[0-9]{1,}"), co_1)
colnames(co_op) <- std_colnames

co_ft <- df |>
  filter(cond == "co_ft") |>
  select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}4_[0-9]{1,}"), co_2)
colnames(co_ft) <- std_colnames

df_comb <- rbind(auto_ft, co_ft, co_op, auto_op)

#=================================================================================
# Individual plots for different dependant vars
#=================================================================================

# Defined capable as a binary variable with the threshold at 4
df_comb |>
  mutate(capable = ifelse(capability < 4, "no", "yes")) -> df_comb

rm(auto_ft, co_ft, co_op, auto_op)

# Plot capability, Human Responsible, transparency and label
hr <- ggplot(df_comb, aes(x=transparency, y=resp_human, color=capable, fill=capable))+
      geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
      ylab("Human Responsible") +
      theme_light()

# Plot capability, Software Responsible, transparency and label
sr <- ggplot(df_comb, aes(x=transparency, y=resp_soft, color=capable, fill=capable))+
  geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
  ylab("Software Responsible") +
  theme_light()
  
# Plot capability, Human Liable, transparency and label
hl <- ggplot(df_comb, aes(x=transparency, y=liable_human, color=capable, fill=capable))+
  geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
  ylab("Human Liable") +
  theme_light()

# Plot capability, Software Liable, transparency and label
sl <- ggplot(df_comb, aes(x=transparency, y=liable_soft, color=capable, fill=capable))+
  geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
  ylab("Software Liable") +
  theme_light()

#=================================================================================
# Gather and facet wrap plots
#=================================================================================

df_comb |>
  gather(key = "DV", value = "Rating", resp_soft, resp_human, liable_soft, liable_human) |>
  mutate(
    DV = case_when( DV == "liable_human" ~ "Human is Liable",
                    DV == "liable_soft" ~ "Software is Liable",
                    DV == "resp_human" ~ "Human is Responsible",
                    DV == "resp_soft" ~ " Software is Responsible",)
  ) |>
  group_by(capable, DV) |>
  mutate(
    mean = mean(Rating),
    std = sd(Rating)
    ) -> df2

sd_width <- 1

plot <- ggplot(df2, aes(x=transparency, y=Rating, color=capable, fill=capable))+
  geom_violin(width = 0.9, alpha = 0.35, size = 0.75) +
  geom_errorbar(aes(ymin=mean-(std*sd_width), ymax=mean+(std*sd_width)), position = "dodge", size=.25) +
  geom_point(aes(y=mean),position=position_dodge(width = .9), size=.75) +
  facet_wrap(vars(DV), nrow = 2) + 
  theme_light()

plot

#===============================================================================
# Regressions
#================================================================================
# Capability as a continuous variable
reg1 <- lm(resp_soft ~ transparency*capability, df_comb)
summary(reg1)

reg2 <- lm(resp_human ~ transparency*capability, df_comb)
summary(reg2)

reg3 <- lm(liable_soft ~ transparency*capability, df_comb)
summary(reg3)

reg4 <- lm(liable_human ~ transparency*capability, df_comb)
summary(reg4)



stargazer(reg1, reg2, reg3, reg4)

# Only transparency dummy
reg1a <- lm(resp_soft ~ transparency, df_comb)
summary(reg1)

reg2a <- lm(resp_human ~ transparency, df_comb)
summary(reg2)

reg3a <- lm(liable_soft ~ transparency, df_comb)
summary(reg3)

reg4a <- lm(liable_human ~ transparency, df_comb)
summary(reg4)

stargazer(reg1a, reg2a, reg3a, reg4a)

# capability dummy interaction
reg1b <- lm(resp_soft ~ transparency*as.factor(capability), df_comb)
summary(reg1b)

reg2b <- lm(resp_human ~ transparency*as.factor(capability), df_comb)
summary(reg2b)

reg3b <- lm(liable_soft ~ transparency*as.factor(capability), df_comb)
summary(reg3b)

reg4b <- lm(liable_human ~ transparency*as.factor(capability), df_comb)
summary(reg4b)

stargazer(reg1b, reg2b, reg3b, reg4b)


#===============================================================================
# ANOVA
#================================================================================
anova_rh <- aov(resp_human ~ capability * as.factor(transparency), data = df_comb)
summary(anova_rh)

anova_lh <- aov(liable_human ~ capability * as.factor(transparency), data = df_comb)
summary(anova_lh)

anova_rs <- aov(resp_soft ~ capability * as.factor(transparency), data = df_comb)
summary(anova_rs)

anova_ls <- aov(liable_soft ~ capability * as.factor(transparency), data = df_comb)
summary(anova_ls)
