# =============================================================================
# BEHAVIORAL SIMULATION ANALYSIS - R VERSION
# =============================================================================

## clear workspace
rm(list = ls()) 

# install packages
if (!require(pacman)) {install.packages("pacman")}

pacman::p_load('tidyverse',
               'broom',
               'car',
               'psych',
               'ggplot',
               'ggpubr',
               'sjstats')

file <- "AV_Culpability_E18_Behavior_MultiTasking_Notifs_Stage1_Full.csv"
df <- read_csv(file)

# -----------------------------------------------------------------------------
# EXCLUSIONS & CREATING NEW COLUMNS
# -----------------------------------------------------------------------------

# First attention check
df <- df %>% filter(att_1 == "Purple")
n_recruited <- dim(df)[1]; n_recruited

# Initial comprehension checks
df <- df %>% filter(
  comp_1 == "Different types of vehicles based on how many driving tasks are controlled by humans vs. the self-driving system" &
    comp_2 == "HUMAN, HUMAN, HUMAN, HUMAN"
)

# Exclude non-mobile users
df <- df %>% filter(device_type == "Not Mobile")

# Remove rows where any DV is NA
df <- df %>% filter(
  !is.na(adas_choice) &
    !is.na(relaxedness_1) &
    !is.na(anxiety_1) &
    !is.na(safety_1)
)

# Create treatment indicator: notifs_shown
df <- df %>% mutate(notifs_shown = as.integer(is.na(adas_choice_plain)))

# Simulation comprehension checks
df <- df %>% filter(
  (notifs_shown == 1 & comp_notif == "Time elapsed & hitting white obstacles") |
    (notifs_shown == 0 & comp_plain == "Time elapsed & hitting white obstacles")
)

# Create choice dependent variable
df <- df %>% mutate(autopilot_chosen = adas_choice == "<strong>AEON Autopilot</strong>")

# Convert to factors for modeling
df <- df %>% mutate(
  notifs_shown = factor(notifs_shown, levels = c(0, 1)),
  autopilot_chosen = factor(autopilot_chosen, levels = c(FALSE, TRUE))
)

# Glance at the data
print(head(df))
print(dim(df))

n_final <- dim(df)[1]
n_excluded <- n_recruited - n_final; n_excluded
n_excluded/n_recruited

# =============================================================================
# DEMOGRAPHICS
# =============================================================================

mean(as.numeric(df$age), na.rm=T) # filtering the ones who put year
prop_male <- prop.table(table(df$gender))[[1]]; prop_male
table(df$license)[2]/sum(table(df$license))

# =============================================================================
# ANALYSES
# =============================================================================

## Capability
t.test(df$auto_1, df$co_1, paired=TRUE)
mean(df$auto_1)
sd(df$auto_1)

mean(df$co_1)
sd(df$co_1)

d <- df$auto_1 - df$co_1
cohens_d <- mean(d, na.rm = TRUE) / sd(d, na.rm = TRUE)
cohens_d

# -----------------------------------------------------------------------------
# BINOMIAL TEST FOR POOLED ADAS CHOICE
# -----------------------------------------------------------------------------
cat("\n=== BINOMIAL TEST ===\n")
n_autopilot <- sum(df$autopilot_chosen == TRUE)
n_total <- nrow(df)
binom_result <- binom.test(n_autopilot, n_total, p = 0.5)
print(binom_result)

# Proportions
cat("\nADAS Choice Proportions:\n")
print(prop.table(table(df$autopilot_chosen)))

# -----------------------------------------------------------------------------
# VISUALIZE ADAS CHOICE DISTRIBUTION
# -----------------------------------------------------------------------------
adas_props <- df %>%
  count(autopilot_chosen) %>%
  mutate(prop = n / sum(n))

ggplot(adas_props, aes(x = autopilot_chosen, y = prop, fill = autopilot_chosen)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), vjust = -0.5) +
  #scale_fill_manual(values = c("#4C72B0", "#DD8452")) +
  labs(
    title = "Distribution of ADAS Choices",
    x = "Chose Autopilot",
    y = "Proportion"
  ) +
  ylim(0, 1) +
  theme_classic() + 
  scale_fill_grey() +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# EFFECT OF NOTIFICATION ON ADAS CHOICE - CROSSTAB
# -----------------------------------------------------------------------------
cat("\n=== CROSSTAB: ADAS CHOICE BY NOTIFICATION ===\n")

# Raw counts
print(table(df$notifs_shown, df$autopilot_chosen))

# Proportions within each notification condition
ct <- df %>%
  group_by(notifs_shown, autopilot_chosen) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(notifs_shown) %>%
  mutate(prop = n / sum(n))
print(ct)

# Visualize crosstab
p1 <- ggplot(ct, aes(x = notifs_shown, y = prop, fill = autopilot_chosen)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
   scale_fill_manual(values = c("gray20", "gray80"), labels = c("Copilot", "Autopilot")) +
   scale_x_discrete(labels = c("Absent", "Present")) +
   labs(
        x = "Notification Condition",
        y = "Proportion Chosen",
        fill = "Label Chosen"
   ) +
   ylim(0, 1) +
   theme_classic() 

print(p1)

# -----------------------------------------------------------------------------
# LOGISTIC REGRESSION - ADAS CHOICE ~ NOTIFICATION
# -----------------------------------------------------------------------------
cat("\n=== LOGISTIC REGRESSION: AUTOPILOT CHOSEN ~ NOTIFS SHOWN ===\n")

logit_model <- glm(autopilot_chosen ~ notifs_shown, 
                   data = df, 
                   family = binomial(link = "logit"))
print(summary(logit_model))

# Odds ratio
cat("\nOdds Ratios:\n")
print(exp(coef(logit_model)))

# Confidence intervals for odds ratios
cat("\n95% CI for Odds Ratios:\n")
print(exp(confint(logit_model)))

# -----------------------------------------------------------------------------
# EFFECT OF ADAS CHOICE & NOTIFICATION ON COMFORT - CELL MEANS
# -----------------------------------------------------------------------------
cat("\n=== CELL MEANS FOR COMFORT DVs ===\n")


dvs <- c("relaxedness_1", "safety_1", "anxiety_1")

for (dv in dvs) {
  cat(paste0("\n", dv, ":\n"))
  means <- df %>%
    group_by(notifs_shown, autopilot_chosen) %>%
    summarise(mean = mean(.data[[dv]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = autopilot_chosen, values_from = mean)
  print(means)
}

# Visualize each DV
for (dv in dvs) {
  p <- ggplot(df, aes(x = notifs_shown, y = .data[[dv]], fill = autopilot_chosen)) +
    stat_summary(fun = mean, geom = "bar", position = "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", 
                 position = position_dodge(0.9), width = 0.2) +
    scale_fill_manual(values = c("#4C72B0", "#DD8452"), labels = c("Copilot", "Autopilot")) +
    scale_x_discrete(labels = c("No Notifications", "Notifications Shown")) +
    labs(
      title = paste("Mean", dv, "by Condition"),
      x = "Notification Condition",
      y = paste("Mean", dv),
      fill = "Chose Autopilot"
    ) +
    theme_classic() + 
    scale_fill_grey() 
  print(p)
}

# -----------------------------------------------------------------------------
# OLS MODELS - 2x2 ANOVA FOR EACH COMFORT DV
# -----------------------------------------------------------------------------
cat("\n=== OLS MODELS (2x2 ANOVA) ===\n")

# Relaxedness model
cat("\n--- Relaxedness Model ---\n")
model_relaxed <- lm(relaxedness_1 ~ notifs_shown * autopilot_chosen, data = df)
print(summary(model_relaxed))
cat("\nType II ANOVA:\n")
print(Anova(model_relaxed, type = 2))

# Safety model
cat("\n--- Safety Model ---\n")
model_safety <- lm(safety_1 ~ notifs_shown * autopilot_chosen, data = df)
print(summary(model_safety))
cat("\nType II ANOVA:\n")
print(Anova(model_safety, type = 2))

# Anxiety model
cat("\n--- Anxiety Model ---\n")
model_anxiety <- lm(anxiety_1 ~ notifs_shown * autopilot_chosen, data = df)
print(summary(model_anxiety))
cat("\nType II ANOVA:\n")
print(Anova(model_anxiety, type = 2))

# -----------------------------------------------------------------------------
# COMFORT COMPOSITE - CRONBACH'S ALPHA
# -----------------------------------------------------------------------------
cat("\n=== COMFORT COMPOSITE ===\n")

# Reverse-code anxiety
df <- df %>% mutate(
  anxiety_1_rev = max(anxiety_1, na.rm = TRUE) + min(anxiety_1, na.rm = TRUE) - anxiety_1
)

# Calculate Cronbach's alpha
alpha_items <- df %>% select(relaxedness_1, safety_1, anxiety_1_rev)
alpha_result <- psych::alpha(alpha_items)
cat("\nCronbach's Alpha for comfort composite:\n")
print(alpha_result$total$raw_alpha)

# Create comfort composite (mean of relaxedness, safety, reverse-coded anxiety)
df <- df %>% mutate(
  comfort = (relaxedness_1 + safety_1 + anxiety_1_rev) / 3
)

# -----------------------------------------------------------------------------
# OLS MODEL FOR COMFORT COMPOSITE
# -----------------------------------------------------------------------------
cat("\n=== COMFORT COMPOSITE MODEL ===\n")
model_comfort <- aov(comfort ~ notifs_shown * autopilot_chosen, data = df)
print(summary(model_comfort))
cat("\nType II ANOVA:\n")
print(Anova(model_comfort, type = 2))
anova_stats(model_comfort); anova_stats(model_comfort)$partial.etasq

mean(df$comfort[df$notifs_shown==1])
sd(df$comfort[df$notifs_shown==1])
mean(df$comfort[df$notifs_shown==0])
sd(df$comfort[df$notifs_shown==0])

mean(df$comfort[df$autopilot_chosen==TRUE])
sd(df$comfort[df$autopilot_chosen==TRUE])
mean(df$comfort[df$autopilot_chosen==FALSE])
sd(df$comfort[df$autopilot_chosen==FALSE])

#replication controlling for automated familiarity
model_comfort_cont <- lm(comfort ~ notifs_shown * autopilot_chosen + ai_knowledge_1, data = df)
summary(model_comfort_cont)

# Visualize comfort composite
p2 <- ggplot(df, aes(x = notifs_shown, y = comfort, fill = autopilot_chosen)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = c("gray20", "gray80"), labels = c("Copilot", "Autopilot")) +
  scale_x_discrete(labels = c("Absent", "Present")) +
  ylim(0,100)+
  labs(
    x = "Notification Condition",
    y = "Comfort Level Ratings",
    fill = "Label Chosen"
  ) +
  theme_classic() 

print(p2)

# Create "plots" folder if it doesn't already exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

ggarrange(p1 , p2, common.legend = TRUE,
          legend = "top")

ggsave("plots/multiTasking_choice.jpg", device = "jpg",width = 8.3, height = 3.7, units = "in")

# -----------------------------------------------------------------------------
# SUMMARY TABLE
# -----------------------------------------------------------------------------
cat("\n=== SUMMARY OF KEY FINDINGS ===\n")

# Extract key statistics
summary_df <- tibble(
  Model = c("Relaxedness", "Safety", "Anxiety", "Comfort"),
  `Notifs Effect (b)` = c(
    coef(model_relaxed)["notifs_shown1"],
    coef(model_safety)["notifs_shown1"],
    coef(model_anxiety)["notifs_shown1"],
    coef(model_comfort)["notifs_shown1"]
  ),
  `Notifs p-value` = c(
    summary(model_relaxed)$coefficients["notifs_shown1", "Pr(>|t|)"],
    summary(model_safety)$coefficients["notifs_shown1", "Pr(>|t|)"],
    summary(model_anxiety)$coefficients["notifs_shown1", "Pr(>|t|)"],
    summary(model_comfort)$coefficients["notifs_shown1", "Pr(>|t|)"]
  ),
  `Autopilot Effect (b)` = c(
    coef(model_relaxed)["autopilot_chosenTRUE"],
    coef(model_safety)["autopilot_chosenTRUE"],
    coef(model_anxiety)["autopilot_chosenTRUE"],
    coef(model_comfort)["autopilot_chosenTRUE"]
  ),
  `Autopilot p-value` = c(
    summary(model_relaxed)$coefficients["autopilot_chosenTRUE", "Pr(>|t|)"],
    summary(model_safety)$coefficients["autopilot_chosenTRUE", "Pr(>|t|)"],
    summary(model_anxiety)$coefficients["autopilot_chosenTRUE", "Pr(>|t|)"],
    summary(model_comfort)$coefficients["autopilot_chosenTRUE", "Pr(>|t|)"]
  ),
  `Interaction p-value` = c(
    summary(model_relaxed)$coefficients["notifs_shown1:autopilot_chosenTRUE", "Pr(>|t|)"],
    summary(model_safety)$coefficients["notifs_shown1:autopilot_chosenTRUE", "Pr(>|t|)"],
    summary(model_anxiety)$coefficients["notifs_shown1:autopilot_chosenTRUE", "Pr(>|t|)"],
    summary(model_comfort)$coefficients["notifs_shown1:autopilot_chosenTRUE", "Pr(>|t|)"]
  )
)

print(summary_df)

