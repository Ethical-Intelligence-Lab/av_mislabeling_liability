## clear workspace
rm(list = ls()) 

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggsignif',
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
               "sjstats",
               'lavaan',
               'semTools'
)

mediation <- FALSE
if(mediation) source('../process.r')

## ================================================================================================================
##                                Exclusions and Pre-processing               
## ================================================================================================================

# Read full dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d_raw <- read.csv("data.csv")
# Remove first two rows that were headers
d_raw <- d_raw[-c(1,2),]

# Convert all numeric columns to numeric
num_cols <- sapply(d_raw, is.numeric)
d_raw[num_cols] <- lapply(d_raw[num_cols], as.numeric)

# Filter rows where Finished == 1
d_raw <- d_raw[d_raw$Finished == 1, ]

# ATTENTION CHECKS
d_raw <- d_raw[d_raw$att_1==2 & d_raw$att_2 == 2, ]
n_recruited <- nrow(d_raw); n_recruited

# COMPREHENSION CHECKS 1 and 2
d_raw <- d_raw[d_raw$comp_1 == 2 & d_raw$comp_2 == 4, ]

n_ss <- dim(d_raw)[1]; n_ss

## -- Pre-process  

# Define new data frame that we'll extract preprocessed data into
d_subset <- array(dim=c(n_ss, 7))
colnames(d_subset) <- c('cond', 'capability','control', 'handsOff', 'watch', 'nap', 'comp3')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

# Extract the good data from the middle part of the raw data
for(i in 1:n_ss) {
  cond_ns <- names(d_raw[i,24:35])[which(d_raw[i,24:35] != "")]
  d_subset[i,1] <- strsplit(cond_ns[[1]], "_")[[1]][1]
  
  curr <- d_raw[i,24:35][!is.na(d_raw[i,24:35])] #for a given row, get only the non NA values
  d_subset[i,2:7] <- as.numeric(curr[curr!= ""]) #and only the non-empty values
}

# Merge good data with first and last halves of the original data
d <- cbind(d_raw[,20:24], d_subset, d_raw[,36:56])
d$ss <- 1:dim(d)[1]

# Replace NA in TimeVehicleStopped with 19.36
d$TimeVehicleStopped <- ifelse(is.na(d$TimeVehicleStopped), 19.36, d$TimeVehicleStopped)


# COMPREHENSION CHECKS 3
d <- d[d$comp3 == 1, ]
n_final <- dim(d)[1]; n_final

#risk aversion
risk_cols <- c("unsafe_self_10", "worried_self_10", "unsafe_others_10", "worried_others_10",
          "likely_others_10", "likely_self_10", "concern_others_10", "concern_self_10")

d[risk_cols] <- lapply(d[risk_cols], function(x) as.numeric(as.character(x)))
d$risk_aversion <- rowMeans(d[risk_cols], na.rm = TRUE)

intention_cols <- c("control", "handsOff", "watch", "nap")

d[intention_cols] <- lapply(d[intention_cols], function(x) as.numeric(as.character(x)))
d$intentions <- rowMeans(d[intention_cols], na.rm = TRUE)

# FOR PROCESS
d_process <- d
d_process$cond <- as.numeric(as.factor(d_process$cond))

## Number Excluded
n_recruited - n_final

## ================================================================================================================
##                                Participants Characteristics               
## ================================================================================================================

mean(as.numeric(d$age), na.rm=T) # filtering the ones who put year
prop_male <- prop.table(table(d$gender))[[1]]; prop_male
table(d$license)[1]/sum(table(d$license))

## ================================================================================================================
##                                Analysis               
## ================================================================================================================
cronbach.alpha(d[, c("control", "handsOff", "watch", "nap")])
cronbach.alpha(d[, c("unsafe_self_10", "worried_self_10", "unsafe_others_10", "worried_others_10", 
                     "likely_others_10", "likely_self_10", "concern_others_10", "concern_self_10")])

# Perceived Automation Level
t0 <- t.test(d[d$cond == "auto",]$capability, 
             d[d$cond == "co",]$capability)
t0

sd(d[d$cond == "auto",]$capability)
sd(d[d$cond == "co",]$capability)

cohen.d(d[d$cond == "auto",]$capability, d[d$cond == "co",]$capability)

# Distracted Intentions
t1 <- t.test(d[d$cond == "auto",]$intentions, d[d$cond == "co",]$intentions, paired = FALSE)
t1

sd(d[d$cond == "auto",]$intentions)
sd(d[d$cond == "co",]$intentions)

cohen.d(d[d$cond == "auto",]$intentions, d[d$cond == "co",]$intentions)

#control for risk aversion and av knowledge
intention_mod <- lm(intentions ~ cond + risk_aversion + ai_knowledge_1, data = d)
summary(intention_mod)

## Simple Mediation
if(mediation) process(data = d_process, y = "behavior", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Moderated Mediation (Risk Aversion) 
if(mediation) process(data = d_process, y = "behavior", x = "label", w = c("risk_aversion"),
        m =c("capability"), model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Moderated Mediation (AI Knowledge) 
if(mediation) process(data = d_process, y = "behavior", x = "label", w = "ai_knowledge",
        m =c("capability"), model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)


# Time to Take Control
d$TimeVehicleStopped <- as.numeric(d$TimeVehicleStopped)
t2 <- t.test(d[d$cond == "auto",]$TimeVehicleStopped, d[d$cond == "co",]$TimeVehicleStopped, paired = FALSE)
t2

sd(d[d$cond == "auto",]$TimeVehicleStopped)
sd(d[d$cond == "co",]$TimeVehicleStopped)

cohen.d(d[d$cond == "auto",]$TimeVehicleStopped, d[d$cond == "co",]$TimeVehicleStopped)

#control for risk aversion and av knowledge
rt_mod <- lm(TimeVehicleStopped ~ cond + risk_aversion + ai_knowledge_1, data = d)
summary(rt_mod)


## Simple Mediation
if(mediation) process(data = d_process, y = "time_control", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Moderated Mediation (Risk Aversion) 
if(mediation) process(data = d_process, y = "time_control", x = "label", w = c("risk_aversion"),
        m =c("capability"), model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Moderated Mediation (AI Knowledge) 
if(mediation) process(data = d_process, y = "time_control", x = "label", w = "ai_knowledge",
        m =c("capability"), model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Distribution of Time to Take Control
## Kolmogorov-Smirnov 
ks.test(d[d$label == "auto",]$time_control, d[d$label == "co",]$time_control)

## ================================================================================================================
##                                VISUALIZATION               
## ================================================================================================================
d |>
  mutate(
    Label = ifelse( label == "co", "Copilot", "Autopilot"),
    `Time to Take Control (s)` = time_control
  ) -> d_plot

ggplot(d_plot, aes(x = Label, y = `Time to Take Control (s)`)) +
  stat_summary(fun = mean, geom = "bar",, alpha = 0.5) +  # Bar plot
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) +   # Error bars
  geom_jitter(width = 0.2, alpha = 0.15, size = .1, color = "#00003B") +  # Individual data points
  scale_fill_grey() +
  scale_color_grey() +
  theme_classic() + 
  geom_signif(comparisons = list(c("Copilot", "Autopilot")), map_signif_level = TRUE, , test = "t.test") +
  theme(text = element_text(face = "bold"))-> p1

p1

ggplot(data = d_plot, aes(color =`Label`, x=time_control )) +
  stat_density(geom="line", position="identity", alpha=.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=10), legend.position = 'top' ) + 
  scale_color_grey() +
  ylab("Density") +
  xlab("Time to Take Control (s)") + 
  ggplot2::annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax = .2,alpha = .1) +
  ggplot2::annotate("text", x = 7.5, y = .08, label = "Vehicle approaches intersection", size = 2) +
  ggplot2::annotate("rect", xmin = 15, xmax = 20, ymin = 0, ymax = .2,alpha = .1) +
  ggplot2::annotate("text", x = 17.5, y = .19, label = "Vehicle approaches jaywalkers", size = 2) +
  theme(legend.key = element_rect(fill = NA), text = element_text(face = "bold")) -> p2

p2

# Create "plots" folder if it doesn't already exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

ggarrange(p1 , p2)

ggsave("plots/TimeToTakeControl.jpg", device = "jpg",width = 8.3, height = 3.7, units = "in")

## ================================================================================================================
##                                VISUALIZATION (OLD)           
## ================================================================================================================

std.error <- function(x) sd(x)/sqrt(length(x))

d |>
  gather(key = "DV", value = "Value", 
         time_control, behavior) |>
  mutate(
    DV = ifelse( DV == "behavior", "Distraction Intention", "Time to Take Control"),
    `Marketing Label` = case_when(
      label == "auto" ~ "Autopilot",
      label == "co" ~ "Copilot"
    )
  ) |>
  group_by(`Marketing Label`, DV) |>
  summarise( 
    mean = mean(Value),
    se = std.error(Value) 
  ) -> d_plot


plot_did <- function(df=d_plot, dv, signif=c("*","*","*"), yaxis=TRUE, ypos=c(40)) {
  
  d_plot <- df |>
    filter(DV == dv)
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=`Marketing Label`, y=mean)) +
    geom_bar(stat="summary", position="dodge", alpha=.75) +
    geom_errorbar(aes(ymin=mean-(se*se_width), ymax=mean+(se*se_width)), position = position_dodge(width=.9), 
                  size=.25, color="black", width=.5) +
    geom_point(aes(y=mean),position=position_dodge(width = .9), size=.5, color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold", size=10)
    ) +
    geom_signif(
      y_position = ypos, xmin = c(1.0), xmax = c(2.0),
      annotation = signif, tip_length = 0.1, color='black', size = .25, textsize = 3.5 
    ) +
    scale_fill_grey() +
    scale_color_grey() +
    ggtitle(dv) +
    xlab("Marketing Label") +
    ylab("Response") -> p
  
  if(!yaxis) {
    p <- p +
      theme( axis.line.y = element_line(color = "white"),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())
  }
  
  return(p)
}

plot_did(dv = "Distraction Intention", signif = c("*"), yaxis=T) -> p1
p1 + theme(text = element_text(face = "bold")) 

ggsave("behavioral.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

plot_did(dv = "Time to Take Control", signif = c("**"), yaxis=T, ypos = 17)  +
  ylab("Response Time (s)") -> p2
p2 + theme(text = element_text(face = "bold"))

ggsave("control_time.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

ggarrange(p1 + ylab("Mean Rating") + rremove("xlab") + theme(text = element_text(face = "bold")),
          p2+ rremove("xlab") + theme(text = element_text(face = "bold")),
          ncol = 2, common.legend = TRUE)  |>
  annotate_figure(bottom = textGrob("Marketing Label", gp = gpar(cex = .8, fontface = "bold")))

ggsave("behavior_time.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

d  |>
  mutate(
    `Marketing Label` = case_when(
      label == "auto" ~ "Autopilot",
      label == "co" ~ "Copilot"
    )
  ) -> d_density



ggsave("time_density.pdf", device = "pdf",width = 5.3, height = 3.7, units = "in")

##==================================================
#                     Covariate Check
##==================================================

# DV: capability
summary(lm(capability ~ label + age, d))

summary(lm(capability ~ label + gender, d))

summary(lm(capability ~ label + ai_knowledge, d))

# DV: behavior
summary(lm(behavior ~ label + age, d))

summary(lm(behavior ~ label + ai_knowledge, d))

summary(lm(behavior ~ label + gender, d))

# DV: time_control
summary(lm(time_control ~ label + age, d))

summary(lm(time_control ~ label + ai_knowledge, d))

summary(lm(time_control ~ label + gender, d))
