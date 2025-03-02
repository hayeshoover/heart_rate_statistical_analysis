# Load necessary libraries
library(tidyverse)
library(lme4)
library(patchwork)
library(ggpubr)
library(knitr)

folder_path <- as.character(here::here()) #>>>THIS SHOULD POINT TO THE "MAIN" FOLDER<<<<#

#Data Import
Masindi <- read_csv(paste0(folder_path, "/Processed Data/Masindi/Masindi_data.csv"))
Masindi_routine <- read_csv(paste0(folder_path, "/Processed Data/Masindi/Masindi_routine_data.csv"))

### Figure

categories <- factor(c("Male", "Female", "Routine", "New Concern"), levels = c("Male", "Female", "Routine", "New Concern"))
means <- c(mean(Masindi$hr[Masindi$sex == "M"]),
           mean(Masindi$hr[Masindi$sex == "F"]),
           mean(Masindi$hr[Masindi$reason == "Routine"]),
           mean(Masindi$hr[Masindi$reason == "New Concern"]))

ci_lower <- c(t.test(Masindi$hr[Masindi$sex == "M"])$conf.int[1],
              t.test(Masindi$hr[Masindi$sex == "F"])$conf.int[1],
              t.test(Masindi$hr[Masindi$reason == "Routine"])$conf.int[1],
              t.test(Masindi$hr[Masindi$reason == "New Concern"])$conf.int[1])

ci_upper <- c(t.test(Masindi$hr[Masindi$sex == "M"])$conf.int[2],
              t.test(Masindi$hr[Masindi$sex == "F"])$conf.int[2],
              t.test(Masindi$hr[Masindi$reason == "Routine"])$conf.int[2],
              t.test(Masindi$hr[Masindi$reason == "New Concern"])$conf.int[2])

plot_data <- data.frame(Category = categories, Mean = means, Lower = ci_lower, Upper = ci_upper)
plot_data$FillColor <- c("gold2", "red", "gold2", "red")  # Assign colors manually
plot_data$wrapping <- factor(c("Sex", "Sex", "Reason", "Reason"), levels = c("Sex", "Reason"))

tiff("FigA.tiff", units="px", width=1900, height=2300, res=400)
ggplot(plot_data, aes(x = factor(Category, levels = unique(Category[wrapping == wrapping])), 
                      y = Mean, fill = FillColor)) +
  geom_bar(stat = "identity", color = "black") +  
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +  
  scale_y_continuous(breaks = seq(70, 91, by = 5)) +
  coord_cartesian(ylim = c(70, 91)) +
  scale_fill_identity() + 
  labs(x = "", y = "Average Heart Rate (bpm)", title = "") +
  facet_grid(cols = vars(wrapping), scales = "free_x", space = "free_x", switch = "x", 
             labeller = labeller(wrapping = c("Reason" = "Reason for Visit"))) +
  theme_pubr() +
  geom_signif(comparisons = list(c("Male", "Female")), 
              annotations = "***", y_position = 90, tip_length = 0.03, 
              data = plot_data[plot_data$wrapping == "Sex", ]) +
  geom_signif(comparisons = list(c("Routine", "New Concern")), 
              annotations = "ns", y_position = 90, tip_length = 0.03, 
              data = plot_data[plot_data$wrapping == "Reason", ]) +
  theme(strip.placement = "outside",
        axis.title.y = element_text(margin = margin(r = 10)))
dev.off()

###



#AGE and SEX
# Visualizing Age and Sex
hist(Masindi$age)

Masindi_Male <- Masindi[Masindi$sex == "M", ]
Masindi_Female <- Masindi[Masindi$sex == "F", ]

tiff("FigB.tiff", units="px", width=2200, height=2000, res=400)
par(mfrow = c(1, 2))
hist(Masindi_Male$age, xlab = "Age (years)", main = "Male", col = "gold2", ylim = c(0, 250))     # NOT normally distributed
hist(Masindi_Female$age, xlab = "Age (years)", main = "Female", col = "red")   # NOT normally distributed
par(mfrow = c(1, 1))
dev.off()

# Used non-parametric Mann-Whitney-Wilcoxon test rather than t-test
wilcox.test(Masindi_Male$age, Masindi_Female$age, conf.int = T)
# p < .01, Reject the null, There is a significant difference between the age of males and females


#HEART RATE and SEX
# Visualizing Heart Rate and Sex
hist(Masindi$hr) # Normally Distributed

tiff("FigC.tiff", units="px", width=2200, height=2000, res=400)
par(mfrow = c(1, 2))
hist(Masindi_Male$hr, xlab = "Heart Rate (bpm)", main = "Male", col = "gold2", breaks = seq(from = 20, to = 160, by = 20), ylim = c(0,300))   # Possibly normally distributed?
hist(Masindi_Female$hr, xlab = "Heart Rate (bpm)", main = "Female", col = "red", breaks = seq(from = 20, to = 160, by = 20)) # Normally distributed
par(mfrow = c(1, 1))
dev.off()

# Confirmation with Shapiro-Wilk test
with(Masindi_Male, shapiro.test(hr))   # p < .01, Reject the Null, NOT normally distributed
with(Masindi_Female, shapiro.test(hr)) # p > .05, Fail to Reject the Null, Normally distributed


wilcox.test(Masindi_Male$hr, Masindi_Female$hr, conf.int = T)
# p < .01, Reject the null, There is a significant difference between the hr of males and females

print(c(mean(Masindi_Female$hr), sd(Masindi_Female$hr)))
print(c(mean(Masindi_Male$hr), sd(Masindi_Male$hr)))
mean(Masindi_Female$hr) - mean(Masindi_Male$hr)


#HEART RATE and REASON for visit

Masindi_newconcern <- Masindi[Masindi$reason == "New Concern", ]

par(mfrow = c(1, 2))
hist(Masindi_routine$hr, xlab = "Heart Rate (bpm)", main = "Routine", col = "purple3")   # Normally distributed
hist(Masindi_newconcern$hr, xlab = "Heart Rate (bpm)", main = "New Concern", col = "gold2") # Normally distributed
par(mfrow = c(1, 1))

# Welch's two sample, two-tailed t-test
t.test(Masindi_routine$hr, Masindi_newconcern$hr, conf.int = T)
# NOT significant


#HEART RATE and AGE

plot1 <- ggplot(Masindi, aes(x = age, y = hr)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatterplot points
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # Regression line
  labs(title = "", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal()

plot1
# Data points appear linear with hr decreasing with age

# Linear regression model
hr_age_model <- lm(hr ~ age, data = Masindi)

summary(hr_age_model) # p < .01, reject null
  
# Check Assumptions
par(mfrow = c(2, 2))
plot(hr_age_model)
par(mfrow = c(1, 1))
# Q-Q plot shows normality of residuals
# Scale-Location shows homoskedasticity 
# Few extreme outliers in Residuals vs. Leverage


#HEART RATE, AGE, and SEX
# Visualizing
p2 <- ggplot(Masindi_Male, aes(x = age, y = hr)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Masindi - Males", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal()

p3 <- ggplot(Masindi_Female, aes(x = age, y = hr)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Masindi - Females", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal()

gridExtra::grid.arrange(p2, p3, nrow = 1, ncol = 2)
# Apparent interaction between sex and age on heart rate
# Age and HR appear unrelated  in males, while HR appears to decrease with age in females

# ANCOVA model (w/ interaction)
additive_model <- aov(hr ~ age + sex, data = Masindi)

interaction_model <- aov(hr ~ age*sex, data = Masindi)
summary(interaction_model) # p < .01 for interaction term

anova(additive_model, interaction_model) # p < .01, interaction term significantly improves model

# Separate linear regression models (hr ~ age) for males and females

#MALE
male_model <- lm(hr ~ age, data = Masindi_Male)
summary(male_model) # p > .05, No significant affect of age on hr in males

# Test linear regression assumptions
par(mfrow = c(2, 2))
plot(male_model)
par(mfrow = c(1, 1))
# Assumptions met: Linearity, normality of residuals, homoscedasticity

#FEMALE
female_model <- lm(hr ~ age, data = Masindi_Female)
summary(female_model) # p < .01, Significant affect of age on hr in females
# In females, HR decreases by 0.20 bpm for each year increase in age

# Test linear regression assumptions
par(mfrow = c(2, 2))
plot(female_model)
par(mfrow = c(1, 1))
# Assumptions met: Linearity, normality of residuals, homoscedasticity




####Repeat analysis EXCLUDING patients presenting with new concerns####

#AGE and SEX
# Visualizing Age and Sex
hist(Masindi_routine$age, xlab = "Age (years)", main = "Routine-Only Masindi")

Masindi_routine_Male <- Masindi_routine[Masindi_routine$sex == "M", ]
Masindi_routine_Female <- Masindi_routine[Masindi_routine$sex == "F", ]

par(mfrow = c(1, 2))
hist(Masindi_routine_Male$age, xlab = "Age (years)", main = "Male", col = "purple3")     # NOT normally distributed
hist(Masindi_routine_Female$age, xlab = "Age (years)", main = "Female", col = "gold2")   # NOT normally distributed
par(mfrow = c(1, 1))

# Used non-parametric Mann-Whitney-Wilcoxon test rather than t-test
wilcox.test(Masindi_routine_Male$age, Masindi_routine_Female$age, conf.int = T)
# p < .01, Reject the null, There is a significant difference between the age of males and females


#HEART RATE and SEX
# Visualizing Heart Rate and Sex
hist(Masindi_routine$hr, xlab = "Heart Rate (bpm)", main = "Routine-Only Masindi") # Normally Distributed

par(mfrow = c(1, 2))
hist(Masindi_routine_Male$hr, xlab = "Heart Rate (bpm)", main = "Male", col = "purple3")   # Not normally distributed
hist(Masindi_routine_Female$hr, xlab = "Heart Rate (bpm)", main = "Female", col = "gold2") # Possibly normally distributed?
par(mfrow = c(1, 1))

# Use non-parametric alternative to t-test
wilcox.test(Masindi_routine_Male$hr, Masindi_routine_Female$hr, conf.int = T)
# p < .01, Reject the null, There is a significant difference between the hr of males and females

print(c(mean(Masindi_routine_Female$hr), sd(Masindi_routine_Female$hr)))
print(c(mean(Masindi_routine_Male$hr), sd(Masindi_routine_Male$hr)))
mean(Masindi_routine_Female$hr) - mean(Masindi_routine_Male$hr)



#HEART RATE and AGE

plot1 <- ggplot(Masindi_routine, aes(x = age, y = hr)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatterplot points
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # Regression line
  labs(title = "", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal()

plot1
# Data points appear linear with hr decreasing with age

# Linear regression model
hr_age_routine_model <- lm(hr ~ age, data = Masindi_routine)

summary(hr_age_routine_model) # p < .01, reject null

# Check Assumptions
par(mfrow = c(2, 2))
plot(hr_age_routine_model)
par(mfrow = c(1, 1))
# Q-Q plot shows normality of residuals
# Scale-Location shows homoskedasticity 
# Few extreme outliers in Residuals vs. Leverage


#HEART RATE, AGE, and SEX
# Visualizing
p2 <- ggplot(Masindi_routine_Male, aes(x = age, y = hr)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Masindi (Routine) - Males", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal()

p3 <- ggplot(Masindi_routine_Female, aes(x = age, y = hr)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Masindi (Routine) - Females", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal()

gridExtra::grid.arrange(p2, p3, nrow = 1, ncol = 2)
# Apparent interaction between sex and age on heart rate
# HR appears to slightly increase w/ age in males, decrease with age in females

# ANCOVA model (w/ interaction)
additive_routine_model <- aov(hr ~ age + sex, data = Masindi_routine)

interaction_routine_model <- aov(hr ~ age*sex, data = Masindi_routine)
summary(interaction_routine_model) # p < .01 for interaction term

anova(additive_routine_model, interaction_routine_model) # p < .01, interaction term significantly improves model

# Separate linear regression models (hr ~ age) for males and females

#MALE
male_routine_model <- lm(hr ~ age, data = Masindi_routine_Male)
summary(male_routine_model) # p > .05, No significant affect of age on hr in males

# Test linear regression assumptions
par(mfrow = c(2, 2))
plot(male_routine_model)
par(mfrow = c(1, 1))
# Assumptions met: Linearity, normality of residuals, homoscedasticity

#FEMALE
female_routine_model <- lm(hr ~ age, data = Masindi_routine_Female)
summary(female_routine_model) # p < .01, Significant affect of age on hr in females
# In females, HR decreases by 0.22 bpm for each year increase in age

# Test linear regression assumptions
par(mfrow = c(2, 2))
plot(female_routine_model)
par(mfrow = c(1, 1))
# Assumptions met: Linearity, normality of residuals, homoscedasticity


