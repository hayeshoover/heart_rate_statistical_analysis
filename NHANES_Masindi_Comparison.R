library(tidyverse)
library(car)
library(emmeans)
library(boot)
library(ggpubr)
library(patchwork)

folder_path <- as.character(here::here()) #>>>THIS SHOULD POINT TO THE "MAIN" FOLDER<<<<#

#Data Import
NHANES <- read_csv(paste0(folder_path, "/Processed Data/NHANES/HR_DEMO_NHANES.csv"))
Masindi <- read_csv(paste0(folder_path, '/Processed Data/Masindi/Masindi_data.csv'))
Masindi_routineonly <- read_csv(paste0(folder_path, '/Processed Data/Masindi//Masindi_routine_data.csv'))

####Does the demographic structure (age and sex) of Masindi study participants differ from that of the NHANES study?####

#Does Age differ? - Mann-Whitney-Wilcoxon test (non-parametric t-test)

Masindi <- Masindi %>%
  filter(age <= 85) #  To match NHANES age range

hist(NHANES$age, main = "Age Distribution in NHANES", xlab = "Age", col = "blue") #Non-normal
hist(Masindi$age, main = "Age Distribution in Masindi", xlab = "Age", col = "red") #Non-normal

wilcox.test(NHANES$age, Masindi$age) # p < .01, reject null, significant difference in ages


#Does sex differ? - Chi-square test for independence

table_sex <- table( # Contingency table
  factor(c(NHANES$sex, Masindi$sex), levels = c("M", "F")), 
  factor(c(rep("NHANES", length(NHANES$sex)), rep("Masindi", length(Masindi$sex))), levels = c("NHANES", "Masindi"))
)

chi_square_test <- chisq.test(table_sex)

print(chi_square_test) # p < .01, reject null, significant difference in sex ratios


#Representation with population pyramid
Masindi_pyr <- Masindi %>%
  filter(age <= 85) %>%
  mutate(age_category = cut(age, breaks = c(17, 29, 39, 49, 59, 69, 85), labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-85")))

NHANES_pyr <- NHANES %>%
  mutate(age_category = cut(age, breaks = c(17, 29, 39, 49, 59, 69, 85), labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-85")))

Masindi_summary_df <- Masindi_pyr %>%
  filter(!is.na(age_category)) %>%  
  group_by(sex, age_category) %>%
  summarise(count = n()) %>%
  spread(key = sex, value = count, fill = 0)

NHANES_summary_df <- NHANES_pyr %>%
  filter(!is.na(age_category)) %>%  
  group_by(sex, age_category) %>%
  summarise(count = n()) %>%
  spread(key = sex, value = count, fill = 0)

pop_pyramid_masindi <- ggplot(data = Masindi_summary_df, aes(x = age_category)) +
  geom_bar(aes(y = M, fill = "Male"), stat = "identity", position = "identity", width = 1) +
  geom_bar(aes(y = -`F`, fill = "Female"), stat = "identity", position = "identity", width = 1) +
  scale_fill_manual(values = c("Male" = "gold2", "Female" = "red"), name = "Gender") +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = 0.05, add = 0), labels = abs, limits = c(-max(abs(Masindi_summary_df$M), abs(Masindi_summary_df$`F`)), max(abs(Masindi_summary_df$M), abs(Masindi_summary_df$`F`)))) +
  labs(title = "Masindi Study Cohort", y = "Count", x = "Age Category") +
  theme_pubr() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x.bottom = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = c(.81, .8),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


pop_pyramid_NHANES <- ggplot(data = NHANES_summary_df, aes(x = age_category)) +
  geom_bar(aes(y = M, fill = "Male"), stat = "identity", position = "identity", width = 1) +
  geom_bar(aes(y = -`F`, fill = "Female"), stat = "identity", position = "identity", width = 1) +
  scale_fill_manual(values = c("Male" = "gold2", "Female" = "red"), name = "Gender") +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = 0.05, add = 0), labels = abs, limits = c(-max(abs(NHANES_summary_df$M), abs(NHANES_summary_df$`F`)), max(abs(NHANES_summary_df$M), abs(NHANES_summary_df$`F`)))) +
  labs(title = "NHSR Study Cohort", y = "Count", x = "Age Category") +
  theme_pubr() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x.bottom = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

aligned_plots <- pop_pyramid_masindi + plot_spacer() + pop_pyramid_NHANES + plot_layout(heights = c(1, .1, 1), ncol = 1)

tiff("FigD.tiff", units="px", width=2200, height=2000, res=400)
aligned_plots
dev.off()



####Does heart rate differ between the two groups (after adjustment for demographics)?####
Masindi <- Masindi[-5]
Combined_Df <- rbind(NHANES[-1], Masindi[-1])
Combined_Df$sex <- as.factor(Combined_Df$sex)
Combined_Df$source <- as.factor(Combined_Df$source)

#ANCOVA
ancova_model <- aov(hr ~ source + age*sex, data = Combined_Df)
summary(ancova_model) # All p < .01, reject null, significant difference in heart rates between sources


#Testing Assumptions
# 1. Normally Distributed Outcome Variable
par(mfrow = c(1, 2))
hist(Combined_Df$hr[Combined_Df$source == "Masindi"], main = "Masindi", xlab = "Heart Rate (bpm)", col = "darkgreen", xlim = c(40, 180), breaks = 30, )
hist(Combined_Df$hr[Combined_Df$source == "NHANES"], main = "NHANES", xlab = "Heart Rate (bpm)", col = "orange",  xlim = c(40, 180), breaks = 30)
hist(Combined_Df$hr[Combined_Df$sex == "F"], main = "Female", xlab = "Heart Rate (bpm)", col = "darkgreen", xlim = c(40, 180), breaks = 30, )
hist(Combined_Df$hr[Combined_Df$sex == "M"], main = "Male", xlab = "Heart Rate (bpm)", col = "orange",  xlim = c(40, 180), breaks = 30)
# HRs are approx. normally distributed across groups

# 2. Normality of Residuals
par(mfrow = c(2, 2))
plot(ancova_model) # Q-Q plot suggests normality of residuals assumption is met, also few outliers
par(mfrow = c(1, 1))

# 3. No Multicollinearity
vif(ancova_model, type = "predictor") # GVIF values close to 1 -> No Multicollinearity

# 4. Homogeneity of Variances
tiff("FigE.tiff", units="px", width=2100, height=2100, res=400)
boxplot(hr ~ source, xlab = "Source", ylab = "Heart Rate", data = Combined_Df, names = c("Masindi", "NHSR")) # Approx. homogeneity of variance
dev.off()

bartlett.test(hr ~ source, data = Combined_Df) # Test of homoscedasticity, p < .01 indicates difference in variance between source groups
leveneTest(hr ~ source, data = Combined_Df) # Robust confirmation of heteroscedasticity, p < .01
var_group <- aggregate(hr ~ source, data = Combined_Df, var)
var_ratio <- max(var_group$hr) / min(var_group$hr)
print(var_ratio) # Variance ratio 1.48 < 1.5, not a severe difference

# Confirming results with bootstrapping
boot_ancova_p <- function(data, indices) {
  boot_sample <- data[indices, ] # Resampling
  model <- aov(hr ~ source + age * sex, data = boot_sample)
  summary_aov <- summary(model)
  p_value <- summary_aov[[1]]$`Pr(>F)`[1]  # p-value for `source`
  
  return(p_value)
}

# Perform bootstrapping for p-values
set.seed(123)
boot_results_p <- boot(data = Combined_Df, statistic = boot_ancova_p, R = 1000)
mean(boot_results_p$t <= 0.05)  # Proportion of p-values <= .05 is 1; all p-values are significant
# Bootstrapping indicates results are robust to heteroscedasticity

# 5. Linear Relationship with Covariate (Age)
NHANES_M <- Combined_Df[Combined_Df$source == "NHANES" & Combined_Df$sex == "M", ]
NHANES_F <- Combined_Df[Combined_Df$source == "NHANES" & Combined_Df$sex == "F", ]
Masindi_M <- Combined_Df[Combined_Df$source == "Masindi" & Combined_Df$sex == "M", ]
Masindi_F <- Combined_Df[Combined_Df$source == "Masindi" & Combined_Df$sex == "F", ]

NHANES_M_lr <- lm(hr ~ age, data = NHANES_M)
NHANES_F_lr <- lm(hr ~ age, data = NHANES_F)
Masindi_M_lr <- lm(hr ~ age, data = Masindi_M)
Masindi_F_lr <- lm(hr ~ age, data = Masindi_F)

summary(NHANES_M_lr)  # p < .01
summary(NHANES_F_lr)  # p < .01
summary(Masindi_M_lr) # p < .01
summary(Masindi_F_lr) # p < .01

p1 <- ggplot(NHANES_M, aes(x = age, y = hr)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatterplot points
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # Regression line
  labs(title = "NHANES - Males", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal() +
  ylim(40, 160)

p2 <- ggplot(NHANES_F, aes(x = age, y = hr)) +
  geom_point(color = "red", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "NHANES - Females", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal() +
  ylim(40, 160)

p3 <- ggplot(Masindi_M, aes(x = age, y = hr)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Masindi - Males", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal() +
  ylim(40, 160)

p4 <- ggplot(Masindi_F, aes(x = age, y = hr)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Masindi - Females", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal() +
  ylim(40, 160)

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
# All linear with significant p-values and similar variance across ages

# 6. Homogeneity of regression slopes
interaction_model <- lm(hr ~ source * age + sex * age, data = Combined_Df)
reduced_model <- lm(hr ~ source + sex * age, data = Combined_Df)
anova(reduced_model, interaction_model) # p > .05, relationship between age and hr does not differ by source, assumption met

#Post-Hoc: Compute estimated marginal means for 'source', adjusting for covariates
emms <- emmeans(ancova_model, ~ source, cov.reduce = mean)
summary(emms)
pairs(emms)


####After removing "new concern" visits, do heart rates differ between the two groups?####
Masindi_routineonly <- Masindi_routineonly %>%
  filter(age <= 85)

Combined_Df_routine <- rbind(NHANES[-1], Masindi_routineonly[-1])
Combined_Df_routine$sex <- as.factor(Combined_Df_routine$sex)
Combined_Df_routine$source <- as.factor(Combined_Df_routine$source)

#ANCOVA
ancova_model_routine <- aov(hr ~ source + age*sex, data = Combined_Df_routine)
summary(ancova_model_routine) # All p < .01, reject null, significant difference in heart rates between sources

#Testing Assumptions

# 1. Normally Distributed Outcome Variable
par(mfrow = c(1, 2))
hist(Combined_Df_routine$hr[Combined_Df_routine$source == "Masindi"], main = "Masindi", xlab = "Heart Rate (bpm)", col = "darkgreen", xlim = c(40, 180), breaks = 30, )
hist(Combined_Df_routine$hr[Combined_Df_routine$source == "NHANES"], main = "NHANES", xlab = "Heart Rate (bpm)", col = "orange",  xlim = c(40, 180), breaks = 30)
hist(Combined_Df_routine$hr[Combined_Df_routine$sex == "F"], main = "Female", xlab = "Heart Rate (bpm)", col = "darkgreen", xlim = c(40, 180), breaks = 30, )
hist(Combined_Df_routine$hr[Combined_Df_routine$sex == "M"], main = "Male", xlab = "Heart Rate (bpm)", col = "orange",  xlim = c(40, 180), breaks = 30)
# HRs are approx. normally distributed across groups

# 2. Normality of Residuals
par(mfrow = c(2, 2))
plot(ancova_model_routine) # Q-Q plot suggests normality of residuals assumption is met, also few outliers
par(mfrow = c(1, 1))

# 3. No Multicollinearity
vif(ancova_model_routine, type = "predictor") # GVIF values close to 1 -> No Multicollinearity

# 4. Homogeneity of Variances
boxplot(hr ~ source, xlab = "Source", ylab = "Heart Rate", data = Combined_Df_routine) # Approx. homogeneity of variance with some positive outliers for NHANES
bartlett.test(hr ~ source, data = Combined_Df_routine) # Test of homoscedasticity, p < .01 indicates difference in variance between source groups
leveneTest(hr ~ source, data = Combined_Df_routine) # Robust confirmation of heteroscedasticity, p < .01
var_group <- aggregate(hr ~ source, data = Combined_Df_routine, var)
var_ratio <- max(var_group$hr) / min(var_group$hr)
print(var_ratio) # Variance ratio 1.46 < 1.5, not a severe difference

# Confirming results with bootstrapping
boot_ancova_p_adj <- function(data, indices) { #Adjusted for sample sizes
  nhanes_data <- data[data$source == "NHANES", ]
  masindi_data <- data[data$source == "Masindi", ]
  nhanes_indices <- sample(seq_len(nrow(nhanes_data)), size = 700, replace = TRUE) #Adjusted for large difference in data sample sizes
  masindi_indices <- sample(seq_len(nrow(masindi_data)), size = 300, replace = TRUE)
  boot_sample <- rbind(nhanes_data[nhanes_indices, ], masindi_data[masindi_indices, ])
  model <- aov(hr ~ source + age * sex, data = boot_sample)
  summary_aov <- summary(model)
  p_value <- summary_aov[[1]]$`Pr(>F)`[1]
  
  return(p_value)
}

# Perform bootstrapping for p-values
boot_results_p_adj <- boot(data = Combined_Df_routine, statistic = boot_ancova_p_adj, R = 1000)
mean(boot_results_p_adj$t <= 0.05)  # Proportion of p-values <= 0.05 is 1; all p-values are significant
#Bootstrapping indicates results are robust to heteroscedasticity

# 5. Linear Relationship with Covariate (Age)
NHANES_M_routine <- Combined_Df_routine[Combined_Df_routine$source == "NHANES" & Combined_Df_routine$sex == "M", ]
NHANES_F_routine <- Combined_Df_routine[Combined_Df_routine$source == "NHANES" & Combined_Df_routine$sex == "F", ]
Masindi_M_routine <- Combined_Df_routine[Combined_Df_routine$source == "Masindi" & Combined_Df_routine$sex == "M", ]
Masindi_F_routine <- Combined_Df_routine[Combined_Df_routine$source == "Masindi" & Combined_Df_routine$sex == "F", ]

NHANES_M_routine_lr <- lm(hr ~ age, data = NHANES_M_routine)
NHANES_F_routine_lr <- lm(hr ~ age, data = NHANES_F_routine)
Masindi_M_routine_lr <- lm(hr ~ age, data = Masindi_M_routine)
Masindi_F_routine_lr <- lm(hr ~ age, data = Masindi_F_routine)

summary(NHANES_M_routine_lr)  # p < .01
summary(NHANES_F_routine_lr)  # p < .01
summary(Masindi_M_routine_lr) # p > .05, NOT significant
summary(Masindi_F_routine_lr) # p < .01

p1 <- ggplot(NHANES_M_routine, aes(x = age, y = hr)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatterplot points
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # Regression line
  labs(title = "NHANES - Males", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal() +
  ylim(40, 160)

p2 <- ggplot(NHANES_F_routine, aes(x = age, y = hr)) +
  geom_point(color = "red", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "NHANES - Females", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal() +
  ylim(40, 160)

p3 <- ggplot(Masindi_M_routine, aes(x = age, y = hr)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Masindi - Males", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal() +
  ylim(40, 160)

p4 <- ggplot(Masindi_F_routine, aes(x = age, y = hr)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Masindi - Females", x = "Age", y = "Heart Rate (bpm)") +
  theme_minimal() +
  ylim(40, 160)

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
# Slope = 0 for Males in Masindi, while positive slope for Females in Masindi. Accounted for in interaction term (age*sex)

# 6. Homogeneity of regression slopes
interaction_routine_model <- lm(hr ~ source * age + sex * age, data = Combined_Df_routine)
reduced_routine_model <- lm(hr ~ source + sex * age, data = Combined_Df_routine)
anova(reduced_routine_model, interaction_routine_model) # p > .05, relationship between age and hr does not differ by source, assumption met


#Post-Hoc: Compute estimated marginal means for 'source', adjusting for covariates
emms <- emmeans(ancova_model_routine, ~ source, cov.reduce = mean)
summary(emms)
pairs(emms)

