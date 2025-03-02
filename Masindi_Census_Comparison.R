library(tidyverse)

folder_path <- as.character(here::here()) #>>>THIS SHOULD POINT TO THE "MAIN" FOLDER<<<<#

Masindi <- read_csv(paste0(folder_path, "/Processed Data/Masindi/Masindi_data.csv"))
Masindi_census <- read_csv(paste0(folder_path, "/Processed Data/Masindi/Masindi_Census_Projections.csv"))

Masindi <- Masindi %>% #To Match Masindi census data
  filter(age < 80)

Masindi_census_long <- pivot_longer( #To Match formatting of cohort data
  Masindi_census,
  cols = c(Male, Female),
  names_to = "sex",
  values_to = "Count"
) %>% 
  drop_na (Count) %>%
  uncount(weights = Count) %>%
  mutate(sex = case_when(
    sex == "Male" ~ "M",
    sex == "Female" ~ "F",
    TRUE ~ sex))

hist(Masindi_census_long$Age) # NOT normally distributed
hist(Masindi$age) # NOT normally distributed

# Used non-parametric Mann-Whitney-Wilcoxon test rather than t-test for age
wilcox.test(Masindi$age, Masindi_census_long$Age) # p < .001

# Sex comparison
table_sex <- table( # Contingency table
  factor(c(Masindi_census_long$sex, Masindi$sex), levels = c("M", "F")), 
  factor(c(rep("Masindi Census", length(Masindi_census_long$sex)), rep("Masindi", length(Masindi$sex))), levels = c("Masindi Census", "Masindi"))
)

chi_square_test <- chisq.test(table_sex)

print(chi_square_test)
# p < .001

# Age, separated by sex
MCL_Male <- Masindi_census_long[Masindi_census_long$sex == "M", ] 
MCL_Female <- Masindi_census_long[Masindi_census_long$sex == "F", ]
Masindi_Male <- Masindi[Masindi$sex == "M", ]
Masindi_Female <- Masindi[Masindi$sex == "F", ]

wilcox.test(Masindi_Male$age, MCL_Male$Age) # p < .001
wilcox.test(Masindi_Female$age, MCL_Female$Age) # p < .001

proportion_male <- nrow(MCL_Male)/nrow(Masindi_census_long)
proportion_female <- nrow(MCL_Female)/nrow(Masindi_census_long)

mean_age_male <- mean(MCL_Male$Age)
mean_age_female <- mean(MCL_Female$Age)

model <- aov(hr ~ age*sex, data = Masindi) # From 'Masindi_HR_Analysis.R'
summary.lm(model)


# Extract coefficients
beta_0 <- coef(model)[1]  # Intercept
beta_1 <- coef(model)[2]  # Age effect
beta_2 <- coef(model)[3]  # Sex effect
beta_3 <- coef(model)[4]  # Age:Sex interaction

# Compute expected HR using weighted means
expected_HR <- beta_0 + 
  beta_1 * (mean(Masindi_census_long$Age)) +
  beta_2 * proportion_male + 
  beta_3 * (proportion_male * mean_age_male)

# Print result
expected_HR # 85.4588

mean(Masindi$hr) # 85.85728
