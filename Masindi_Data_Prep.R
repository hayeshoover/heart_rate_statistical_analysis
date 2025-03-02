# Load necessary libraries
library(tidyverse)
library(Hmisc)
library(knitr)

folder_path <- as.character(here::here()) #>>>THIS SHOULD POINT TO THE "MAIN" FOLDER<<<<#

#Read Heart Rate Data
HR_data <- read.csv(paste0(folder_path, '/Raw Data/Masindi/Masindi Data.csv'))

#Setting Labels and Factors
label(HR_data$record_id)="Record ID"
label(HR_data$age)="Age"
label(HR_data$gender)="Gender"
label(HR_data$reason)="Reason for visit"
label(HR_data$hr)="x6.lead.Heart.Rate"

HR_data$redcap_repeat_instrument.factor = factor(HR_data$redcap_repeat_instrument,levels=c("form_1","ekg_interpretation"))


# Filter and clean up data
HR_data <- HR_data %>%
  filter(redcap_repeat_instrument.factor == "form_1") %>%
  mutate(hr = X6.lead.Heart.Rate) %>%
  filter(!is.na(hr))


HR_data_export <- HR_data %>%
  select(Masindi_ID = record_id, hr = hr, age = age, sex = gender, reason = reason) %>%
  mutate(source = "Masindi",
         sex = case_when(
           sex == 1 ~ "F",
           sex == 2 ~ "M",
           TRUE ~ as.factor(sex)),
         reason = case_when(
           reason == 1 ~ "New Concern",
           reason != 1 ~ "Routine",
           TRUE ~ as.factor(reason))
         )

HR_data_export_routineonly <- HR_data %>%
  filter(reason != 1) %>%
  select(Masindi_ID = record_id, hr = hr, age = age, sex = gender) %>%
  mutate(source = "Masindi",
         sex = case_when(
           sex == 1 ~ "F",
           sex == 2 ~ "M",
           TRUE ~ as.factor(sex) 
         )) 

any(is.na(HR_data_export))             # No NAs
any(is.na(HR_data_export_routineonly)) # No NAs

# Create files for processed data
write_csv(HR_data_export, paste0(folder_path, '/Processed Data/Masindi/Masindi_data.csv'))
write_csv(HR_data_export_routineonly, paste0(folder_path, '/Processed Data/Masindi//Masindi_routine_data.csv'))


