library(tidyverse)
library(haven)

folder_path <- as.character(here::here()) #>>>THIS SHOULD POINT TO THE "MAIN" FOLDER<<<<#

#Data Import
BPX_1999_2000 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/BPX_1999_2000.xpt"))
BPX_2001_2002 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/BPX_2001_2002.xpt"))
BPX_2003_2004 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/BPX_2003_2004.xpt"))
BPX_2005_2006 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/BPX_2005_2006.xpt"))
BPX_2007_2008 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/BPX_2007_2008.xpt"))
DEMO_1999_2000 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/DEMO_1999_2000.xpt"))
DEMO_2001_2002 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/DEMO_2001_2002.xpt"))
DEMO_2003_2004 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/DEMO_2003_2004.xpt"))
DEMO_2005_2006 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/DEMO_2005_2006.xpt"))
DEMO_2007_2008 <- read_xpt(paste0(folder_path, "/Raw Data/NHANES/DEMO_2007_2008.xpt"))

HR_DEMO_1999_2000 <- BPX_1999_2000 %>%
  select(SEQN, BPXPLS) %>%
  inner_join(DEMO_1999_2000, by = "SEQN") %>%
  select(1:7)

HR_DEMO_2001_2002 <- BPX_2001_2002 %>%
  select(SEQN, BPXPLS) %>%
  inner_join(DEMO_2001_2002, by = "SEQN") %>%
  select(1:7)

HR_DEMO_2003_2004 <- BPX_2003_2004 %>%
  select(SEQN, BPXPLS) %>%
  inner_join(DEMO_2003_2004, by = "SEQN") %>%
  select(1:7)

HR_DEMO_2005_2006 <- BPX_2005_2006 %>%
  select(SEQN, BPXPLS) %>%
  inner_join(DEMO_2005_2006, by = "SEQN") %>%
  select(1:7)

HR_DEMO_2007_2008 <- BPX_2007_2008 %>%
  select(SEQN, BPXPLS) %>%
  inner_join(DEMO_2007_2008, by = "SEQN") %>%
  select(1:7)

HR_DEMO <- bind_rows(HR_DEMO_1999_2000, HR_DEMO_2001_2002, HR_DEMO_2003_2004, HR_DEMO_2005_2006, HR_DEMO_2007_2008)
any(duplicated(HR_DEMO$SEQN)) # FALSE --> NO repeated SEQN (Identification Codes)

HR_DEMO <- HR_DEMO %>%
  filter(RIDAGEYR >= 18) %>% # Adults only
  filter(!is.na(BPXPLS)) %>% # Must have Pulse data
  select(NHANES_ID = SEQN, hr = BPXPLS, age = RIDAGEYR, sex = RIAGENDR) %>% # 1 = MALE, 2 = FEMALE
  mutate(source = "NHANES",
         sex = case_when(
           sex == 1 ~ "M",
           sex == 2 ~ "F",
           TRUE ~ as.factor(sex) 
         ))
  
write_csv(HR_DEMO, paste0(folder_path, "/Processed Data/NHANES/HR_DEMO_NHANES.csv"))
