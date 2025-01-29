library(tidyverse)
library(here)

weekly_si <- list.files("data/24.01.2025/weekly/", recursive = TRUE, full.names = TRUE, pattern = "survey_info.csv") %>% 
  map(read_csv) %>% 
  bind_rows() %>% 
  rename(question = title, question_nr = Q)

write_csv(weekly_si, here("output/survey_info_weekly.csv"))

intake_si <- list.files("data/24.01.2025/intake/", recursive = TRUE, full.names = TRUE, pattern = "survey_info.csv") %>% 
  map(read_csv) %>% 
  bind_rows() %>% 
  rename(question = title, question_nr = Q)

write_csv(intake_si, here("output/survey_info_intake.csv"))

vaccination_si <- list.files("data/24.01.2025/vaccination/", recursive = TRUE, full.names = TRUE, pattern = "survey_info.csv") %>% 
  map(read_csv) %>% 
  bind_rows() %>% 
  rename(question = title, question_nr = Q)

write_csv(intake_si, here("output/survey_info_vaccination.csv"))


