#' ---
#' title: Gripiradar
#' date: 29-Jan-2024
#' author: ""
#' ---

#+ include=FALSE
knitr::opts_chunk$set(message = FALSE)

#+
library(tidyverse)
library(here)
library(jsonlite)

#+
weekly_responses_files <- list.files(here("data"), recursive = TRUE, full.names = TRUE, pattern = "weekly_responses")
weekly_responses <- weekly_responses_files %>% 
  map(read_csv) %>% 
  set_names(basename(weekly_responses_files)) %>% 
  bind_rows(.id = "id") %>% 
  separate(id, into = c("out1", "out2", "start_date", "end_date", "out3"), sep = "[_\\.]") %>% 
  mutate(
    type = paste0(out1,"_", out2),
    across(ends_with("date"), \(x) as.Date(x, tz = ""))
  ) %>% 
  select(type, start_date, end_date, participantID, starts_with("weekly"))

weekly_si <- read_csv(here("output/survey_info_weekly.csv"))


#' ## Active users
weekly_responses %>% 
  group_by(start_date) %>% 
  distinct() %>% 
  count() %>% 
  ggplot() +
  geom_line(aes(start_date, n)) +
  scale_y_continuous("Active Users", limits = c(0, NA)) +
  scale_x_date("Date", date_breaks = "month", date_labels = "%b %Y") +
  labs(title = "Gripiradar")

#' ## Symptoms
#' 

#+ fig.height=10, fig.width=9
weekly_responses %>% 
  mutate(int = interval(start_date, end_date)) %>% 
  select(int, participantID, matches("weekly.Q1")) %>% 
  pivot_longer(starts_with("weekly"), names_to = "key") %>% 
  mutate(
    question_nr = "weekly.Q1",
    across(key, \(x) str_remove_all(x, "weekly.Q1."))
    ) %>% 
  inner_join(weekly_si) %>% 
  group_by(int, label) %>% 
  summarise(
    across(value, mean)
  ) %>% 
  ggplot(aes(value, fct_reorder(label, value))) +
  geom_col() +
  facet_wrap(~int) +
  scale_y_discrete("Symptoms") +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  labs(title = "Gripiradar")

#' ## Demograpics
intake_responses_files <- list.files(here("data"), recursive = TRUE, full.names = TRUE, pattern = "intake_responses")
intake_responses <- intake_responses_files %>% 
  map(read_csv) %>% 
  set_names(basename(intake_responses_files)) %>% 
  bind_rows(.id = "id") %>% 
  separate(id, into = c("out1", "out2", "start_date", "end_date", "out3"), sep = "[_\\.]") %>% 
  mutate(
    type = paste0(out1,"_", out2),
    across(ends_with("date"), \(x) as.Date(x, tz = ""))
  ) %>% 
  select(type, start_date, end_date, participantID, starts_with("intake"))

intake_si <- read_csv(here("output/survey_info_intake.csv"))
demographics <- intake_responses %>% 
  mutate(int = interval(start_date, end_date)) %>% 
  select(int, end_date, participantID, female = intake.Q1, birth_date = intake.Q2, zip_code = intake.Q3.0) %>% 
  mutate(
    birth_date = as.Date(as_datetime(birth_date)),
    age = (end_date - birth_date) / dyears(1)
    )
  
#' ### Gender
demographics %>% 
  ggplot() +
  geom_bar(aes(fct_infreq(factor(female, labels = c("Male", "Female", "Other"))), after_stat(count / sum(count)))) +
  scale_y_continuous("Frequency (%)", labels = scales::percent) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Gripiradar")


#' ### Zip codes
adr <- read_csv(here("output/zip_codes.csv")) %>% 
  select(zip_code, county) %>% 
  distinct() %>% 
  drop_na()
demographics %>% 
  select(participantID, zip_code) %>% 
  distinct() %>% 
  drop_na() %>% 
  left_join(adr) %>% 
  ggplot() +
  geom_bar(aes(after_stat(count / sum(count)), fct_infreq(str_remove(county, " maakond")))) +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  scale_y_discrete("County") +
  labs(title = "Gripiradar")


#' ### Age groups
demographics %>% 
  select(participantID, age) %>% 
  distinct() %>% 
  drop_na() %>% 
  mutate(
    age_group = case_when(
      age <= 15 ~ "0-15",
      age > 60 ~ "60+",
      age > 15 & age <= 30 ~ "16-30",
      age > 30 & age <= 45 ~ "31-45",
      TRUE ~ "46-60"
    )
  ) %>% 
  ggplot() +
  geom_bar(aes(after_stat(count / sum(count)), age_group)) +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  scale_y_discrete("Age group") +
  labs(title = "Gripiradar")
