#' ---
#' title: "Gripiradar"
#' date: "`r Sys.Date()`"
#' author: ""
#' format:
#'    html:
#'      theme: "theme/gripiradar-theme.scss"
#' ---

#+ include=FALSE
knitr::opts_chunk$set(echo = FALSE, message = FALSE)

#+
library(tidyverse)
library(here)
library(jsonlite)

#+
weekly_responses_files <- list.files(here(), recursive = TRUE, full.names = TRUE, pattern = "weekly_responses")
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

weekly_si <- read_csv(here("data/survey_info_weekly.csv"))


#' ## Active users
#' 

#' Total number of unique user IDs: `r n_distinct(weekly_responses$participantID)`

#+
weekly_responses %>% 
  group_by(start_date) %>% 
  distinct() %>% 
  count() %>% 
  ggplot() +
  geom_line(aes(start_date, n)) +
  scale_y_continuous("Active Users", limits = c(0, NA)) +
  scale_x_date("Date", date_breaks = "month", date_labels = "%b %Y") +
  labs(title = "Gripiradar \u2014 Active Users")

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
  facet_wrap(~str_replace_all(str_remove_all(as.character(int), "UTC"), "--", "\u2014 ")) +
  scale_y_discrete("Symptoms") +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  labs(title = "Gripiradar \u2014 Symptoms")

#' ## Demograpics
intake_responses_files <- list.files(here(), recursive = TRUE, full.names = TRUE, pattern = "intake_responses")
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

intake_si <- read_csv(here("data/survey_info_intake.csv"))
demographics <- intake_responses %>% 
  mutate(int = interval(start_date, end_date)) %>% 
  select(int, end_date, participantID, female = intake.Q1, birth_date = intake.Q2, zip_code = intake.Q3.0) %>% 
  mutate(
    birth_date = as.Date(as_datetime(birth_date)),
    age = (end_date - birth_date) / dyears(1)
    )
  
#' ### Gender
demographics %>% 
  ggplot(aes(fct_infreq(factor(female, labels = c("Male", "Female", "Other"))), after_stat(count / sum(count)))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, color = "white") +
  scale_y_continuous("Frequency (%)", labels = scales::percent) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Gripiradar \u2014 Gender")


#' ### Zip codes
adr <- read_csv(here("data/zip_codes.csv")) %>% 
  select(zip_code, county) %>% 
  distinct()

#+ include=FALSE
demographics %>%
  select(participantID, zip_code) %>%
  distinct() %>%
  count(`Sihtnumber olemas` = !is.na(zip_code))
n_distinct(demographics$participantID)

#+
demographics %>% 
  select(participantID, zip_code) %>% 
  left_join(adr) %>% 
  distinct() %>% 
  ggplot(aes(after_stat(count / sum(count)), fct_infreq(str_remove(county, " maakond")))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.2, color = "white") +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  scale_y_discrete("County") +
  labs(title = "Gripiradar \u2014 Counties")

#' ### Age groups

#+ include=FALSE
demographics %>% 
  select(participantID, age) %>% 
  distinct() %>% 
  mutate(
    age_group = case_when(
      is.na(age) ~ NA_character_,
      age <= 15 ~ "0-15",
      age > 60 ~ "60+",
      age > 15 & age <= 30 ~ "16-30",
      age > 30 & age <= 45 ~ "31-45",
      TRUE ~ "46-60"
    )
  ) %>% 
  count(age_group)

#+
demographics %>% 
  select(participantID, age) %>% 
  distinct() %>% 
  mutate(
    age_group = case_when(
      is.na(age) ~ NA_character_,
      age <= 15 ~ "0-15",
      age > 60 ~ "60+",
      age > 15 & age <= 30 ~ "16-30",
      age > 30 & age <= 45 ~ "31-45",
      TRUE ~ "46-60"
    )
  ) %>% 
  ggplot(aes(after_stat(count / sum(count)), age_group)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.2, color = "white") +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  scale_y_discrete("Age group") +
  labs(title = "Gripiradar \u2014 Age Groups")
