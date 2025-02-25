#' ---
#' title: "Gripiradar"
#' date: "`r Sys.Date()`"
#' author: ""
#' format: 
#'    html: 
#'      embed-resources: true
#' ---

#+ include=FALSE
knitr::opts_chunk$set(echo = FALSE, message = FALSE)

#+
library(tidyverse)
library(lubridate)
library(here)
library(jsonlite)
library(sf)
library(ggspatial)
library(cowplot)

#+
colors <- c("#2C5696", "#39870C", "#EEA230", "#D92321", "#CCE0F1", "lightgray")

#+
weekly_responses_files <- list.files(here(), recursive = TRUE, full.names = TRUE, pattern = "weekly_responses")
weekly_responses <- weekly_responses_files %>% 
  map(read_csv) %>% 
  set_names(basename(weekly_responses_files)) %>% 
  bind_rows(.id = "id") %>% 
  separate(id, into = c("out1", "out2", "start_date", "end_date", "out3"), sep = "[_\\.]") %>% 
  mutate(
    type = paste0(out1,"_", out2),
    across(ends_with("date"), \(x) as.Date(x, tz = "")),
    intvl = interval(start_date, end_date)
  ) %>% 
  filter(!(today() %within% intvl)) %>% 
  select(type, start_date, end_date, intvl, participantID, starts_with("weekly"))

weekly_si <- read_csv(here("data/survey_info_weekly.csv"))


#' ## Active users
#' 

#' Total number of unique user IDs: `r n_distinct(weekly_responses$participantID)`

#+
weekly_responses %>% 
  group_by(intvl) %>% 
  distinct() %>% 
  count() %>% 
  ggplot() +
  geom_line(aes(as.Date(int_end(intvl)), n), color = colors[1]) +
  scale_y_continuous("Active Users", limits = c(0, NA)) +
  scale_x_date("Date", date_labels = "%b %d %Y") +
  labs(title = "Gripiradar \u2014 Active Users")

#' ## Symptoms
#' 

#+ fig.height=10, fig.width=9
last_wks <- interval(today() - weeks(4), today())
weekly_responses %>% 
  filter(end_date %within% last_wks) %>% 
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
  mutate(
    Interval = str_replace_all(str_remove_all(as.character(int), "UTC"), "--", "\u2014 ")
    ) %>% 
  ggplot(aes(value, fct_reorder(label, value), fill = Interval)) +
  geom_col(position = "dodge") +
  scale_y_discrete("Symptoms") +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  scale_fill_manual(values = colors) +
  labs(title = "Gripiradar \u2014 Symptoms") +
  guides(fill = guide_legend(reverse = TRUE))

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
    age = (end_date - birth_date) / dyears(1),
    Gender = factor(female, labels = c("Male", "Female", "Other")),
    age_group = cut(age, breaks = seq(0, 100, by = 10)),
    age_group = as.character(age_group),
    age_group = str_replace_all(age_group, ",", "-"),
    age_group = str_extract(age_group, "\\d+-\\d+")
    )
  
#' ### Gender and age group
demographics %>% 
  filter(Gender %in% c("Female", "Male")) %>% 
  count(Gender, age_group) %>% 
  ggplot(aes(if_else(Gender == "Female", -n, n), age_group, fill = Gender)) +
  geom_col() +
  geom_text(aes(label = abs(n)), color = "black") +
  scale_x_continuous("N", labels = abs) +
  scale_y_discrete("Age group") +
  scale_fill_manual(values = colors[3:4], limits = rev) +
  labs(
    title = "Gripiradar \u2014 Population",
    caption = paste("'Other':", sum(demographics$Gender=="Other"))
    ) +
  theme(legend.title = element_blank())

#' ### Zip codes
adr <- read_csv(here("data/zip_codes.csv")) %>% 
  select(zip_code, county) %>% 
  distinct()
sihtnumbrid <- read_delim(
  here("data/sihtnumbrid.csv"), 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE
) %>% 
  rename_all(str_to_lower)


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
  geom_bar(fill = colors[1]) +
  geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.2, color = "white") +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  scale_y_discrete("County") +
  labs(title = "Gripiradar \u2014 Counties")

#+
mk <- st_read(here("data/maakond_shp/maakond.shp"), quiet = TRUE)
df_sf <- st_as_sf(sihtnumbrid, coords = c('viitepunkt_x', 'viitepunkt_y'))

#+
centroids_sf <- df_sf %>%
  group_by(sihtnumber) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_centroid
participans_by_sn <- demographics %>% 
  select(participantID, sihtnumber = zip_code, Gender, Age = age) %>% 
  drop_na() %>% 
  inner_join(
    centroids_sf
      ) %>% 
  st_as_sf(crs = 25884)

#+
ggplot(data = mk) +
  geom_sf(fill= "antiquewhite") +
  geom_sf(data = participans_by_sn, aes(color = Age, shape = Gender), alpha = 1/2, size = 3) +
  scale_shape_manual(values = c(Male = "\u2642", Female = "\u2640", Other = "\u26A5")) +
  labs(title = "Gripiradar") +
  annotation_scale() +
  coord_sf() +
  theme(
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = colors[6])
    )

