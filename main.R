#' ---
#' title: "Gripiradar"
#' date: "`r Sys.Date()`"
#' author: "by Gripiradar collective"
#' format: 
#'    dashboard:
#'      logo: images/logo.png
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
source(here("scripts/my_label_date_short.R"))

#+
colors <- c("#2C5696", "#39870C", "#EEA230", "#D92321", "#CCE0F1", "lightgray")
old <- theme_set(theme_minimal())

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


#' 
#' ## Column {width=60%}
#' 

#' ## Symptoms
#' 

#+ fig.height=10, fig.width=9
fmt_label <- function(intvl) {
  if (year(int_start(intvl)) == year(int_end(intvl))) {
    return(paste(format(int_start(intvl), "%d-%b"), "\u2014", format(int_end(intvl), "%d-%b")))
  } else {
    return(paste(format(int_start(intvl), "%d-%b-%Y"), "\u2014", format(int_end(intvl), "%d-%b-%Y")))
  }
}
last_4_weeks <- interval(today() - weeks(5), today())
weekly_responses %>% 
  filter(int_start(intvl) %within% last_4_weeks) %>% 
  select(intvl, participantID, matches("weekly.Q1")) %>% 
  pivot_longer(starts_with("weekly"), names_to = "key") %>% 
  mutate(
    question_nr = "weekly.Q1",
    across(key, \(x) str_remove_all(x, "weekly.Q1."))
    ) %>% 
  inner_join(weekly_si) %>% 
  group_by(intvl, label) %>% 
  summarise(
    across(value, mean)
  ) %>% 
  rowwise() %>% 
  mutate(
    Interval = fmt_label(intvl),
    Interval = fct_reorder(Interval, intvl)
    ) %>% 
  ungroup() %>% 
  ggplot(aes(value, fct_reorder(label, value), fill = Interval)) +
  geom_col(position = "dodge") +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  scale_fill_manual(values = colors) +
  labs(title = "Symptoms") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    axis.title.y = element_blank()
  )

#' 
#' ## Column {width=40%}
#' 
#' 

#' 
#' ::: {.card}
#' Total number of unique user IDs: `r n_distinct(weekly_responses$participantID)`

#+
last_6_months <- interval((today() - months(6)), today())
active_users <- weekly_responses %>% 
  filter(int_start(intvl) %within% last_6_months) %>% 
  group_by(intvl) %>% 
  distinct() %>% 
  count() %>% 
  ungroup()
active_users %>% 
  ggplot(aes(as.Date(int_end(intvl)), n)) +
  geom_line(color = colors[6], linewidth = 1) +
  geom_point(color = colors[6], size = 2) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_breaks = "2 weeks", labels = my_label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "-")) +
  labs(title = "Active Users (weekly)") +
  theme(
    axis.title = element_blank()
  )


#' 
#' ::: {.card}
#+
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
  
#'
#' ::: {.card}
#' 
#+
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
    title = "Population",
    caption = paste("'Other':", sum(demographics$Gender=="Other"))
    ) +
  theme(legend.title = element_blank())


#' 
#' ::: {.card}
#' 
#+
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


# #+ include=FALSE
# demographics %>%
#   select(participantID, zip_code) %>%
#   distinct() %>%
#   count(`Sihtnumber olemas` = !is.na(zip_code))
# n_distinct(demographics$participantID)

demographics %>% 
  select(participantID, zip_code) %>% 
  left_join(adr) %>% 
  distinct() %>% 
  ggplot(aes(after_stat(count / sum(count)), fct_infreq(str_remove(county, " maakond")))) +
  geom_bar(fill = colors[1]) +
  geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.2, color = "white") +
  scale_x_continuous("Frequency (%)", labels = scales::percent) +
  labs(title = "Participant Distribution by County") +
  theme(
    axis.title.y = element_blank()
  )

#'
#' ::: {.card}
#' 
#+
mk <- st_read(here("data/maakond_shp/maakond.shp"), quiet = TRUE)
df_sf <- st_as_sf(sihtnumbrid, coords = c('viitepunkt_x', 'viitepunkt_y'))

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

ggplot(data = mk) +
  geom_sf(fill= "antiquewhite") +
  geom_sf(data = participans_by_sn, aes(color = Age, shape = Gender), alpha = 1/2, size = 3) +
  scale_shape_manual(values = c(Male = "\u2642", Female = "\u2640", Other = "\u26A5")) +
  labs(title = "Participants geographic Distribution") +
  annotation_scale() +
  coord_sf() +
  theme(
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = colors[6])
    )

