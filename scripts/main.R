library(tidyverse)
library(here)
library(jsonlite)
library(sf)
library(ggspatial)
library(cowplot)
library(ggfortify)
library(patchwork)

# Setup helper functions
source(here("scripts/my_label_date_short.R"))
fmt_label <- function(intvl) {
  if (year(int_start(intvl)) == year(int_end(intvl))) {
    return(paste(
      format(int_start(intvl), "%d-%b"),
      "\u2014",
      format(int_end(intvl), "%d-%b")
    ))
  } else {
    return(paste(
      format(int_start(intvl), "%d-%b-%Y"),
      "\u2014",
      format(int_end(intvl), "%d-%b-%Y")
    ))
  }
}

# Setup parameters
colors <- c("#2C5696",
            "#39870C",
            "#EEA230",
            "#D92321",
            "#CCE0F1",
            "lightgray")
old <- theme_set(theme_minimal() + theme(text = element_text(size = 12)))
last_saturday <- floor_date(today(), "week") - days(1)
last_4_weeks <- interval(last_saturday - weeks(4), last_saturday)
last_6_months <- interval((last_saturday - months(6)), last_saturday)

# Import weekly responses
weekly_responses_files <- list.files(here(),
                                     recursive = TRUE,
                                     full.names = TRUE,
                                     pattern = "weekly_responses") %>%
  tibble(weekly_responses_files = .) %>%
  mutate(
    wrf = basename(weekly_responses_files),
    i = str_split(wrf, "[_.]"),
    start = map_chr(i, 3),
    end = map_chr(i, 4),
    intl = interval(as.Date(start), as.Date(end))
  ) %>%
  filter(intl %within% last_6_months) %>%
  pull(weekly_responses_files)

# Parse weekly responses
weekly_responses <- weekly_responses_files %>%
  map(\(x) read_csv(x, show_col_types = FALSE)) %>%
  set_names(basename(weekly_responses_files)) %>%
  bind_rows(.id = "id") %>%
  separate(
    id,
    into = c("out1", "out2", "start_date", "end_date", "out3"),
    sep = "[_\\.]"
  ) %>%
  mutate(
    type = paste0(out1, "_", out2),
    across(ends_with("date"), \(x) as.Date(x, tz = "")),
    intvl = interval(start_date, end_date)
  ) %>%
  group_by(intvl, participantID) %>%
  filter(submitted == max(submitted)) %>%
  ungroup() %>%
  select(type,
         start_date,
         end_date,
         intvl,
         participantID,
         starts_with("weekly"))

# Import weekly survey info
weekly_si <- read_csv(here("data/survey_info_weekly.csv"), show_col_types = FALSE)

# Import intake responses
intake_responses_files <- list.files(here(),
                                     recursive = TRUE,
                                     full.names = TRUE,
                                     pattern = "intake_responses")

# Parse intake responses
intake_responses <- intake_responses_files %>%
  map(read_csv, show_col_types = FALSE) %>%
  set_names(basename(intake_responses_files)) %>%
  bind_rows(.id = "id") %>%
  separate(
    id,
    into = c("out1", "out2", "start_date", "end_date", "out3"),
    sep = "[_\\.]"
  ) %>%
  mutate(type = paste0(out1, "_", out2), across(ends_with("date"), \(x) as.Date(x, tz = ""))) %>%
  select(type,
         start_date,
         end_date,
         participantID,
         starts_with("intake"))

# Summarize sample demographics
demographics <- intake_responses %>%
  mutate(int = interval(start_date, end_date)) %>%
  select(
    int,
    end_date,
    participantID,
    female = intake.Q1,
    birth_date = intake.Q2,
    zip_code = intake.Q3.0
  ) %>%
  mutate(
    birth_date = as.Date(as_datetime(birth_date)),
    age = (end_date - birth_date) / dyears(1),
    Gender = factor(female, labels = c("Male", "Female", "Other")),
    age_group = cut(age, breaks = seq(0, 100, by = 10)),
    age_group = as.character(age_group),
    age_group = str_replace_all(age_group, ",", "-"),
    age_group = str_extract(age_group, "\\d+-\\d+")
  )
others <- sum(demographics$Gender == "Other")

# Import participant geography
centroids_sf <- st_read(here("data/sihtnumbrid_shp/centroids.shp"), quiet = TRUE)
participans_by_sn <- demographics %>%
  select(participantID, sihtnumber = zip_code, Gender, Age = age) %>%
  drop_na() %>%
  inner_join(centroids_sf) %>%
  st_as_sf(crs = 25884)
mk <- st_read(here("data/maakond_shp/maakond.shp"), quiet = TRUE)

# Parse symptoms
wri <- weekly_responses %>%
  filter(intvl %within% last_4_weeks) %>%
  select(intvl, participantID, matches("weekly.Q1")) %>%
  pivot_longer(starts_with("weekly"), names_to = "key") %>%
  mutate(question_nr = "weekly.Q1", across(key, \(x) str_remove_all(x, "weekly.Q1."))) %>%
  inner_join(weekly_si) %>%
  mutate(Interval = map_chr(intvl, fmt_label),
         Interval = fct_reorder(Interval, int_start(intvl)))
symptoms <- wri %>%
  group_by(Interval) %>%
  mutate(npid = n_distinct(participantID)) %>%
  group_by(Interval, label, npid) %>%
  summarise(value = sum(value))

# Import translations
languages <- read_csv(here("data/languages.csv")) %>%
  mutate(label = en) %>%
  pivot_longer(cols = c(et, en, ru),
               names_to = "lang",
               values_to = "text")

# Merge symptoms with translations
symptoms_lang <- symptoms %>%
  left_join(languages, relationship = "many-to-many")

# Dashboard data and plots
# Unique users
active_users_n <- n_distinct(weekly_responses$participantID)
active_users <- weekly_responses %>%
  group_by(intvl) %>%
  distinct() %>%
  count() %>%
  ungroup()

# Weekly users plot
active_users_p <- active_users %>%
  ggplot(aes(as.Date(int_end(intvl)), n)) +
  geom_line(color = colors[1]) +
  geom_point(color = colors[1]) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_breaks = "2 weeks",
               labels = my_label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "-")) +
  theme(axis.title = element_blank())

# Symptoms plot function
symptoms_p_fun <- function(data, lang = lang) {
  data %>%
    ggplot(aes(fct_reorder(text, value / npid, .desc = TRUE), value / npid, fill = Interval)) +
    geom_col(position = position_dodge(preserve = "single")) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = \(x) str_wrap(x, 20)) +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      legend.position = "inside",
      legend.position.inside = c(0.8, 0.7)
    )
}

nosymptoms_p_fun <- function(data, lang = lang) {
  data %>%
    ggplot(aes(Interval, value / npid)) +
    geom_point(color = colors[1]) +
    geom_line(aes(group = 1), color = colors[1]) +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ))
}

demographics_p <- demographics %>%
  filter(Gender %in% c("Female", "Male")) %>%
  count(Gender, age_group) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(if_else(Gender == "Female", -p, p), age_group, fill = Gender)) +
  geom_col() +
  scale_x_continuous(labels = \(x) scales::percent(abs(x))) +
  theme(legend.title = element_blank(), axis.title.x = element_blank())

geo_p <- ggplot(data = mk) +
  geom_sf(fill = "#CCE0F1") +
  geom_sf(data = participans_by_sn,
          color = "#39870C",
          alpha = 1 / 3) +
  coord_sf() +
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank()
  )
