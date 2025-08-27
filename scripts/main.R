library(tidyverse)
library(here)
library(jsonlite)
library(sf)
library(ggspatial)
library(cowplot)
library(ggfortify)
library(patchwork)
library(tsibble)

# Setup helper functions
source(here("scripts/my_label_date_short.R"))
source(here("scripts/import_responses.R"))
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
this_monday <- floor_date(today(), "week", week_start = 1)
last_4_weeks <- lubridate::interval(this_monday - weeks(4), this_monday - 1)
last_6_months <- lubridate::interval(this_monday - months(6), this_monday - 1)

# Import weekly responses
weekly_responses <- parse_responses("weekly") %>% 
  select(1:4) %>% 
  rename(symptoms = 4) %>% 
  unnest(symptoms)

# Import intake responses
intake_responses <- parse_responses("intake") %>% 
  select(participantID, gender, age_group, ov_home)

# Import translations
languages <- read_csv(here("data/languages.csv")) %>%
  mutate(label = en) %>%
  pivot_longer(cols = c(et, en, ru),
               names_to = "lang",
               values_to = "text")

# Merge symptoms with translations
symptoms_lang <- weekly_responses %>%
  filter(submitted_date %within% last_4_weeks) %>% 
  left_join(languages, by = join_by(symptoms == label), relationship = "many-to-many")

# Dashboard data and plots
# Unique users
active_users_n <- n_distinct(weekly_responses$participantID)
active_users <- weekly_responses %>%
  filter(submitted_date %within% last_6_months) %>% 
  group_by(intvl) %>%
  summarise(
    n = n_distinct(participantID)
  ) %>%
  ungroup()

# Weekly users plot
active_users_p <- active_users %>%
  ggplot(aes(intvl, n)) +
  geom_line(color = colors[1]) +
  geom_point(color = colors[1]) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_yearmonth(date_breaks = "1 month",
               labels = my_label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "-")) +
  theme(axis.title = element_blank())

# Symptoms plot function
symptoms_p_fun <- function(data, lang = lang) {
  data %>%
    ggplot(aes(fct_reorder(text, n / npid, .desc = TRUE), n / npid, fill = as.factor(intvl))) +
    geom_col(position = position_dodge(preserve = "single")) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = \(x) str_wrap(x, 20)) +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    )
}

nosymptoms_p_fun <- function(data, lang = lang) {
  data %>%
    ggplot(aes(intvl, n / npid)) +
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

# ILI plot
ili <- weekly_responses %>%
  filter(submitted_date %within% last_6_months, symptoms != "No symptoms") %>% 
  group_by(intvl) %>% 
  mutate(
    total = n_distinct(participantID)
  ) %>% 
  filter(str_detect(symptoms, "Fever|Cough")) %>% 
  group_by(intvl, participantID, total) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  count(intvl, total) %>% 
  mutate(
    prop_test = map2(n, total, prop.test),
    estimate = map_dbl(prop_test, "estimate"),
    conf_int = map(prop_test, "conf.int")
  ) %>% 
  unnest_wider(conf_int, names_sep = "_")
  
ili_p <- ili %>% 
  ggplot(aes(intvl, estimate)) +
  geom_line(aes(group = 1), color = colors[1]) +
  geom_point(color = colors[1]) +
  geom_ribbon(aes(ymin = conf_int_1, ymax = conf_int_2), alpha = 0.2) +
  labs(y = "ILI") +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_x_yearmonth(date_breaks = "1 month",
                    labels = my_label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "-")) +
  theme(axis.title.x = element_blank())

ili_last <- ili %>% 
  filter(intvl == max(intvl)) %>% 
  pull(estimate) %>% 
  scales::percent(accuracy = 0.1)

# Demographics plot
demographics_p <- intake_responses %>%
  filter(participantID %in% unique(weekly_responses$participantID), gender %in% c("Female", "Male")) %>% 
  count(gender, age_group) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(if_else(gender == "Female", -p, p), age_group, fill = gender)) +
  geom_col() +
  scale_x_continuous(labels = \(x) scales::percent(abs(x))) +
  theme(legend.title = element_blank(), axis.title.x = element_blank())

others <- intake_responses %>% 
  filter(gender == "Other") %>% 
  pull(participantID) %>% 
  n_distinct()

mk <- st_read(here("data/maakond_shp/maakond.shp"), quiet = TRUE)
ov <- st_read(here("data/omavalitsus_shp/omavalitsus.shp"), quiet = TRUE)
ov_counts <- ov %>% 
  left_join(
    intake_responses %>%
      filter(participantID %in% unique(weekly_responses$participantID)) %>% 
      rename(ONIMI = ov_home) %>% 
      drop_na() %>% 
      count(ONIMI)
  ) %>% 
  mutate(n = if_else(is.na(n), 0, n)) 

geo_p <- ggplot(ov_counts) +
  geom_sf(aes(fill = n), color = "white") +
  geom_sf(data = mk, alpha = 1/100, color = "gray50") +
  coord_sf() +
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank()
  )

