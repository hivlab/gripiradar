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

# Setup parameters
colors <- c("#2C5696",
            "#39870C",
            "#EEA230",
            "#D92321",
            "#CCE0F1",
            "lightgray")
old <- theme_set(
  theme_minimal() +
    theme(
      text = element_text(size = 12),
      plot.caption = element_text(color = "gray40")
    )
)
this_monday <- floor_date(today(), "week", week_start = 1)
last_4_weeks <- lubridate::interval(
  this_monday - weeks(4), 
  this_monday - 1
)
last_6_months <- lubridate::interval(
  ceiling_date(this_monday - months(6), "week", week_start = 1), 
  this_monday - 1
)

# Import weekly responses
weekly_responses <- parse_responses("weekly") %>%
  select(intvl, submitted_date, participantID, 
         symptoms = "Have you had any of the following symptoms since your last questionnaire (or in the past week, if this the first tie you are taking this questionnaire)?", 
         suddenly = "Did your symptoms develop suddenly over a few hours?") %>% 
  unnest(symptoms)

# Import intake responses
intake_responses <- parse_responses("intake") %>%
  select(participantID, gender, age_group, ov_home, mk_home)

# Import vaccination responses (latest answer per participant within 6mo window)
vaccination_responses <- parse_responses("vaccination") %>%
  filter(submitted_date %within% last_6_months) %>%
  select(
    participantID, submitted_date,
    flu_this_season = "Have you received a flu vaccine this autumn/winter season? (2025-2026)",
    covid_ever = "Have you received a COVID-19 vaccine?"
  ) %>%
  group_by(participantID) %>%
  filter(submitted_date == max(submitted_date)) %>%
  slice(1) %>%
  ungroup()

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
weekly_responses_6mo <- weekly_responses %>%
  filter(submitted_date %within% last_6_months)
active_users_n <- n_distinct(weekly_responses_6mo$participantID)
active_pids_6mo <- unique(weekly_responses_6mo$participantID)
active_users <- weekly_responses_6mo %>%
  group_by(intvl) %>%
  summarise(
    n = n_distinct(participantID)
  ) %>%
  ungroup()

# Vaccination headline numbers (denominator: active weekly users in 6mo)
vacc_active <- vaccination_responses %>%
  filter(participantID %in% active_pids_6mo)
flu_vacc_pct <- if (nrow(vacc_active) > 0) {
  scales::percent(mean(str_starts(vacc_active$flu_this_season, "Yes"), na.rm = TRUE), accuracy = 0.1)
} else "—"
covid_vacc_pct <- if (nrow(vacc_active) > 0) {
  scales::percent(mean(str_starts(vacc_active$covid_ever, "Yes"), na.rm = TRUE), accuracy = 0.1)
} else "—"

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
      legend.position = "bottom",
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
          axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))
}

# ILI plot.
# Numerator: participants meeting the case definition (sudden onset +
# systemic + respiratory symptoms). Denominator: ALL weekly responders
# that week, including those reporting "No symptoms" — this is the
# standard participatory-surveillance denominator.
ili_per_pid <- weekly_responses_6mo %>%
  group_by(intvl, participantID) %>%
  summarise(
    has_systemic = any(str_detect(str_to_lower(symptoms), "fever|malaise|headache|myalgia")),
    has_resp = any(str_detect(str_to_lower(symptoms), "cough|sore throat|shortness of breath")),
    has_sudden = any(str_detect(suddenly, "Yes"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(is_ili = has_systemic & has_resp & has_sudden)

ili <- ili_per_pid %>%
  group_by(intvl) %>%
  summarise(
    total = n_distinct(participantID),
    ili_n = sum(is_ili),
    .groups = "drop"
  ) %>%
  mutate(
    raw_rate = ili_n / total,
    intvl_num = as.numeric(intvl)
  )

# Smooth on the logit scale via a binomial GAM, then back-transform.
# Gives a temporally coherent rate and 95% CI ribbon that doesn't
# whip around with single-week noise.
# k caps basis dimension; gamma<1 lets the fit follow weekly trends
# instead of over-smoothing into a near-flat line.
ili_k <- min(nrow(ili) - 2, 15)
ili_gam <- mgcv::gam(
  cbind(ili_n, total - ili_n) ~ s(intvl_num, k = ili_k),
  family = binomial,
  data = ili,
  gamma = 0.4
)
ili_pred <- predict(ili_gam, newdata = ili, type = "link", se.fit = TRUE)
ili <- ili %>%
  mutate(
    estimate = plogis(ili_pred$fit),
    conf_int_1 = plogis(ili_pred$fit - 1.96 * ili_pred$se.fit),
    conf_int_2 = plogis(ili_pred$fit + 1.96 * ili_pred$se.fit)
  )

ili_p <- ili %>%
  ggplot(aes(intvl, estimate)) +
  geom_ribbon(aes(ymin = conf_int_1, ymax = conf_int_2), alpha = 0.2, fill = colors[1]) +
  geom_line(aes(group = 1), color = colors[1], linewidth = 0.8) +
  geom_point(aes(y = raw_rate), color = colors[1], alpha = 0.4, size = 1.5) +
  labs(y = "ILI") +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_x_yearmonth(date_breaks = "1 month",
                    labels = my_label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "-")) +
  theme(axis.title.x = element_blank())

ili_last <- ili %>%
  filter(intvl == max(intvl)) %>%
  pull(estimate) %>%
  scales::percent(accuracy = 0.1)

# ILI by age group — weekly rate per age group, suppressed where N is too low
age_min_n <- 5
ili_age <- weekly_responses_6mo %>%
  left_join(intake_responses %>% select(participantID, age_group) %>% distinct(),
            by = "participantID") %>%
  filter(!is.na(age_group)) %>%
  group_by(intvl, age_group, participantID) %>%
  summarise(
    has_systemic = any(str_detect(str_to_lower(symptoms), "fever|malaise|headache|myalgia")),
    has_resp = any(str_detect(str_to_lower(symptoms), "cough|sore throat|shortness of breath")),
    sudden = any(str_detect(suddenly, "Yes"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(is_ili = has_systemic & has_resp & sudden) %>%
  group_by(intvl, age_group) %>%
  summarise(
    n = n_distinct(participantID),
    ili_n = sum(is_ili),
    .groups = "drop"
  ) %>%
  mutate(rate = if_else(n >= age_min_n, ili_n / n, NA_real_))

ili_age_p <- ili_age %>%
  ggplot(aes(intvl, age_group, fill = rate)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_x_yearmonth(
    date_breaks = "1 month",
    labels = my_label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "-")
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Demographics plot
intake_responses_6mo <- intake_responses %>%
  filter(participantID %in% unique(weekly_responses_6mo$participantID))
demographics_p <- intake_responses_6mo %>%
  filter(gender %in% c("Female", "Male")) %>% 
  count(gender, age_group) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(if_else(gender == "Female", -p, p), age_group, fill = gender)) +
  geom_col() +
  scale_x_continuous(labels = \(x) scales::percent(abs(x))) +
  theme(legend.title = element_blank(), axis.title.x = element_blank())

others <- intake_responses_6mo %>% 
  filter(gender == "Other") %>% 
  pull(participantID) %>% 
  n_distinct()

mk <- st_read(here("data/maakond_shp/maakond.shp"), quiet = TRUE) %>%
  st_simplify(dTolerance = 200)

# Per-maakond ILI rate over the last 4 weeks.
# Suppress estimates from counties with too few participants to avoid 1/1 spikes.
mk_min_n <- 5

weekly_responses_4w <- weekly_responses %>%
  filter(submitted_date %within% last_4_weeks)

ili_pids_4w <- weekly_responses_4w %>%
  filter(symptoms != "No symptoms") %>%
  group_by(participantID) %>%
  summarise(
    systemic = any(str_detect(str_to_lower(symptoms), "fever|malaise|headache|myalgia")),
    resp = any(str_detect(str_to_lower(symptoms), "cough|sore throat|shortness of breath")),
    sudden = any(str_detect(suddenly, "Yes")),
    .groups = "drop"
  ) %>%
  filter(systemic, resp, sudden) %>%
  pull(participantID)

mk_ili <- intake_responses %>%
  filter(participantID %in% unique(weekly_responses_4w$participantID)) %>%
  drop_na(mk_home) %>%
  group_by(mk_home) %>%
  summarise(
    n = n_distinct(participantID),
    ili_n = n_distinct(participantID[participantID %in% ili_pids_4w]),
    .groups = "drop"
  ) %>%
  mutate(rate = if_else(n >= mk_min_n, ili_n / n, NA_real_))

mk_ili_sf <- mk %>%
  left_join(mk_ili, by = c("MNIMI" = "mk_home"))

geo_p <- ggplot(mk_ili_sf) +
  geom_sf(aes(fill = rate), color = "white") +
  coord_sf() +
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank()
  )

