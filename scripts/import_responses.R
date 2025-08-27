library(dplyr)
library(purrr)
library(readr)
library(here)
library(glue)
library(tidyr)
library(lubridate)
library(stringr)
library(sf)
library(tsibble)

parse_responses_files <- function(responses_files) {
  responses_files %>%
    map(\(x) read_csv(x, guess_max = 1000, show_col_types = FALSE)) %>%
    set_names(basename(responses_files)) %>%
    bind_rows(.id = "id") %>%
    mutate(
      submitted_date = as_date(submitted / (24 * 3600), origin = lubridate::origin),
      intvl = yearweek(submitted_date, week_start = 1)
    ) %>%
    group_by(intvl, participantID) %>%
    filter(submitted == max(submitted)) %>%
    ungroup() %>% 
    select(
      intvl,
      submitted_date,
      participantID,
      matches("Q")
    )
}

parse_survey_info <- function(survey_info_file) {
  read_csv(
    survey_info_file, 
    col_types = cols(
      question = col_character(),
      key = col_number(),
      questionType = col_character(),
      title = col_character(),
      label = col_character()
    ),
    show_col_types = FALSE
  )
}

parse_responses <- function(responses = c("intake", "weekly", "vaccination")) {
  stopifnot(responses %in% c("weekly", "intake", "vaccination"))
  survey_info <- parse_survey_info(here(glue("data/{responses}_survey_info.csv")))
  response <- list.files(
    here(),
    recursive = TRUE,
    full.names = TRUE,
    pattern = glue("{responses}_responses")
  ) %>%
    parse_responses_files()
  
  response_logical <- response %>% 
    select(intvl, submitted_date, participantID, where(is_logical)) %>% 
    pivot_longer(starts_with(responses)) %>% 
    filter(value) %>% 
    separate_wider_regex(name, c(question = "^.*Q[0-9A-z]+?", "\\.", key = "\\d+$")) %>%
    mutate(key = as.numeric(key)) %>%
    left_join(survey_info, by = c("question", "key")) %>% 
    select(1:3, question = title, response = label) %>% 
    group_by(intvl, submitted_date, participantID, question) %>% 
    pivot_wider(names_from = question, values_from = response, values_fn = list) %>% 
    ungroup()
  
  response_num <- response %>% 
    select(intvl, submitted_date, participantID, where(is.numeric)) %>% 
    pivot_longer(starts_with(responses)) %>% 
    drop_na()
  
  if (responses == "intake") {
    
    gender <- response_num %>% 
      filter(name == "intake.Q1") %>%
      left_join(survey_info, by = c("name" = "question", "value" = "key")) %>% 
      select(participantID, gender = label) %>% 
      distinct() %>% 
      add_count(participantID) %>%
      mutate(
        gender = if_else(n > 1, NA_character_, gender)
      ) %>% 
      distinct() %>% 
      select(-n)
    
    age <- response_num %>% 
      filter(name %in% c("intake.Q2")) %>%
      left_join(survey_info, by = c("name" = "question")) %>% 
      select(1:5) %>% 
      rename(birth_ym = value) %>% 
      mutate(
        birth_ym = as_date(as.numeric(birth_ym) / (24 * 3600), origin = lubridate::origin),
        age = round(lubridate::interval(birth_ym, submitted_date) / years(1), 0),
        age_group = cut(age, breaks = seq(0, 100, by = 10)),
        age_group = as.character(age_group),
        age_group = str_replace_all(age_group, ",", "-"),
        age_group = str_extract(age_group, "\\d+-\\d+")
      ) %>% 
      select(-name) %>% 
      group_by(participantID) %>% 
      filter(submitted_date == min(submitted_date)) %>% 
      ungroup() %>% 
      select(participantID, birth_ym, age, age_group)
    
    postcodes <- response_num %>% 
      filter(name %in% c("intake.Q3.0", "intake.Q4b.0")) %>%
      left_join(survey_info, by = c("name" = "question", "value" = "key")) %>% 
      select(1:5) %>% 
      pivot_wider(names_from = name, values_from = value) %>% 
      rename(postcode_home = intake.Q3.0, postcode_work = intake.Q4b.0)
    
    centroids_sf <- st_read(here("data/sihtnumbrid_shp/centroids.shp"), quiet = TRUE)
    ov <- st_read(here("data/omavalitsus_shp/omavalitsus.shp"), quiet = TRUE)
    
    home <- postcodes %>% 
      dplyr::select(1:4) %>%
      drop_na() %>% 
      group_by(participantID) %>% 
      filter(submitted_date == min(submitted_date)) %>% 
      ungroup() %>% 
      left_join(centroids_sf, by = c("postcode_home" = "sihtnumber")) %>%
      st_as_sf() %>% 
      st_transform(crs = st_crs(ov))
    
    home_ov <- st_intersection(home, ov) %>% 
      as_tibble() %>% 
      dplyr::select(intvl, submitted_date, participantID, postcode_home, ov_home = ONIMI, mk_home = MNIMI)
    
    work <- postcodes %>% 
      dplyr::select(1:3, 5) %>%
      drop_na() %>% 
      group_by(participantID) %>% 
      filter(submitted_date == min(submitted_date)) %>% 
      ungroup() %>% 
      left_join(centroids_sf, by = c("postcode_work" = "sihtnumber")) %>%
      st_as_sf() %>% 
      st_transform(crs = st_crs(ov))
    
    work_ov <- st_intersection(work, ov) %>% 
      as_tibble() %>% 
      dplyr::select(intvl, submitted_date, participantID, postcode_work, ov_work = ONIMI, mk_work = MNIMI)
    
    # dropping observations of same occupation submitted at different date
    occupation <- response_num %>% 
      filter(name == "intake.Q4h") %>%
      left_join(survey_info, by = c("name" = "question", "value" = "key")) %>% 
      select(1:3, occupation = label) %>% 
      group_by(participantID, occupation) %>% 
      filter(submitted_date == min(submitted_date)) %>% 
      ungroup()
    
    places <- home_ov %>% 
      full_join(work_ov, by = join_by(participantID, intvl, submitted_date)) 
    
    participants <- gender %>% 
      full_join(age) %>% 
      left_join(occupation, by = join_by(participantID)) %>% 
      left_join(places, by = join_by(participantID, intvl, submitted_date))
    
    response_logical <- response_logical %>% 
      group_by(participantID) %>% 
      filter(submitted_date == max(submitted_date)) %>% 
      ungroup() %>% 
      select(-intvl, -submitted_date)
    
    intake <- participants %>% 
      left_join(response_logical, by = join_by(participantID))
    
    return(intake)
    
  }
  
  response_num_wide <- response_num %>%
    left_join(survey_info, by = c("name" = "question", "value" = "key")) %>%
    drop_na() %>% # lets drop some of the responses for now
    select(1:3, title, label) %>%
    pivot_wider(names_from = title, values_from = label)
  
  responses_merged <- response_logical %>% 
    left_join(response_num_wide)
  
  return(responses_merged)
  
}