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

num_to_date <- function(x) as_date(x / (24 * 3600), origin = lubridate::origin)
rep_safely <- safely(rep, otherwise = NA_character_)
date_questions_regex <- "(Q10b|Q35j)(\\.1)?"

parse_responses_files <- function(responses_files) {
  responses_files %>%
    map(\(x) read_csv(x, guess_max = 1000, show_col_types = FALSE)) %>%
    set_names(basename(responses_files)) %>%
    bind_rows(.id = "id") %>%
    mutate(
      submitted_date = num_to_date(submitted),
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
    select(intvl, submitted_date, participantID, question = title, response = label) %>% 
    group_by(intvl, submitted_date, participantID, question) %>% 
    pivot_wider(names_from = question, values_from = response, values_fn = list) %>% 
    ungroup()
  
  response_num_all <- response %>% 
    select(intvl, submitted_date, participantID, where(is.numeric)) %>% 
    pivot_longer(starts_with(responses)) %>% 
    drop_na()
  
  response_date <- response_num_all %>% 
    filter(str_detect(name, date_questions_regex), value > 1) %>% 
    mutate(
      label = num_to_date(value),
      name = str_replace(name, date_questions_regex, "\\1")
      ) %>% 
    select(-value)
  
  response_num <- response_num_all %>% 
    filter(!str_detect(name, date_questions_regex))
  
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
      select(intvl, submitted_date, participantID, name, value) %>% 
      rename(birth_ym = value) %>% 
      mutate(
        birth_ym = num_to_date(as.numeric(birth_ym)),
        age = round(lubridate::interval(birth_ym, submitted_date) / years(1), 0),
        age_group = cut(age, breaks = c(0, 4, 18, 44, 64, 100), include.lowest = TRUE),
        age_group1 = factor(age_group, levels = c("[0,4]", "(4,18]", "(18,44]", "(44,64]", "(64,100]"), labels = c("0-4", "5-18", "19-44", "45-64", "65+"))
      ) %>% 
      select(-name) %>% 
      group_by(participantID) %>% 
      filter(submitted_date == min(submitted_date)) %>% 
      ungroup() %>% 
      select(participantID, birth_ym, age, age_group)
    
    household_ppl <- response_num %>% 
      filter(str_detect(name, "intake.Q6.mat.row")) %>% 
      mutate(
        name = case_when(
          name == "intake.Q6.mat.row0.col1" ~ "0-4",
          name == "intake.Q6.mat.row1.col1" ~ "5-18",
          name == "intake.Q6.mat.row2.col1" ~ "19-44",
          name == "intake.Q6.mat.row3.col1" ~ "45-64",
          TRUE ~ "65+"
        ),
        name = factor(name, levels = c("0-4", "5-18", "19-44", "45-64", "65+"))
      ) %>% 
      group_by(participantID) %>% 
      mutate(
        resp = map2(name, value, \(x, y) rep(x, y))
      ) %>% 
      select(participantID, resp) %>% 
      reframe(
        across(resp, \(x) list(x))
      ) %>% 
      mutate(
        resp = map(resp, unlist)
      ) %>% 
      rename(`INCLUDING YOU, how many people in each of the following age groups live in your household?` =  resp)

    postcodes <- response_num %>% 
      filter(name %in% c("intake.Q3.0", "intake.Q4b.0")) %>%
      left_join(survey_info, by = c("name" = "question", "value" = "key")) %>% 
      select(intvl, submitted_date, participantID, name, value) %>% 
      pivot_wider(names_from = name, values_from = value) %>% 
      rename(postcode_home = intake.Q3.0, postcode_work = intake.Q4b.0)
    
    centroids_sf <- st_read(here("data/sihtnumbrid_shp/centroids.shp"), quiet = TRUE)
    ov <- st_read(here("data/omavalitsus_shp/omavalitsus.shp"), quiet = TRUE)
    
    home <- postcodes %>% 
      dplyr::select(intvl, submitted_date, participantID, postcode_home) %>%
      drop_na() %>% 
      group_by(participantID) %>% 
      filter(submitted_date == min(submitted_date)) %>% 
      ungroup() %>% 
      left_join(centroids_sf, by = c("postcode_home" = "sihtnumber")) %>%
      st_as_sf() %>% 
      st_transform(crs = st_crs(ov))
    
    home_ov <- st_intersection(home, ov) %>% 
      as_tibble() %>% 
      dplyr::select(participantID, postcode_home, ov_home = ONIMI, mk_home = MNIMI)
    
    work <- postcodes %>% 
      dplyr::select(intvl, submitted_date, participantID, postcode_work) %>%
      drop_na() %>% 
      group_by(participantID) %>% 
      filter(submitted_date == min(submitted_date)) %>% 
      ungroup() %>% 
      left_join(centroids_sf, by = c("postcode_work" = "sihtnumber")) %>%
      st_as_sf() %>% 
      st_transform(crs = st_crs(ov))
    
    work_ov <- st_intersection(work, ov) %>% 
      as_tibble() %>% 
      dplyr::select(participantID, postcode_work, ov_work = ONIMI, mk_work = MNIMI)
    
    # dropping observations of same occupation submitted at different date
    occupation <- response_num %>% 
      filter(name == "intake.Q4h") %>%
      left_join(survey_info, by = c("name" = "question", "value" = "key")) %>% 
      dplyr::select(intvl, submitted_date, participantID, occupation = label) %>% 
      group_by(participantID, occupation) %>% 
      filter(submitted_date == min(submitted_date)) %>% 
      ungroup() %>% 
      select(participantID, occupation)
    
    places <- home_ov %>% 
      full_join(work_ov, by = join_by(participantID)) 
    
    participants <- gender %>% 
      full_join(age, by = join_by(participantID)) %>% 
      left_join(occupation, by = join_by(participantID)) %>% 
      left_join(places, by = join_by(participantID))
    
    response_logical <- response_logical %>% 
      group_by(participantID) %>% 
      filter(submitted_date == max(submitted_date)) %>% 
      ungroup() %>% 
      select(-intvl, -submitted_date)
    
    intake <- participants %>% 
      left_join(response_logical, by = join_by(participantID)) %>% 
      left_join(household_ppl, by = join_by(participantID)) %>% 
      distinct()
    
    return(intake)
    
  }
  
  response_num_wide <- response_num %>%
    left_join(survey_info, by = c("name" = "question", "value" = "key")) %>%
    drop_na() %>% # lets drop some of the responses for now
    select(intvl, submitted_date, participantID, title, label) %>%
    pivot_wider(names_from = title, values_from = label)
  
  response_date_wide <- response_date %>% 
    left_join(
      survey_info %>% 
        filter(str_detect(question, date_questions_regex)) %>% 
        select(question, title, questionType) %>% 
        distinct(), 
      by = join_by(name == question)
    ) %>% 
    select(intvl, submitted_date, participantID, title, label) %>%
    pivot_wider(names_from = title, values_from = label)
  
  responses_merged <- response_logical %>% 
    left_join(response_num_wide) %>% 
    left_join(response_date_wide)
  
  return(responses_merged)
  
}