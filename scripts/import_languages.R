library(tidyverse)
library(here)

lines <- read_lines(here("data/translations.txt")) 
langs <- lines %>% 
  as_tibble() %>% 
  mutate(
    id = cumsum(value == "")
  ) %>% 
  split(.$id) %>% 
  map(
    \(x) filter(x, value != "") %>% 
      select(value) %>% 
      mutate(
        lang = str_extract(value, "[enru:]{3}"),
        value = str_remove(value, lang),
        value = str_trim(str_remove_all(value, "\"")),
        value = str_remove(value, ",$"),
        lang = str_remove(lang, ":")
      ) %>% 
      pivot_wider(names_from = lang, values_from = value) %>% 
      select(ee, en, ru) %>% 
      unnest(everything())
    ) %>% 
  bind_rows() %>% 
  distinct()


langs %>% 
  write_csv(here("data/languages.csv"))

