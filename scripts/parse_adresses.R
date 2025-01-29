library(tidyverse)
adr <- read_delim(
  here("data/17_15012025_03400_1.csv"), 
  delim = ";", 
  escape_double = FALSE,
  trim_ws = TRUE
) %>% 
  mutate(
    county = str_extract(TAISAADRESS, "^.*maakond"),
    city_municipality = str_extract(TAISAADRESS, ", .*(linn|vald),"),
    city_municipality =  str_trim(str_remove_all(city_municipality, "^,|,$")),
    across(c("county", "city_municipality"), \(x) str_replace_all(x, "P�r", "Pär")),
    across(c("county", "city_municipality"), \(x) str_replace_all(x, "P�([hl])", "Põ\\1")),
    across(c("county", "city_municipality"), \(x) str_replace_all(x, "L��", "Lää")),
    across(c("county", "city_municipality"), \(x) str_replace_all(x, "J�([gh])", "Jõ\\1")),
    across(c("county", "city_municipality"), \(x) str_replace_all(x, "V�i", "Väi")),
    across(c("county", "city_municipality"), \(x) str_replace_all(x, "([VT])�r", "\\1õr")),
    across(c("county", "city_municipality"), \(x) str_replace_all(x, "J�r", "Jär")),
    across(c("county", "city_municipality"), \(x) str_replace_all(x, "N�o", "Nõo")),
    across("city_municipality", \(x) str_replace(x, "^.* (vald|linn|maakond), (.* linn)", "\\2")),
    county = if_else(county == "Tartu maakond, Elva vald, Elva linn, Tartu maakond", "Tartu maakond", county)
  ) %>% 
  select(zip_code = SIHTNUMBER, county, city_municipality) %>% 
  distinct()
write_csv(adr, here("output/zip_codes.csv"))
