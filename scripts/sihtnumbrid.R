library(tidyverse)
library(here)
library(sf)

temp <- tempfile()
download.file("https://geoportaal.maaamet.ee/docs/aadress/sihtnumbrid.zip?t=20250305063216", temp)
sihtnumbrid <- read_delim(
  unz(temp, "sihtnumbrid.csv"), 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE, 
  show_col_types = FALSE
) %>% 
  rename_all(str_to_lower)
unlink(temp)
df_sf <- st_as_sf(sihtnumbrid, coords = c('viitepunkt_x', 'viitepunkt_y'))
centroids_sf <- df_sf %>%
  group_by(sihtnumber) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_centroid() %>% 
  st_set_crs(25884)
st_write(centroids_sf, "data/sihtnumbrid_shp/centroids.shp")
