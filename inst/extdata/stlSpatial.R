library(dplyr)
library(tigris)
library(sf)
library(usethis)
devtools::load_all()

# city of st. louis boundary
counties(state = 29, class = "sf") %>%
  filter(COUNTYFP == "510") %>%
  select(GEOID, NAMELSAD, ALAND, AWATER) %>%
  st_transform(crs = 4269) -> stl_city

tracts(state = 29, county = 510, class = "sf") %>%
  select(GEOID, TRACTCE, ALAND, AWATER) %>%
  st_transform(crs = 4269) -> stl_tracts10

block_groups(state = 29, county = 510, class = "sf") %>%
  select(GEOID, TRACTCE, BLKGRPCE, ALAND, AWATER) %>%
  st_transform(crs = 4269) -> stl_blockgrps10

gw_get_data(data = "Neighborhoods", class = "sf") %>%
  select(NHD_NUM, NHD_NAME) %>%
  rename(
    ID = NHD_NUM,
    NAME = NHD_NAME
  ) %>%
  st_transform(crs = 6512) %>%
  mutate(AREA = as.numeric(st_area(.))) %>%
  st_transform(crs = 4269) -> stl_nhoods

gw_get_data(data = "Voting Precincts", class = "sf") %>%
  select(HANDLE, PREC10, WARD10) %>%
  mutate(HANDLE = as.numeric(HANDLE)) %>%
  st_transform(crs = 6512) %>%
  mutate(AREA = as.numeric(st_area(.))) %>%
  st_transform(crs = 4269) -> stl_precincts10

stl_precincts10 %>%
  group_by(WARD10) %>%
  summarise(ID = first(WARD10)) %>%
  select(-WARD10) %>%
  st_transform(crs = 6512) %>%
  mutate(AREA = as.numeric(st_area(.))) %>%
  st_transform(crs = 4269) -> stl_wards10

use_data(stl_city, stl_tracts10, stl_blockgrps10, stl_nhoods, stl_precincts10, stl_wards10, internal = TRUE, overwrite = TRUE)
