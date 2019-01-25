library(dplyr)
library(tigris)
library(sf)
library(usethis)
library(gateway)

moCounties <- counties(state = 29, class = "sf")
moCounties %>%
  filter(COUNTYFP == "510") %>%
  select(GEOID, NAMELSAD, ALAND, AWATER) %>%
  st_transform(crs = 4269) -> stl_city

use_data(stl_city)

moCounties %>%
  filter(COUNTYFP == "189") %>%
  select(GEOID, NAMELSAD, ALAND, AWATER) %>%
  st_transform(crs = 4269) -> slc_county

use_data(slc_county)

stl_tracts10 <- tracts(state = 29, county = 510, class = "sf")
stl_tracts10 %>%
  select(GEOID, TRACTCE, ALAND, AWATER) %>%
  st_transform(crs = 4269) -> stl_tracts10

use_data(stl_tracts10)

slc_tracts10 <- tracts(state = 29, county = 189, class = "sf")
slc_tracts10 %>%
  select(GEOID, TRACTCE, ALAND, AWATER) %>%
  st_transform(crs = 4269) -> slc_tracts10

use_data(slc_tracts10)

stl_blockgrps10 <- block_groups(state = 29, county = 510, class = "sf")
stl_blockgrps10 %>%
  select(GEOID, TRACTCE, BLKGRPCE, ALAND, AWATER) %>%
  st_transform(crs = 4269) -> stl_blockgrps10

use_data(stl_blockgrps10)

slc_blockgrps10 <- block_groups(state = 29, county = 189, class = "sf")
slc_blockgrps10 %>%
  select(GEOID, TRACTCE, BLKGRPCE, ALAND, AWATER) %>%
  st_transform(crs = 4269) -> slc_blockgrps10

use_data(slc_blockgrps10)

stl_nhoods <- gw_get_city(data = "Neighborhoods")
stl_nhoods %>%
  select(NHD_NUM, NHD_NAME) %>%
  rename(
    ID = NHD_NUM,
    NAME = NHD_NAME
  ) %>%
  st_transform(crs = 6512) %>%
  mutate(AREA = as.numeric(st_area(.))) %>%
  st_transform(crs = 4269) -> stl_nhoods

use_data(stl_nhoods)

stl_precincts <- gw_get_city(data = "Voting Precincts")
stl_precincts %>%
  select(HANDLE, PREC10, WARD10) %>%
  mutate(HANDLE = as.numeric(HANDLE)) %>%
  st_transform(crs = 6512) %>%
  mutate(AREA = as.numeric(st_area(.))) %>%
  st_transform(crs = 4269) -> stl_precincts10

use_data(stl_precincts10)

stl_precincts10 %>%
  group_by(WARD10) %>%
  summarise(ID = first(WARD10)) %>%
  select(-WARD10) -> stl_wards10

use_data(stl_wards10)


