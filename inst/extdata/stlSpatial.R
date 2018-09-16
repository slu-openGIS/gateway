library(dplyr)
library(tigris)
library(sf)
library(usethis)

stlCity <- counties(state = 29)
stlCity %>%
  st_as_sf() %>%
  filter(COUNTYFP == "510") %>%
  select(GEOID, NAMELSAD, ALAND, AWATER) -> stlCity

use_data(stlCity)

stlTracts <- tracts(state = 29, count = 510)
stlTracts %>%
  st_as_sf() %>%
  select(GEOID, TRACTCE, ALAND, AWATER) -> stlTracts

use_data(stlTracts)
