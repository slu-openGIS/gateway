# create sushi

# dependencies
library(dplyr)      # data wrangling
library(postmastr)  # parse and standardize street addresses

# load data from postmastr
sushi <- sushi2

# clean and parse sample data
sushi %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(input = "short", address = "address") %>%
  select(name, pm.address, visit) %>%
  rename(address = pm.address) -> sushi

usethis::use_data(sushi)

# build sf version of local geocoder
geocoder <- gw_build_geocoder(class = "sf")

# geocode
gw_geocode(sushi, type = "local", class = "sf", address = "address", geocoder = geocoder) %>%
  select(-addrrecnum) -> sushi_sf

usethis::use_data(sushi_sf)

# create smaller geocoder
geocoder %>%
  mutate(insample = ifelse(stringr::str_detect(string = address, pattern = "Olive St") == TRUE, TRUE, FALSE)) %>%
  mutate(insample = ifelse(stringr::str_detect(string = address, pattern = "Clark Ave") == TRUE, TRUE, insample)) %>%
  mutate(insample = ifelse(stringr::str_detect(string = address, pattern = "S Grand Blvd") == TRUE, TRUE, insample)) %>%
  mutate(insample = ifelse(stringr::str_detect(string = address, pattern = "Maryland Plz") == TRUE, TRUE, insample)) %>%
  mutate(insample = ifelse(stringr::str_detect(string = address, pattern = "W Pine Blvd") == TRUE, TRUE, insample)) %>%
  mutate(insample = ifelse(stringr::str_detect(string = address, pattern = "Forest Park Ave") == TRUE, TRUE, insample)) %>%
  mutate(insample = ifelse(stringr::str_detect(string = address, pattern = "Washington Ave") == TRUE, TRUE, insample)) %>%
  mutate(insample = ifelse(stringr::str_detect(string = address, pattern = "N Euclid Ave") == TRUE, TRUE, insample)) %>%
  filter(insample == TRUE) %>%
  select(-insample) -> sample_geocoder

usethis::use_data(sample_geocoder, overwrite = TRUE)

