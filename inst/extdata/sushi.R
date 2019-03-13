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
