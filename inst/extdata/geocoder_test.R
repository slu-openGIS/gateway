

# dependencies
library(dplyr)      # data wrangling
library(gateway)    # local geocoder functions
library(postmastr)  # parse and standardize street addresses

# load sample data from gateway
sushi <- sushi

# clean and parse sample data
sushi %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(input = "short", address = "address") -> sushi_clean

# build local geocoder
geocoder <- gw_build_geocoder(return = c("id", "coords"), class = "tibble", include_units = FALSE)

# build sf version of local geocoder
x <- gw_build_geocoder(class = "sf")

# geocode
y <- gw_geocode(sushi_clean, type = "local", class = "sf", address = "pm.address", geocoder = x)

library(leaflet)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(data = y)

