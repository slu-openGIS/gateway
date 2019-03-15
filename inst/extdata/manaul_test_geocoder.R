

# dependencies
library(dplyr)      # data wrangling
library(gateway)    # local geocoder functions
library(postmastr)  # parse and standardize street addresses

# load sample data from gateway
sushi <- sushi2

# clean and parse sample data
sushi %>%
  dplyr::filter(name != "Drunken Fish - Ballpark Village") %>%
  postmastr::pm_parse(input = "short", address = "address", keep_parsed = "no") %>%
  dplyr::select(name, pm.address, visit) %>%
  dplyr::rename(address = pm.address) -> sushi_clean

# build local geocoder
geocoder <- gw_build_geocoder(return = c("id", "coords"), class = "tibble", include_units = FALSE)

# build sf version of local geocoder
geocoder <- gw_build_geocoder(class = "sf")

# geocode
sushi_sf <- gw_geocode(sushi_clean, type = "local", class = "tibble",
                       address = address, geocoder = geocoder)

library(leaflet)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(data = y)

