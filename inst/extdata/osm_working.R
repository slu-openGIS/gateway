library(dplyr)
library(sf)
library(tmaptools)

x <- tibble(id = c(1:4), address = c("4247 Botanical Ave", "4247 Botanical", "4100 Saint Louis Ave", "Kennedy Forest"))

geocoder <- gw_build_geocoder(style = "full", class = "tibble", return = "coords")
geocoder_s <- gw_build_geocoder(style = "short", class = "tibble", return = "coords")

gw_geocode_composite(x, var = address,
                     local_geocoder = geocoder, short_geocoder = geocoder_s,
                     local = FALSE)
