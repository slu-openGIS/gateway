devtools::load_all()
library(dplyr)

addresses <- dplyr::tibble(
  id = c(1:6),
  address = c("3623 Finney Ave", "3643 Delmar Blvd", "N Grand Blvd at Washington Blvd", "O'Fallon Park",
              "1400 Delmar", "Calvary Cemetery")
)

geocoder <- gw_build_geocoder(return = "coords")

gw_geocode(addresses, type = "local", var = "address", class = "tibble", geocoder = geocoder)
gw_geocode(addresses, type = "local short", var = "address", class = "tibble", geocoder = geocoder)
gw_geocode(addresses, type = "local placename", var = "address", class = "tibble", geocoder = geocoder)

gw_geocode(addresses, type = "composite, local", var = "address", class = "tibble", geocoder = geocoder)

gw_add_batch(addresses, id = "id", address = "address")

gw_geocode(addresses, type = "city batch", var = "address", class = "tibble")

gw_add_candidates(address = "3623 Finney Ave", threshold = 90, crs = 4326)

gw_geocode(addresses, type = "city candidate", var = "address", threshold = 90, class = "tibble")

gw_geocode(addresses, type = "census", var = "address", class = "tibble")

gw_geocode(addresses, type = "composite, full", var = "address", threshold = 90, class = "tibble", geocoder = geocoder)
