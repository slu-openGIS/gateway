library(dplyr)
devtools::load_all()

test <- tibble(
  address = c("1416 Delmar Blvd", "3803 E Dr Martin Luther King Dr", "3700 Lindell Blvd", "3700 Lindell",
              "Lindell Blvd at S Vandeventer Ave", "1420 Delmar Blvd")
)

geocoder <- gw_build_geocoder(style = "full", class = "tibble", return = "coords")
geocoder_s <- gw_build_geocoder(style = "short", class = "tibble", return = "coords")

gw_geocode_composite(test, var = address, local_geocoder = geocoder, short_geocoder = geocoder_s, local = FALSE)
