library(dplyr)
devtools::load_all()

test <- tibble(
  address = c("1416 Delmar Blvd", "3803 E Dr Martin Luther King Dr", "3700 Lindell Blvd", "3700 Lindell",
              "Lindell Blvd at S Vandeventer Ave", "1420 Delmar Blvd", "Calvary Cemetery", "O'Fallon Park")
)

geocoder <- gw_build_geocoder(style = "full", return = "coords")
geocoder_s <- gw_build_geocoder(style = "short", return = "coords")
geocoder_p <- gw_build_geocoder(style = "placename", return = "coords")

gw_geocode(test, type = "local", var = address, class = "tibble", local = geocoder)
gw_geocode(test, type = "local short", var = address, class = "tibble", local_short = geocoder_s)
gw_geocode(test, type = "local placename", var = address, class = "tibble", local_place = geocoder_p)
gw_geocode(test, type = "city batch", var = address, class = "tibble")
gw_geocode(test, type = "city candidate", var = address, class = "tibble", threshold = 90)
gw_geocode(test, type = "census", var = address, class = "tibble")

gw_geocode(test, type = "composite, local", var = address, class = "tibble",
           local = geocoder, local_short = geocoder_s, local_place = geocoder_p)

gw_geocode(test, type = "composite, full", var = address, class = "tibble",
           local = geocoder, local_short = geocoder_s, local_place = geocoder_p, threshold = 90)

test <- tibble(
  address = c("1416 Delmar Blvd", "3803 E Dr Martin Luther King Dr", "3700 Lindell Blvd", "3700 Lindell",
              "Lindell Blvd at S Vandeventer Ave", "1420 Delmar Blvd", "Calvary Cemetery"),
  zip = c(NA, NA, 63108, 63108, NA, NA , NA)
)

gw_geocode(test, type = "local", var = address, zip = "zip", class = "tibble", local = geocoder)
gw_geocode(test, type = "local short", var = address, zip = "zip", class = "tibble", local_short = geocoder_s)
gw_geocode(test, type = "local placename", var = address, zip = "zip", class = "tibble", local_place = geocoder_p)
gw_geocode(test, type = "city batch", var = address, zip = "zip", class = "tibble")
gw_geocode(test, type = "city candidate", var = address, zip = "zip", class = "tibble", threshold = 90)
gw_geocode(test, type = "census", var = address, zip = "zip", class = "tibble")

gw_geocode(test, type = "composite, local", var = address, zip = "zip", class = "tibble",
           local = geocoder, local_short = geocoder_s, local_place = geocoder_p, threshold = 90)

gw_geocode(test, type = "composite, full", var = address, zip = "zip", class = "tibble",
           local = geocoder, local_short = geocoder_s, local_place = geocoder_p, threshold = 90)
