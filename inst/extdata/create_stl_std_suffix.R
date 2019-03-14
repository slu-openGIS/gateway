# Create stl_std_suffix

library(postmastr)
devtools::load_all()

geocoder <- gw_build_geocoder(class = "tibble")
geocoder <- pm_identify(geocoder, var = "address")



geocoder %>%
  pm_prep(var = "address") %>%
  pm_house_parse() %>%
  pm_houseFrac_parse() %>%
 pm_streetDir_parse() %>%
  pm_streetSuf_parse() %>%
  pm_street_parse() -> parsed

vals <- unique(parsed$pm.streetSuf)

stl_std_suffix <- pm_dictionary(type = "suffix", filter = vals, case = "title")

usethis::use_data(stl_std_suffix)
