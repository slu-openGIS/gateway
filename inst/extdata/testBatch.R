library(dplyr)
devtools::load_all()

df <- tibble(
  id = c(1,6,8,9),
  address = c("20 N Grand", "Not an Address", "3720 Laclede Ave", "Taylor at Newstead")
)

df <- tibble(
  id = c(1:2),
  address = c("Not an Address", "Really not an address")
)

gw_add_batch(df, id, address)

gw_geocode(df, type = "city batch")
