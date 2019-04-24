devtools::load_all()
library(dplyr)

addresses <- dplyr::tibble(
  id = c(1:5),
  address = c("3623 Finney Ave", "3643 Delmar Blvd", "N Grand Blvd at Washington Blvd", "62 Ham Lane", "O'Fallon Park")
)

x <- gw_add_batch(addresses, id = id, address = address)

