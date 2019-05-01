devtools::load_all()
library(dplyr)

addresses <- dplyr::tibble(
  id = c(1:5),
  address = c("3623 Finney Ave", "3643 Delmar Blvd", "N Grand Blvd at Washington Blvd", "62 Ham Lane", "O'Fallon Park")
)

x <- gw_add_batch(addresses, id = "id", address = "address")

addresses <- dplyr::tibble(
  id = c(1:5),
  ham = c("3623 Finney Ave", "3643 Delmar Blvd", "N Grand Blvd at Washington Blvd", "62 Ham Lane", "O'Fallon Park")
)

y <- gw_add_batch(addresses, id = "id", address = "ham")

y <- gw_add_batch(addresses, id = "id", address = ham)

gw_geocode(addresses, type = "city batch", var = ham, class = "tibble")

addresses <- dplyr::tibble(
  id = c(500:504),
  ham = c("3623 Finney Ave", "3643 Delmar Blvd", "N Grand Blvd at Washington Blvd", "62 Ham Lane", "O'Fallon Park")
)

gw_add_batch(addresses, id = "id", address = ham)
