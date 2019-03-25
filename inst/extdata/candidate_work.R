library(dplyr)
library(purrr)
library(tidyr)

x <- tibble(
  id = c(1:2),
  add = c("4137 Botanical Ave", "4139 Botanical Ave"),
  zip = c(63110, 63110)
)

y <- mutate(x, geo = map2(add, zip, ~ gw_create_candidates(address = .x, zip = .y, style = "all")))


gw_create_candidates <- function(address, zip, style){

  if (style == "top"){

    api_result <- gw_add_candidates(address = address, zip = zip, n = 1)

  } else if (style == "all"){

    api_result <- gw_add_candidates(address = address, zip = zip)
    api_result <- tibble::rowid_to_column(api_result, var = "result_id")

  }

  return(api_result)


}

gw_view_candidates <- function(.data, id){

  .data$geo[[id]]

}

gw_select_candidate <- function(.data, id, candidate){

  x <- .data$geo[[id]]

  x <- filter(x, result_id == candidate)

  .data$geo[[id]] <- x

  return(.data)

}

z <- gw_select_candidate(y, id = 1, candidate = 3)
z <- gw_select_candidate(z, id = 2, candidate = 1)

unnest(z)

