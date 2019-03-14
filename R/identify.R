#' Add Areal Unit Identifiers to Point Data
#'
#' @description Apply identifiers from a selection of commonly used areal units
#'    to point data stored in a \code{sf} object.
#'
#' @usage gw_identify(.data, to, side = "right", class = "sf")
#'
#' @param .data A \code{sf} object
#' @param to The string name of an areal unit to aggregate to: \code{"block group"}, \code{"tract"},
#'     \code{"precinct"}, \code{"ward"}, or \code{"neighborhood"}.
#' @param side One of either \code{"right"} or \code{"left"} indicating where the identifier variable
#'     should be placed in the
#' @param class One of either \code{"sf"} (default) or \code{"tibble"}.
#'
#' @return \code{gw_identify} returns either a \code{sf} object or tibble with the requested data.
#'
#' @export
gw_identify <- function(.data, to, side = "right", class = "sf"){

  # global bindings
  ID = NULL

  # check to inputs
  areas_all <- c("block group", "tract", "precinct", "ward", "neighborhood", "grid", "city")

  if (to %in% areas_all == FALSE){

    stop(glue::glue("{to} is not a valid input."))

  }

  # logic check - is .data sf?
  if (gw_is_sf(.data) == FALSE){

    stop(glue::glue("Point data give are not stored an sf object. Transform them to sf before proceeding.)"))

  }

  # logic check - is .data in state plane meters?
  if (gw_get_epsg(.data) != 6512){

    .data <- sf::st_transform(.data, crs = 6512)
    message("Point data transformed to Missouri State Plane East NAD83 (2011) for spatial join.")

  }

  # load and convert areal features
  areal <- gw_load_areal(name = to, id_only = TRUE)

  # spatial join
  out <- sf::st_join(.data, areal)

  # change output
  if (class == "tibble"){
    sf::st_geometry(out) <- NULL
  }

  # move ID column
  if (side == "left"){
    out <- dplyr::select(out, ID, dplyr::everything())
  }

  # rename ID variable
  out <- gw_rename_id(out, name = to)

  # return
  return(out)

}
