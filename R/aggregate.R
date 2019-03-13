#' Aggregate Point Data to Selected Areal Units
#'
#' @description \code{gw_aggregate} aggregates points from a given \code{sf} object to one of
#'     six possible geographies: census block, block group, and tract; precinct and ward;
#'     neighborhood.
#'
#' @param .data A \code{sf} object
#' @param to The string name of an areal unit to aggregate to: block group, tract,
#'     precinct, ward, neighborhood, or city.
#' @param sf A logical scalar; if \code{TRUE}, returns an \code{sf} object. Otherwise returns
#'     a tibble.
#' @param replace.na A logical scalar; if \code{TRUE}, areal units that do not have any points
#'     enclosed in them with be given a value of \code{0}. If \code{FALSE}, they will be given
#'     a value of \code{NA}.
#' @param keep.na A logical scalar; if \code{TRUE}, a row with count of points that could not be
#'     joined to the areal unit will be returned. This occurs when points fall outside of all
#'     given features. If \code{FALSE}, no count of missing points is returned. This argument
#'     only returns an \code{NA} row if \code{sf = FALSE}.
#'
#' @return \code{gw_aggregate} returns a table or simple features object with the requested data.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom glue glue
#' @importFrom sf st_crs
#' @importFrom sf st_geometry
#' @importFrom sf st_join
#' @importFrom sf st_transform
#'
#' @export
gw_aggregate <- function(.data, to, sf = TRUE, replace.na = TRUE, keep.na = FALSE){

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

  # aggregate data
  out <- gw_aggregate_points(.data, to, sf = sf, replace.na = replace.na, keep.na = keep.na)

  # return output
  return(out)

}

# Aggregate Points
#
# @description Performs a spatial join to obtain counts of points within specified
#     areal units. This is done with a range of canned options representing
#     common scenarios in cleaning spatial data.
#
# @param .data A \code{sf} object
# @param to The string name of an areal unit to aggregate to: block group, tract,
#     precinct, ward, neighborhood, or municipality.
# @param sf A logical scalar; if \code{TRUE}, returns an \code{sf} object. Otherwise returns
#     a tibble.
# @param replace.na A logical scalar; if \code{TRUE}, areal units that do not have any points
#     enclosed in them with be given a value of \code{0}. If \code{FALSE}, they will be given
#     a value of \code{NA}.
# @param keep.na A logical scalar; if \code{TRUE}, a row with count of points that could not be
#     joined to the areal unit will be returned. This occurs when points fall outside of all
#     given features. If \code{FALSE}, no count of missing points is returned. This argument
#     only returns an \code{NA} row if \code{sf = FALSE}.
#
gw_aggregate_points <- function(.data, to, sf = TRUE, replace.na = TRUE, keep.na = FALSE){

  # no visible global binding
  ID = COUNT = NULL

  # load and convert areal features
  areal <- gw_load_areal(name = to)

  # spatial join
  join <- sf::st_join(.data, areal)
  sf::st_geometry(join) <- NULL

  # aggregate
  join %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(COUNT = dplyr::n()) -> join

  # optionally remove missing row
  if (keep.na == FALSE){

    join <- dplyr::filter(join, is.na(join$ID) == FALSE)

  }

  # combine with spatial data
  join <- dplyr::full_join(areal, join, by = "ID")

  # optionally replace missing values with a zero
  if (replace.na == TRUE) {

    join <- dplyr::mutate(join, COUNT = ifelse(is.na(COUNT) == TRUE, 0, COUNT))

  }

  # optionally choose not to return sf object
  if (sf == FALSE){

    # remove geometry
    sf::st_geometry(join) <- NULL

    # remove excess columns
    join <- dplyr::select(join, ID, COUNT)

  }

  # rename ID variable
  out <- gw_rename_id(join, name = to)

  # return
  return(out)

}

# Logic check for sf data
#
# @param .data An object to be tested
#
# @description Retuns a \code{TRUE} or \code{FALSE} value based on whether
#     the object is from class \code{sf} or not.
#
gw_is_sf <- function(.data){

  # store vector of object classes
  classes <- class(.data)

  # test whether sf is a class
  out <- "sf" %in% classes

  # return output
  return(out)

}

# Extract epsg value
#
# @description Returns the numeric epsg value from \code{sf::st_crs}.
#
# @param .data A \code{sf} object
#
gw_get_epsg <- function(.data){

  # store vector of object crs
  crs <- sf::st_crs(.data)

  # create output
  out <- crs$epsg

  # return output
  return(out)

}

# Load areal data
#
# @description Based on arguments supplied, areal data is loaded, re-projected,
#     and has its ID varaible renamed to a generic \code{ID} to facilitate
#     simpler code in \code{gw_aggregate_city} or \code{gw_aggregate_county}.
#
# @param name The name of the areal geometry to be loaded
#
gw_load_areal <- function(name){

  # no visible global binding
  GEOID = HANDLE = PageNumber = NULL

  if (name == "block group"){

    areal <- sf::st_transform(stl_blockgrps10, crs = 6512)
    areal <- dplyr::rename(areal, ID = GEOID)

  } else if (name == "tract"){

    areal <- sf::st_transform(stl_tracts10, crs = 6512)
    areal <- dplyr::rename(areal, ID = GEOID)

  } else if (name == "county"){

    areal <- sf::st_transform(stl_city, crs = 6512)
    areal <- dplyr::rename(areal, ID = GEOID)

  } else if (name == "neighborhood") {

    areal <- sf::st_transform(stl_nhoods, crs = 6512)

  } else if (name == "precinct") {

    areal <- sf::st_transform(stl_precincts10, crs = 6512)
    areal <- dplyr::rename(areal, ID = HANDLE)

  } else if (name == "ward") {

    areal <- sf::st_transform(stl_wards10, crs = 6512)

  }

  #else if (name == "grid"){

  #  gw_get_data(data = "Grids") %>%
  #    dplyr::rename(ID = PageNumber) %>%
  #    sf::st_transform(crs = 6512) -> areal

  #}

  return(areal)

}

# Rename areal ID variable
#
# @description Rename the ID variable after processing is done so that it
#     matches the origianl ID variable in the data.
#
# @param .data A \code{sf} object
# @param name The name of the areal geometry that has been loaded
#
gw_rename_id <- function(.data, name){

  # no visible global binding
  ID = NULL

  if (name == "block group" | name == "tract" | name == "county"){

    out <- dplyr::rename(.data, GEOID = ID)

  } else if (name == "neighborhood" | name == "ward"){

    out <- .data

  } else if (name == "precinct"){

    out <- dplyr::rename(.data, HANDLE = ID)

  } else if (name == "grid"){

    out <- dplyr::rename(.data, PageNumber = ID)

  }

  return(out)

}

