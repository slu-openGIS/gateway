#' Build Local Geocoder
#'
#' @description This constructs a local geocoder that is based on the latest release of the City of
#'    St. Louis's master address list. This function therefore requires an internet connection,
#'    and will error if your computer is offline. Since the actual geocoding is done with a second
#'    function, however, it is possible to build a geocoder and store it offline for repeated use.
#'
#' @usage gw_build_geocoder(class, crs = 4269, return = c("coords", "parcel", "zip"),
#'     include_units = FALSE)
#'
#' @param class One of either \code{"sf"} or \code{"tibble"}.
#' @param crs A numeric code corresponding to the desired coordinate system for the column output if
#'    \code{return} includes \code{"coords"} as well as the object output if \code{class} is \code{"sf"}.
#' @param return Optional; A character scalar or vector that describes the type of information to be applied
#'    to the target data. Options include the City's address identification numbers (\code{addrrecnum}),
#'    parcel identification numbers (\code{handle}), zip-codes, and x and y coordinates (in decimal degrees).
#' @param include_units A logical scalar; if \code{TRUE}, all individual records for apartment units will
#'    be included. If \code{FALSE} (default), only records for the overall building will be retained.
#'
#' @return A \code{sf} object or tibble with the requested data in the \code{return} argument as well as
#'    a \code{address} variable containing the full street address string.
#'
#' @seealso \code{\link{gw_geocode}}, \href{https://www.stlouis-mo.gov/data/geocode-service.cfm}{City of St. Louis Geocode Service}
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom postmastr pm_rebuild
#' @importFrom postmastr pm_street_std
#' @importFrom postmastr pm_streetSuf_std
#' @importFrom sf st_crs
#' @importFrom sf st_geometry
#' @importFrom sf st_transform
#'
#' @export
gw_build_geocoder <- function(class, crs = 4269, return = c("coords", "parcel", "zip"),
                              include_units = FALSE){

  # set global bindings
  ADDRRECNUM = HANDLE = HOUSENUM = HOUSESUF = PREDIR = STREETNAME = STREETTYPE =
    SUFDIR = UNITNUM = ZIP = pm.rebuilt = NULL

  # check for optional return argument
  if (missing(return)){
    return <- NULL
  }

  # obtain master list if none is provided
  master <- gw_get_data(data = "Addresses", class = "sf")

  # clean address data
  master <- dplyr::select(master, ADDRRECNUM, HANDLE:ZIP)

  # optionally filter out units
  if (include_units == FALSE){
    master <- dplyr::filter(master, is.na(UNITNUM) == TRUE)
  }

  # optionally drop unneeded data
  if ("parcel" %in% return == FALSE){
    master <- dplyr::select(master, -HANDLE)
  }

  if ("zip" %in% return == FALSE){
    master <- dplyr::select(master, -ZIP)
  }

  # store coordinates
  coords <- sf::st_crs(master)$epsg

  if (is.na(coords) == TRUE){
    coords <- 0
  }

  # create coordinates if class is tibble
  if (class == "tibble" & "coords" %in% return == TRUE){
    master <- gw_get_coords(master, crs = crs)
    sf::st_geometry(master) <- NULL
    master <- dplyr::as_tibble(master)
  } else if (class == "tibble" & "coords" %in% return == FALSE){
    sf::st_geometry(master) <- NULL
    master <- dplyr::as_tibble(master)
  } else if (class == "sf" & coords != crs){
    master <- sf::st_transform(master, crs = crs)
  }

  # clean-up data
  master %>%
    dplyr::mutate(UNITNUM = ifelse(HOUSESUF == "E", "E", UNITNUM)) %>%
    dplyr::mutate(HOUSESUF = ifelse(HOUSESUF == "E", NA, HOUSESUF)) %>%
    postmastr::pm_street_std(var = STREETNAME, locale = "us") %>%
    postmastr::pm_streetSuf_std(var = STREETTYPE, locale = "us") %>%
    postmastr::pm_rebuild(start = HOUSENUM, end = SUFDIR) %>%
    dplyr::select(-c(HOUSENUM, HOUSESUF, UNITNUM, PREDIR, STREETNAME, STREETTYPE, SUFDIR)) %>%
    dplyr::rename(
      addrrecnum = ADDRRECNUM,
      address = pm.rebuilt
    ) -> master

  # return output
  return(master)

}

#' Extract Coordinates from sf Object
#'
#' @description Converts point coordinates stored in an \code{sf} object to columns for
#'     both the x and y coordinates. Useful for storing spatial data in tabular form.
#'
#' @details Based on a function written \href{https://github.com/jmlondon}{Josh M. London} and
#'     described in a \href{https://github.com/r-spatial/sf/issues/231}{GitHub issue}.
#'
#' @usage gw_get_coords(.data, names = c("x","y"), crs = 4269)
#'
#' @param .data A \code{sf} object
#' @param names A vector with two column names, one for the x coordinate and one for the y coordinate.
#' @param crs A numeric code corresponding to the desired coordinate system for the column output
#'
#' @return An updated object with two new columns based on the names provided in the \code{names} argument.
#'
#' @importFrom dplyr as_tibble
#' @importFrom dplyr bind_cols
#' @importFrom sf st_crs
#' @importFrom sf st_transform
#' @importFrom stats setNames
#'
#' @export
gw_get_coords <- function(.data, names = c("x","y"), crs = 4269){

  # ensure .data is an sf object
  if ("sf" %in% class(.data) == FALSE){
    stop("An sf object must be used with 'gw_get_coords()'.")
  }

  # store coordinates
  coords <- sf::st_crs(.data)$epsg

  if (is.na(coords) == TRUE){
    coords <- 0
  }

  # reproject
  if (coords != crs){
    .data <- sf::st_transform(.data, crs = crs)
  }

  # create coordinate columns
  ret <- do.call(rbind,sf::st_geometry(.data))
  ret <- dplyr::as_tibble(ret)

  # ensure two columns are returned
  stopifnot(length(names) == ncol(ret))

  # name columns with coordinate data
  ret <- stats::setNames(ret, names)

  # combine coordinate data with source data
  out <- dplyr::bind_cols(.data, ret)

  # return output
  return(out)

}

#' Geocode Addresses
#'
#' @description Apply a previously build geocoder to target data. This function will
#'    apply whatever unique variables exist in the geocoder. See \code{\link{gw_build_geocoder}}
#'    for options.
#'
#' @usage gw_geocode(.data, type, class, address, geocoder, include_result = TRUE)
#'
#' @param .data A target data set
#' @param type Geocoder type; one of either \code{"local"}, \code{"city api"}, or \code{"census"}.
#' @param class Output class; one of either \code{"sf"} or \code{"tibble"}.
#' @param address Address variable in the target data set, which should contain the house number,
#'    street directionals, name, and suffix, and optionally unit types and numbers as well. Unit
#'    names should be replaced with \code{#} to match how \code{\link{gw_build_geocoder}}
#'    creates units.
#' @param geocoder Name of object containing a geocoder built with \code{\link{gw_build_geocoder}}
#' @param include_result Logical scalar; if \code{TRUE} (default), a column describing how each
#'    observation was geocoded is included in the output.
#'
#' @return A copy of the target data with georeferenced data applied to it.
#'
#' @seealso \code{\link{gw_build_geocoder}}
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#' @importFrom sf st_as_sf
#'
#' @export
gw_geocode <- function(.data, type, class, address, geocoder, include_result = TRUE){

  # set global bindings
  . = ...address = out = geometry = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  add <- paramList$address

  if (!is.character(paramList$address)) {
    varQ <- rlang::enquo(add)
  } else if (is.character(paramList$add)) {
    varQ <- rlang::quo(!! rlang::sym(add))
  }

  # ensure sf objects are converted to a-spatial data
  if ("sf" %in% class(.data)){
    sf::st_geometry(.data) <- NULL
  }

  # rename variables
  .data <- dplyr::rename(.data, ...address := !!varQ)

  # geocode
  if (type == "local"){
    .data <- gw_geocode_locale(.data, geocoder = geocoder)
  } else if (type == "city api"){
    stop("functionality not enabled")
  } else if (type == "census"){
    stop("functionality not enabled")
  }

  # rename variables again
  .data <- dplyr::rename(.data, !!varQ := ...address)

  # set-up output
  if (class == "sf"){
    .data <- sf::st_as_sf(.data)
  } else if (class == "tibble" & "geometry" %in% names(out) == TRUE){
    .data <- dplyr::select(.data, -geometry)
  }

  # return output
  return(.data)

}


# local geocoder
gw_geocode_locale <- function(.data, geocoder){

  # set global bindings
  address = NULL

  # identify observations
  .data <- gw_geocode_identify(.data)

  # subset distinct observations
  target <- gw_geocode_prep(.data)

  # rename geocoder address column
  geocoder <- dplyr::rename(geocoder, ...address = address)

  # geocode
  target <- dplyr::left_join(target, geocoder, by = "...address")

  # rebuild data
  .data <- gw_geocode_replace(source = .data, target = target)

}

# city api
gw_geocode_city_api <- function(.data){

}

# city api
gw_geocode_census_xy <- function(.data){

}

# identify data
gw_geocode_identify <- function(.data){

  # set global bindings
  . = ...address = NULL

  # add id numbers to each row
  full <- tibble::rowid_to_column(.data, var = "...id")

  # add unique id numbers for each address string
  full %>%
    dplyr::distinct(...address) %>%
    tibble::rowid_to_column(var = "...uid") %>%
    dplyr::left_join(full, ., by = "...address") -> .data

}

# prep data
gw_geocode_prep <- function(.data){

  # set global bindings
  ...uid = ...address = NULL

  # return only distinct addresses
  .data %>%
    dplyr::distinct(...uid, .keep_all = TRUE) %>%
    dplyr::select(...uid, ...address) -> .data

}

# replace data
gw_geocode_replace <- function(source, target){

  # set global bindings
  . = ...id = ...uid = ...address = NULL

  # join parsed and source data
  target %>%
    dplyr::select(-...address) %>%
    dplyr::left_join(source, ., by = "...uid") %>%
    dplyr::select(-...id, -...uid) -> out

}


