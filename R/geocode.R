#' Build Local Geocoder
#'
#' @description This constructs a local geocoder that is based on the latest release of the City of
#'    St. Louis's master address list. This function therefore requires an internet connection,
#'    and will error if your computer is offline. Since the actual geocoding is done with a second
#'    function, however, it is possible to build a geocoder and store it offline for repeated use.
#'
#' @param return A character scalar or vector that describes the type of information to be applied
#'    to the target data. Options include the City's address identification numbers (\code{addrrecnum}),
#'    parcel identification numbers (\code{handle}), zip-codes, and x and y coordinates (in decimal degrees).
#' @param class One of either \code{"sf"} or \code{"tibble"}.
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
#' @importFrom dplyr bind_cols
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom postmastr pm_rebuild
#' @importFrom postmastr pm_street_std
#' @importFrom postmastr pm_streetSuf_std
#' @importFrom sf st_geometry
#' @importFrom sf st_transform
#' @importFrom stats setNames
#'
#' @export
gw_build_geocoder <- function(return = c("id", "coords", "parcel", "zip"), class, include_units = FALSE){

  # set global bindings
  ADDRRECNUM = HANDLE = HOUSENUM = HOUSESUF = PREDIR = STREETNAME = STREETTYPE = SUFDIR = UNITNUM = ZIP = pm.rebuilt = NULL

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

  # re-project
  master <- sf::st_transform(master, crs = 4268)

  # create coordinates if class is tibble
  if (class == "tibble" & "coords" %in% return == TRUE){
    master <- gw_coords_as_cols(master)
    sf::st_geometry(master) <- NULL
    master <- dplyr::as_tibble(master)
  } else if (class == "tibble" & "coords" %in% return == FALSE){
    sf::st_geometry(master) <- NULL
    master <- dplyr::as_tibble(master)
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


gw_coords_as_cols <- function(x, names = c("x","y")) {
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- dplyr::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# https://github.com/r-spatial/sf/issues/231


#' Geocode Addresses
#'
#' @description Apply a previously build geocoder to target data. This function will
#'    apply whatever unique variables exist in the geocoder. See \code{\link{gw_build_geocoder}}
#'    for options.
#'
#' @param .data A target data set
#' @param address Address variable in the target data set, which should contain the house number,
#'    street directionals, name, and suffix, and optionally unit types and numbers as well. Unit
#'    names should be replaced with \code{#} to match how \code{\link{gw_build_geocoder}}
#'    creates units.
#' @param geocoder Name of object containing a geocoder built with \code{\link{gw_build_geocoder}}
#'
#' @return A copy of the target data with georeferenced data applied to it.
#'
#' @seealso \code{\link{gw_build_geocoder}}
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
#' @export
gw_geocode <- function(.data, address, geocoder){

  # set global bindings
  . = ...address = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  add <- paramList$address

  if (!is.character(paramList$address)) {
    varQ <- rlang::enquo(add)
  } else if (is.character(paramList$add)) {
    varQ <- rlang::quo(!! rlang::sym(add))
  }

  # rename variables
  .data <- dplyr::rename(.data, ...address := !!varQ)
  geocoder <- dplyr::rename(geocoder, ...address = address)

  # geocode
  .data %>%
    dplyr::left_join(., geocoder, by = "...address") %>%
    dplyr::rename(!!varQ := ...address) -> out

  # return output
  return(out)

}
