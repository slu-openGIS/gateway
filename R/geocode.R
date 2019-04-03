#' Build Local Geocoder
#'
#' @description This constructs a local geocoder that is based on the latest release of the City of
#'    St. Louis's master address list. This function therefore requires an internet connection,
#'    and will error if your computer is offline. Since the actual geocoding is done with a second
#'    function, however, it is possible to build a geocoder and store it offline for repeated use.
#'
#' @usage gw_build_geocoder(style, class, crs = 4269, return = c("coords", "parcel", "zip"),
#'     include_units = FALSE)
#'
#' @param style One of either \code{"full"} (\code{"123 Main St"}) or \code{"short"} (\code{"123 Main"}).
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
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom postmastr pm_street_std
#' @importFrom postmastr pm_streetSuf_std
#' @importFrom sf st_crs
#' @importFrom sf st_geometry
#' @importFrom sf st_transform
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish
#' @importFrom tidyr unite
#'
#' @export
gw_build_geocoder <- function(style, class, crs = 4269, return = c("coords", "parcel", "zip"),
                              include_units = FALSE){

  # set global bindings
  ADDRRECNUM = HANDLE = HOUSENUM = HOUSESUF = PREDIR = STREETNAME = STREETTYPE =
    SUFDIR = UNITNUM = ZIP = address = address_short = x = y = flag = NULL

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

  # create subset if class is sf
  if (class == "sf"){
    coords <- dplyr::select(master, ADDRRECNUM)
    sf::st_geometry(master) <- NULL
  }

  # clean-up data
  if (style == "full"){

    master %>%
      dplyr::mutate(UNITNUM = ifelse(HOUSESUF == "E", "E", UNITNUM)) %>%
      dplyr::mutate(HOUSESUF = ifelse(HOUSESUF == "E", NA, HOUSESUF)) %>%
      postmastr::pm_street_std(var = STREETNAME, locale = "us") %>%
      postmastr::pm_streetSuf_std(var = STREETTYPE, locale = "us") %>%
      tidyr::unite(address, HOUSENUM:SUFDIR, sep = " ", remove = TRUE) %>%
      dplyr::mutate(address = stringr::str_replace_all(address, pattern = "\\bNA\\b", replacement = "")) %>%
      dplyr::mutate(address = stringr::str_squish(address)) %>%
      dplyr::distinct(address, .keep_all = TRUE) -> master

  } else if (style == "short"){

    master %>%
      dplyr::mutate(UNITNUM = ifelse(HOUSESUF == "E", "E", UNITNUM)) %>%
      dplyr::mutate(HOUSESUF = ifelse(HOUSESUF == "E", NA, HOUSESUF)) %>%
      postmastr::pm_street_std(var = STREETNAME, locale = "us") %>%
      postmastr::pm_streetSuf_std(var = STREETTYPE, locale = "us") %>%
      tidyr::unite(address, HOUSENUM:SUFDIR, sep = " ", remove = FALSE) %>%
      dplyr::mutate(address = stringr::str_replace_all(address, pattern = "\\bNA\\b", replacement = "")) %>%
      dplyr::mutate(address = stringr::str_squish(address)) %>%
      tidyr::unite(address_short, HOUSENUM:STREETNAME, sep = " ", remove = TRUE) %>%
      dplyr::mutate(address_short = stringr::str_replace_all(address_short, pattern = "\\bNA\\b", replacement = "")) %>%
      dplyr::mutate(address_short = stringr::str_squish(address_short)) %>%
      dplyr::select(-STREETTYPE, -SUFDIR) %>%
      dplyr::distinct(address, .keep_all = TRUE) %>%
      dplyr::select(-address) -> master

    sub <- dplyr::distinct(master, address_short, x, y, .keep_all = TRUE)

    sub %>%
      janitor::get_dupes(address_short) %>%
      dplyr::distinct(address_short) %>%
      dplyr::mutate(flag = TRUE) -> dupes

    master <- dplyr::left_join(sub, dupes, by = "address_short")
    master <- dplyr::filter(master, is.na(flag) == TRUE)
    master <- dplyr::select(master, -flag)

  }

  # combine coordinates and cleaned data
  if (class == "sf"){
    master <- dplyr::left_join(coords, master, by = "ADDRRECNUM")
  }

  # rename id
  master <- dplyr::rename(master, addrrecnum = ADDRRECNUM)



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

  # global bindings
  geometry = NULL

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
  dplyr::bind_cols(.data, ret) %>%
    dplyr::select(-geometry, dplyr::everything()) -> out

  # return output
  return(out)

}

#' Geocode Addresses
#'
#' @description Apply a previously build geocoder to target data. This function will
#'    apply whatever unique variables exist in the geocoder. See \code{\link{gw_build_geocoder}}
#'    for options.
#'
#' @usage gw_geocode(.data, type, var, class, side = "right", geocoder, include_source = TRUE)
#'
#' @param .data A target data set
#' @param type Geocoder type; one of either \code{"local"}, \code{"local short"}, \code{"city batch"},
#'    \code{"city candidate"}, \code{"census"}, or \code{"osm"}.
#' @param var Address variable in the target data set, which should contain the house number,
#'    street directionals, name, and suffix, and optionally unit types and numbers as well. Unit
#'    names should be replaced with \code{#} to match how \code{\link{gw_build_geocoder}}
#'    creates units.
#' @param class Output class; one of either \code{"sf"} or \code{"tibble"}.
#' @param side One of either \code{"right"} or \code{"left"} indicating where the identifier variable
#'     should be placed in the
#' @param geocoder Name of object containing a geocoder built with \code{\link{gw_build_geocoder}}
#' @param include_source Logical scalar; if \code{TRUE} (default), a column describing how each
#'    observation was geocoded is included in the output.
#'
#' @return A copy of the target data with georeferenced data applied to it.
#'
#' @seealso \code{\link{gw_build_geocoder}}
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#' @importFrom sf st_as_sf
#' @importFrom tmaptools geocode_OSM
#'
#' @export
gw_geocode <- function(.data, type, var, class, side = "right", geocoder, include_source = TRUE){

  # set global bindings
  . = ...address = out = addrrecnum = geometry = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  # add <- paramList$address

  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # ensure sf objects are converted to a-spatial data
  if ("sf" %in% class(.data)){
    sf::st_geometry(.data) <- NULL
  }

  # rename variables
  .data <- dplyr::rename(.data, ...address := !!varQ)

  # geocode
  if (type == "local"){
    .data <- gw_geocode_local(.data, class = class, geocoder = geocoder, side = side)
  } else if (type == "local short"){
    .data <- gw_geocode_local_short(.data, class = class, geocoder = geocoder, side = side)
  } else if (type == "city batch"){
    stop("functionality not enabled")
  } else if (type == "city candidate"){
    .data <- gw_geocode_city_candidate(.data)
  } else if (type == "census"){
    stop("functionality not enabled")
  } else if (type == "osm"){
    .data <- gw_geocode_osm(.data)
  }

  # rename variables again
  .data <- dplyr::rename(.data, !!varQ := ...address)

  # optionally remove source
  if (include_source == FALSE){
    .data <- dplyr::select(.data, -source)
  }

  # return output
  return(.data)

}


# local geocoder
gw_geocode_local <- function(.data, class, geocoder, side = "right"){

  # set global bindings
  address = addrrecnum = geometry = out = NULL

  # identify observations
  .data <- gw_geocode_identify(.data)

  # subset distinct observations
  target <- gw_geocode_prep(.data)

  # rename geocoder address column
  geocoder <- dplyr::rename(geocoder, ...address = address)

  # geocode
  target <- dplyr::left_join(target, geocoder, by = "...address")

  # include result
  target <- dplyr::mutate(target, source = ifelse(is.na(addrrecnum) == FALSE, "local geocoder", NA))

  # rebuild data
  .data <- gw_geocode_replace(source = .data, target = target)

  # move ID column
  if (side == "left"){
    .data <- dplyr::select(.data, addrrecnum, dplyr::everything())
  }

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
gw_geocode_local_short <- function(.data, class, geocoder, side = "right"){

  # set global bindings
  address = addrrecnum = geometry = out = address_short = NULL

  # identify observations
  .data <- gw_geocode_identify(.data)

  # subset distinct observations
  target <- gw_geocode_prep(.data)

  # rename geocoder address column
  geocoder <- dplyr::rename(geocoder, ...address = address_short)

  # geocode
  target <- dplyr::left_join(target, geocoder, by = "...address")

  # include result
  target <- dplyr::mutate(target, source = ifelse(is.na(addrrecnum) == FALSE, "local geocoder, short", NA))

  # rebuild data
  .data <- gw_geocode_replace(source = .data, target = target)

  # move ID column
  if (side == "left"){
    .data <- dplyr::select(.data, addrrecnum, dplyr::everything())
  }

  # set-up output
  if (class == "sf"){
    .data <- sf::st_as_sf(.data)
  } else if (class == "tibble" & "geometry" %in% names(out) == TRUE){
    .data <- dplyr::select(.data, -geometry)
  }

  # return output
  return(.data)

}

# city api, batch geocoder
gw_geocode_city_batch <- function(.data){

}

# city api, candidate geocoder
gw_geocode_city_candidate <- function(.data){

  # global bindings
  ...address = geo = NULL

  # identify observations
  .data <- gw_geocode_identify(.data)

  # subset distinct observations
  target <- gw_geocode_prep(.data)

  # generate candidates
  target <- dplyr::mutate(target, geo = purrr::map(...address, ~ gw_create_candidates(address = .x, style = "top")))

  # remove NAs
  target <- dplyr::filter(target, is.na(geo) == FALSE)

  # unnest results
  target <- tidyr::unnest(target)

  # include result
  target <- dplyr::mutate(target, source = "city candidate")

  # rebuild data
  target <- gw_geocode_replace(source = .data, target = target)

  # return output
  return(target)

}

# census xy
gw_geocode_census_xy <- function(.data){

}

# open street map
gw_geocode_osm <- function(.data){

  # set global bindings
  addrrecnum = ...address = ...address2 = lat = lon = query = NULL

  # identify observations
  id <- gw_geocode_identify(.data)

  # subset distinct observations
  target <- gw_geocode_prep(id)

  # make copy of address
  target <- dplyr::mutate(target, ...address2 = stringr::str_c(...address, ", St. Louis"))

  # geocode
  result <- tryCatch(
    {suppressWarnings(tmaptools::geocode_OSM(target$...address2, as.data.frame = TRUE))},
    error=function(cond) {
      return(NULL)
    })

  # clean-up results
  if (is.null(result) == FALSE){

    # subset results
    result <- dplyr::select(result, query, lon, lat)

    # rename variables
    result <- dplyr::rename(result,
      ...address2 = query,
      x = lon,
      y = lat
    )

    # add source
    result <- dplyr::mutate(result, source = "open street map")

    # combine result and target data
    target <- dplyr::left_join(target, result, by = "...address2")

    # remove ...address2
    target <- dplyr::select(target, -...address2)

    # rebuild data
    out <- gw_geocode_replace(source = id, target = target)

  } else if (is.null(result) == TRUE){

    out <- .data

  }

  # return result
  return(out)

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

gw_create_candidates <- function(address, style){

  if (style == "top"){

    api_result <- gw_add_candidates(address = address, n = 1)

  } else if (style == "all"){

    api_result <- gw_add_candidates(address = address)
    api_result <- tibble::rowid_to_column(api_result, var = "result_id")

  }

  return(api_result)

}

#' Composite Geocoder
#'
#' @description An algorithm for processing and geocoding address data. A first
#'     attempt is made to match again a local geocoder. Unmatched addresses are then
#'     matched against a short geocoder. AAddress that remain unmatched are then
#'     matched using the City of St. Louis's address candidate API.
#'
#' @param .data A data frame or tibble to be geocoded
#' @param var Column with address data to be geocoded
#' @param local_geocoder Object with local geocoder data
#' @param short_geocoder Object with short version of local geocoder data
#' @param local A logical scalar; if \code{TRUE}, only local geocoders will be used.
#'     If \code{FALSE}, data unmatched with local geocoders will be passed to APIs.
#'
#' @export
gw_geocode_composite <- function(.data, var, local_geocoder, short_geocoder, local = TRUE){

  # global bindings
  ...gw.id = x = y = unmatched = addrrecnum = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  # add <- paramList$address

  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # add id
  .data <- tibble::rowid_to_column(.data, var = "...gw.id")

  # use local geocoder
  .data <- gw_geocode(.data, type = "local", var = !!varQ, class = "tibble", geocoder = local_geocoder)

  # check results
  result <- any(is.na(.data$x))

  # short geocoder
  if (result == TRUE){

    # subset results
    matched <- dplyr::filter(.data, is.na(x) == FALSE)
    unmatched <- dplyr::filter(.data, is.na(x) == TRUE)
    unmatched <- dplyr::select(unmatched, -addrrecnum, -x, -y, -source)

    # use short geocoder
    initial <- gw_geocode(unmatched, type = "local short", var = !!varQ, class = "tibble", geocoder = short_geocoder)

    # check results
    result2 <- any(is.na(initial$x))

    if (local == TRUE | result2 == FALSE){

      # combine
      initial <- dplyr::bind_rows(matched, initial)
      initial <- dplyr::arrange(initial, ...gw.id)

    } else if (local == FALSE & result2 == TRUE){

      # subset results
      matched2 <- dplyr::filter(initial, is.na(x) == FALSE)
      unmatched <- dplyr::filter(initial, is.na(x) == TRUE)
      unmatched <- dplyr::select(unmatched, -addrrecnum, -x, -y, -source)

      # combine
      matched <- dplyr::bind_rows(matched, matched2)
      matched <- dplyr::arrange(matched, ...gw.id)

      # use candidate geocoder
      initial <- gw_geocode(unmatched, type = "city candidate", class = "tibble", var = !!varQ)

      # check results
      result3 <- any(is.na(initial$x))

      if (result3 == FALSE){

        # combine
        initial <- dplyr::bind_rows(matched, initial)
        initial <- dplyr::arrange(initial, ...gw.id)

      } else if (result3 == TRUE){

        # subset results
        matched2 <- dplyr::filter(initial, is.na(x) == FALSE)
        unmatched <- dplyr::filter(initial, is.na(x) == TRUE)
        unmatched <- dplyr::select(unmatched, -addrrecnum, -x, -y, -source)

        # combine
        matched <- dplyr::bind_rows(matched, matched2)
        matched <- dplyr::arrange(matched, ...gw.id)

      }

    }
  }

  initial <- dplyr::select(initial, -...gw.id)
  return(initial)

}

