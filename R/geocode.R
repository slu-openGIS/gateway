#' Build Local Geocoder
#'
#' @description This constructs a local geocoder that is based on the latest release of the City of
#'    St. Louis's master address list. This function therefore requires an internet connection,
#'    and will error if your computer is offline. Since the actual geocoding is done with a second
#'    function, however, it is possible to build a geocoder and store it offline for repeated use.
#'
#' @usage gw_build_geocoder(style, crs = 4269, return = c("coords", "parcel", "zip"),
#'     include_units = FALSE, appendix)
#'
#' @param style One of either \code{"full"} (\code{"123 Main St"}) or \code{"short"} (\code{"123 Main"}).
#' @param crs A numeric code corresponding to the desired coordinate system for the column output if
#'    \code{return} includes \code{"coords"} as well as the object output if \code{class} is \code{"sf"}.
#' @param return Optional; A character scalar or vector that describes the type of information to be applied
#'    to the target data. Options include the City's address identification numbers (\code{addrrecnum}),
#'    parcel identification numbers (\code{handle}), zip-codes, and x and y coordinates (in decimal degrees).
#' @param include_units A logical scalar; if \code{TRUE}, all individual records for apartment units will
#'    be included. If \code{FALSE} (default), only records for the overall building will be retained.
#' @param appendix An object with additional address or placename data to append to the geocoder.
#'
#' @return A tibble with the requested data in the \code{return} argument as well as
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
gw_build_geocoder <- function(style, crs = 4269, return = c("coords", "parcel", "zip"),
                              include_units = FALSE, appendix){

  # set global bindings
  ADDRRECNUM = HANDLE = HOUSENUM = HOUSESUF = PREDIR = STREETNAME = STREETTYPE =
    SUFDIR = UNITNUM = ZIP = address = address_short = x = y = flag = NULL

  # check for optional return argument
  if (missing(return)){
    return <- NULL
  }

  if (style == "full" | style == "short"){

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
    if ("coords" %in% return == TRUE){
      master <- gw_get_coords(master, crs = crs)
      sf::st_geometry(master) <- NULL
      master <- dplyr::as_tibble(master)
    } else if ("coords" %in% return == FALSE){
      sf::st_geometry(master) <- NULL
      master <- dplyr::as_tibble(master)
    }

    # clean-up data
    if (style == "full"){

      master <- dplyr::mutate(master, UNITNUM = ifelse(HOUSESUF == "E", "E", UNITNUM))
      master <- dplyr::mutate(master, HOUSESUF = ifelse(HOUSESUF == "E", NA, HOUSESUF))
      master <- postmastr::pm_street_std(master, var = STREETNAME, locale = "us")
      master <- postmastr::pm_streetSuf_std(master, var = STREETTYPE, locale = "us")
      master <- tidyr::unite(master, address, HOUSENUM:SUFDIR, sep = " ", remove = TRUE)
      master <- dplyr::mutate(master,
                              address = stringr::str_replace_all(address, pattern = "\\bNA\\b", replacement = ""))
      master <- dplyr::mutate(master, address = stringr::str_squish(address))
      master <- dplyr::distinct(master, address, .keep_all = TRUE)

      # rename variables
      master <- dplyr::rename(master,
                              gw_addrrecnum = ADDRRECNUM,
                              gw_address = address,
                              gw_x = x,
                              gw_y = y)

    } else if (style == "short"){

      master <- dplyr::mutate(master, UNITNUM = ifelse(HOUSESUF == "E", "E", UNITNUM))
      master <- dplyr::mutate(master, HOUSESUF = ifelse(HOUSESUF == "E", NA, HOUSESUF))
      master <- postmastr::pm_street_std(master, var = STREETNAME, locale = "us")
      master <- postmastr::pm_streetSuf_std(master, var = STREETTYPE, locale = "us")
      master <- tidyr::unite(master, address, HOUSENUM:SUFDIR, sep = " ", remove = FALSE)
      master <- dplyr::mutate(master,
                              address = stringr::str_replace_all(address, pattern = "\\bNA\\b", replacement = ""))
      master <- dplyr::mutate(master, address = stringr::str_squish(address))
      master <- tidyr::unite(master, address_short, HOUSENUM:STREETNAME, sep = " ", remove = TRUE)
      master <- dplyr::mutate(master,
                              address_short = stringr::str_replace_all(address_short, pattern = "\\bNA\\b", replacement = ""))
      master <- dplyr::mutate(master, address_short = stringr::str_squish(address_short))
      master <- dplyr::select(master, -STREETTYPE, -SUFDIR)
      master <- dplyr::distinct(master, address, .keep_all = TRUE)
      master <- dplyr::select(master, -address)

      sub <- dplyr::distinct(master, address_short, x, y, .keep_all = TRUE)

      dupes <- janitor::get_dupes(sub, address_short)
      dupes <- dplyr::distinct(dupes, address_short)
      dupes <- dplyr::mutate(dupes, flag = TRUE)

      master <- dplyr::left_join(sub, dupes, by = "address_short")
      master <- dplyr::filter(master, is.na(flag) == TRUE)
      master <- dplyr::select(master, -flag)

      # rename variables
      master <- dplyr::rename(master,
                              gw_addrrecnum = ADDRRECNUM,
                              gw_address = address_short,
                              gw_x = x,
                              gw_y = y)

    }

  } else if (style == "placename"){

    # obtain master list
    master <- gw_get_data(data = "Placenames", class = "sf")

    # remove geonameid
    master <- dplyr::select(master, -geonameid)

    # optionally drop unneeded data
    # if ("parcel" %in% return == FALSE){
    #  master <- dplyr::select(master, -HANDLE)
    #}

    if ("zip" %in% return == FALSE){
      master <- dplyr::select(master, -zip)
    }

    # store coordinates
    coords <- sf::st_crs(master)$epsg

    if (is.na(coords) == TRUE){
      coords <- 0
    }

    # create coordinates if class is tibble
    if ("coords" %in% return == TRUE){
      master <- gw_get_coords(master, crs = crs)
      sf::st_geometry(master) <- NULL
      master <- dplyr::as_tibble(master)
    } else if ("coords" %in% return == FALSE){
      sf::st_geometry(master) <- NULL
      master <- dplyr::as_tibble(master)
    }

    # rename variables
    master <- dplyr::rename(master,
                            gw_id = id,
                            gw_name = name,
                            gw_addrrecnum = addrrecnum,
                            gw_address = address,
                            gw_x = x,
                            gw_y = y)

    # convert addrrecnum to character
    master <- dplyr::mutate(master, gw_addrrecnum = as.character(gw_addrrecnum))

  }

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
#' @usage gw_geocode(.data, type, var, class, side = "right", geocoder, threshold,
#'     include_source = TRUE)
#'
#' @param .data A target data set
#' @param type Geocoder type; one of either \code{"local"}, \code{"local short"}, \code{"city batch"},
#'    \code{"city candidate"}, \code{"census"}, or \code{"osm"}. Composite geocoders consisting of
#'    only local geocoders (\code{"local"}, \code{"local short"}, and \code{"local placename"}) or
#'    both local and the remote geocoders are also available by using \code{"composite, local"} or
#'    \code{"composite, full"} respectively.
#' @param var Address variable in the target data set, which should contain the house number,
#'    street directionals, name, and suffix, and optionally unit types and numbers as well. Unit
#'    names should be replaced with \code{#} to match how \code{\link{gw_build_geocoder}}
#'    creates units.
#' @param class Output class; one of either \code{"sf"} or \code{"tibble"}.
#' @param side One of either \code{"right"} or \code{"left"} indicating where the identifier variable
#'     should be placed in the
#' @param geocoder Name of object containing a geocoder built with \code{\link{gw_build_geocoder}}
#' @param threshold For the city candidate geocoder, what score is the minimum acceptable?
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
gw_geocode <- function(.data, type, var, zip, class, local, local_short, local_place,
                       threshold, side = "right", include_source = TRUE){

  # set global bindings
  . = ...address = out = addrrecnum = geometry = NULL

  # set global variables
  batch <- FALSE

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  if (missing(zip) == FALSE){
    if (!is.character(paramList$zip)) {
      zipQ <- rlang::enquo(zip)
    } else if (is.character(paramList$zip)) {
      zipQ <- rlang::quo(!! rlang::sym(zip))
    }
  }

  # ensure sf objects are converted to a-spatial data
  if ("sf" %in% class(.data)){
    sf::st_geometry(.data) <- NULL
  }

  # rename variables
  .data <- dplyr::rename(.data, ...address := !!varQ)

  if (missing(zip) == FALSE){
    .data <- dplyr::rename(.data, ...zip := !!zipQ)
    zipPresent <- TRUE
  } else if (missing(zip) == TRUE){
    zipPresent <- FALSE
  }

  # identify observations
  .data <- gw_geocode_identify(.data)

  # subset distinct observations
  target <- gw_geocode_prep(.data, zip = zipPresent)

  # geocode
  if (type == "local"){
    target <- gw_geocode_local(target, geocoder = local)
  } else if (type == "local short"){
    target <- gw_geocode_local_short(target, geocoder = local_short)
  } else if (type == "local placename"){
    target <- gw_geocode_local_placename(target, geocoder = local_place)
  } else if (type == "city batch"){
    batch <- TRUE
    target <- gw_geocode_city_batch(target, crs = 4269)
  } else if (type == "city candidate"){
    target <- gw_geocode_city_candidate(target, threshold = threshold)
  } else if (type == "census"){
    stop("Functionality not currently enabled!")
    target <- gw_geocode_census_xy(target, zip = zipPresent)
  } else if (type == "composite, local"){
    target <- gw_geocode_composite(target, zip = zipPresent, local = local,
                                   local_short = local_short, local_place = local_place,
                                   threshold = threshold, offline = TRUE)
  } else if (type == "composite, full"){
    target <- gw_geocode_composite(target, zip = zipPresent, local = local,
                                   local_short = local_short, local_place = local_place,
                                   threshold = threshold, offline = FALSE)
  }

  # rebuild data
  .data <- gw_geocode_replace(source = .data, target = target, zip = zipPresent, batch = batch)

  # move ID column
  if (type %in% c("local", "local short", "composite, local", "composite, full") & side == "left"){
    .data <- dplyr::select(.data, addrrecnum, dplyr::everything())
  }

  # set-up output
  if (class == "sf"){
    .data <- sf::st_as_sf(.data, coords = c("gw_x", "gw_y"), crs = 4269)
  }

  # rename variables again
  .data <- dplyr::rename(.data, !!varQ := ...address)

  if (missing(zip) == FALSE){
    .data <- dplyr::rename(.data, !!zipQ := ...zip)
  }

  # change capitalization
  if ("gw_address" %in% names(.data)){
    .data <- dplyr::mutate(.data, gw_address = stringr::str_to_title(gw_address, locale = "en"))
  }

  # optionally remove source
  if (include_source == FALSE){
    .data <- dplyr::select(.data, -source)
  }

  # return output
  return(.data)

}


# local geocoder
gw_geocode_local <- function(.data, geocoder){

  # set global bindings
  # address = addrrecnum = geometry = out = NULL

  # rename geocoder address column
  geocoder <- dplyr::rename(geocoder, ...address = gw_address)

  # geocode
  .data <- dplyr::left_join(.data, geocoder, by = "...address")

  # include result
  .data <- dplyr::mutate(.data, gw_source = ifelse(is.na(gw_x) == FALSE, "local", NA))

  # return output
  return(.data)

}

# local geocoder
gw_geocode_local_short <- function(.data, geocoder){

  # set global bindings
  # address = addrrecnum = geometry = out = NULL

  # rename geocoder address column
  geocoder <- dplyr::rename(geocoder, ...address = gw_address)

  # geocode
  .data <- dplyr::left_join(.data, geocoder, by = "...address")

  # include result
  .data <- dplyr::mutate(.data, gw_source = ifelse(is.na(gw_x) == FALSE, "local, short", NA))

  # return output
  return(.data)

}

# placename geocoder
gw_geocode_local_placename <- function(.data, geocoder){

  # rename geocoder address column
  geocoder <- dplyr::rename(geocoder, ...address = gw_name)

  # geocode
  .data <- dplyr::left_join(.data, geocoder, by = "...address")

  # include result
  .data <- dplyr::mutate(.data, gw_source = ifelse(is.na(gw_x) == FALSE, "local, placename", NA))

  # return output
  return(.data)

}

# city api, batch geocoder
gw_geocode_city_batch <- function(.data, crs){

  # global bindings
  # ...uid = result_id = address = x = y = address_match = score = NULL

  # geocode
  target <- gw_add_batch(.data, id = "...uid", address = "...address", threshold = 100, vars = "minimal", crs = crs)
  target <- dplyr::rename(target,
                          gw_x = x,
                          gw_y = y,
                          gw_address = address,
                          gw_score = score)
  target <- dplyr::select(target, ...uid, gw_x, gw_y, gw_address, gw_score)

  # include result
  target <- dplyr::mutate(target, gw_source = "city api, batch")

  # return output
  return(target)

}

# city api, candidate geocoder
gw_geocode_city_candidate <- function(.data, threshold){

  # global bindings
  # ...address = geo = NULL

  # generate candidates
  target <- dplyr::mutate(.data, geo = purrr::map(...address, ~
              gw_create_candidates(address = .x, style = "top", threshold = threshold)))

  # remove NAs
  target <- dplyr::filter(target, is.na(geo) == FALSE)

  # unnest results
  target <- tidyr::unnest(target)

  # rename
  target <- dplyr::rename(target,
                          gw_x = x,
                          gw_y = y,
                          gw_address = address_match,
                          gw_score = score)

  # include result
  target <- dplyr::mutate(target, gw_source = "city api, candidate")

  # return output
  return(target)

}

gw_create_candidates <- function(address, style, threshold){

  if (style == "top"){

    api_result <- gw_add_candidates(address = address, n = 1, threshold = threshold)

  } else if (style == "all"){

    api_result <- gw_add_candidates(address = address)
    api_result <- tibble::rowid_to_column(api_result, var = "result_id")

  }

  return(api_result)

}

# census xy
gw_geocode_census_xy <- function(.data, zip){

  # construct data
  if (zip == TRUE){
    .data <- dplyr::mutate(.data,
                           ...city = "St. Louis",
                           ...state = "MO"
    )
  } else if (zip == FALSE){
    .data <- dplyr::mutate(.data,
                           ...city = "St. Louis",
                           ...state = "MO",
                           ...zip = NA
    )
  }

  # geocode
  .data <- censusxy::cxy_geocode(.data, address = "...address", city = "...city",
                                 state  = "...state", zip = "....zip",
                                 style = "minimal", output = "tibble", timeout = 30)

  # rename
  .data <- dplyr::rename(.data,
                         gw_x = lon,
                         gw_y = lat,
                         gw_address = cxy_match,
                         gw_score = cxy_quality)

  # remove status
  .data <- dplyr::select(.data, -cxy_status)

  # return output
  return(.data)

}

gw_geocode_composite <- function(.data, zip, local, local_short, local_place, threshold, offline){

  # local geocoder
  .data <- gw_geocode_local(.data, geocoder = local)

  # check results
  result <- any(is.na(.data$gw_x))

  # short geocoder
  if (result == TRUE){

    # subset results
    matched <- dplyr::filter(.data, is.na(gw_x) == FALSE)
    unmatched <- dplyr::filter(.data, is.na(gw_x) == TRUE)
    unmatched <- dplyr::select(unmatched, -gw_addrrecnum, -gw_x, -gw_y, -gw_source)

    # geocode
    unmatched <- gw_geocode_local_short(unmatched, geocoder = local_short)

    # check results
    result2 <- any(is.na(unmatched$gw_x))

    # placename geocoder
    if (result2 == TRUE){

      # rebuild results
      .data <- dplyr::bind_rows(matched, unmatched)

      # subset results
      matched <- dplyr::filter(.data, is.na(gw_x) == FALSE)
      unmatched <- dplyr::filter(.data, is.na(gw_x) == TRUE)
      unmatched <- dplyr::select(unmatched, -gw_addrrecnum, -gw_x, -gw_y, -gw_source)

      # geocode
      unmatched <- gw_geocode_local_placename(unmatched, geocoder = local_place)

      # check results
      result2 <- any(is.na(unmatched$gw_x))

      # city batch geocoder
      if (result2 == TRUE & offline == FALSE){

        # rebuild results
        .data <- dplyr::bind_rows(matched, unmatched)

        # subset results
        matched <- dplyr::filter(.data, is.na(gw_x) == FALSE)
        unmatched <- dplyr::filter(.data, is.na(gw_x) == TRUE)
        unmatched <- dplyr::select(unmatched, -gw_addrrecnum, -gw_x, -gw_y, -gw_source, -gw_id, -gw_address)

        # geocode
        results <- gw_geocode_city_batch(unmatched, crs = 4269)

        # put results back into unmatched data
        unmatched <- dplyr::left_join(unmatched, results, by = "...uid")

        # check results
        result2 <- any(is.na(unmatched$gw_x))

        # city candidate geocoder
        if (result2 == TRUE){

          # rebuild results
          .data <- dplyr::bind_rows(matched, unmatched)

          # subset results
          matched <- dplyr::filter(.data, is.na(gw_x) == FALSE)
          unmatched <- dplyr::filter(.data, is.na(gw_x) == TRUE)
          unmatched <- dplyr::select(unmatched, -gw_addrrecnum, -gw_x, -gw_y, -gw_source, -gw_id, -gw_address, -gw_score)

          # geocode
          results <- gw_geocode_city_candidate(unmatched, threshold = threshold)

          # put results back into unmatched data
          if (zip == TRUE){
            results <- dplyr::select(results, -...address, -...zip)
          } else if (zip == FALSE){
            results <- dplyr::select(results, -...address)
          }

          unmatched <- dplyr::left_join(unmatched, results, by = "...uid")

        }

      }
    }

    # re-construct data
    .data <- dplyr::bind_rows(matched, unmatched)

  }

  # return output
  return(.data)

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
gw_geocode_prep <- function(.data, zip){

  # set global bindings
  ...uid = ...address = NULL

  # return only distinct addresses
  .data <- dplyr::distinct(.data, ...uid, .keep_all = TRUE)

  # subset cols
  if (zip == FALSE){
    .data <- dplyr::select(.data, ...uid, ...address)
  } else if (zip == TRUE){
    .data <- dplyr::select(.data, ...uid, ...address, ...zip)
  }

}

# replace data
gw_geocode_replace <- function(source, target, zip, batch = FALSE){

  # set global bindings
  . = ...id = ...uid = ...address = NULL

  # optionally prepare
  if (batch == FALSE){
    target <- dplyr::select(target, -...address)
  }

  # prepare zip
  if (zip == TRUE & batch == FALSE){
    target <- dplyr::select(target, -...zip)
  }

  # join parsed and source data
  out <- dplyr::left_join(source, target, by = "...uid")
  out <- dplyr::select(out, -...id, -...uid)

}


#' Additional Geocoder
#' @export
gw_geocode2 <- function(.data, type, var, zip, class, local, local_short, local_place,
                       threshold, side = "right", include_source = TRUE){

  # set global bindings
  . = ...address = out = addrrecnum = geometry = NULL

  # set global variables
  batch <- FALSE

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  if (missing(zip) == FALSE){
    if (!is.character(paramList$zip)) {
      zipQ <- rlang::enquo(zip)
    } else if (is.character(paramList$zip)) {
      zipQ <- rlang::quo(!! rlang::sym(zip))
    }
  }

  # ensure sf objects are converted to a-spatial data
  if ("sf" %in% class(.data)){
    sf::st_geometry(.data) <- NULL
  }

  # rename variables
  .data <- dplyr::rename(.data, ...address := !!varQ)

  if (missing(zip) == FALSE){
    .data <- dplyr::rename(.data, ...zip := !!zipQ)
    zipPresent <- TRUE
  } else if (missing(zip) == TRUE){
    zipPresent <- FALSE
  }

  # identify observations
  .data <- gw_geocode_identify(.data)

  # subset distinct observations
  target <- gw_geocode_prep(.data, zip = zipPresent)

  # geocode
  if (type == "local"){
    target <- gw_geocode_local(target, geocoder = local)
  } else if (type == "local short"){
    target <- gw_geocode_local_short(target, geocoder = local_short)
  } else if (type == "local placename"){
    target <- gw_geocode_local_placename(target, geocoder = local_place)
  } else if (type == "city batch"){
    batch <- TRUE
    target <- gw_geocode_city_batch(target, crs = 4269)
  } else if (type == "city candidate"){
    target <- gw_geocode_city_candidate(target, threshold = threshold)
  } else if (type == "census"){
    target <- gw_geocode_census_xy(target, zip = zipPresent)
  } else if (type == "composite, local"){
    target <- gw_geocode_composite(target, zip = zipPresent, local = local,
                                   local_short = local_short, local_place = local_place,
                                   threshold = threshold, offline = TRUE)
  } else if (type == "composite, full"){
    target <- gw_geocode_composite(target, zip = zipPresent, local = local,
                                   local_short = local_short, local_place = local_place,
                                   threshold = threshold, offline = FALSE)
  }

  # return output
  return(target)

}
