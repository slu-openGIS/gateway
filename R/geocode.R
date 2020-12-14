#' Build Local Geocoder
#'
#' @description This constructs a local geocoder that is based on the latest release of the City of
#'    St. Louis's master address list. This function therefore requires an internet connection,
#'    and will error if your computer is offline. Since the actual geocoding is done with a second
#'    function, however, it is possible to build a geocoder and store it offline for repeated use.
#'
#' @usage gw_build_geocoder(return = c("coords", "parcel", "zip"), crs = 4269, include_units = FALSE,
#'     appendix)
#'
#' @param return Optional; A character scalar or vector that describes the type of information to be applied
#'    to the target data. Options include the City's address identification numbers (\code{addrrecnum}),
#'    parcel identification numbers (\code{handle}), zip-codes, and x and y coordinates (in decimal degrees).
#' @param crs A numeric code corresponding to the desired coordinate system for the column output if
#'    \code{return} includes \code{"coords"} as well as the object output if \code{class} is \code{"sf"}.
#' @param include_units A logical scalar; if \code{TRUE}, all individual records for apartment units will
#'    be included. If \code{FALSE} (default), only records for the overall building will be retained.
#' @param appendix An object with additional address or placename data to append to the geocoder.
#'
#' @return A list containing the \code{"full"} (\code{"123 Main St"}), \code{"short"} (\code{"123 Main"}),
#'    and \code{"placename"} geocoders for St. Louis, MO.
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
gw_build_geocoder <- function(return = c("coords", "parcel", "zip"), crs = 4269, include_units = FALSE, appendix){

  # set global bindings
  ADDRRECNUM = HANDLE = HOUSENUM = HOUSESUF = PREDIR = STREETNAME = STREETTYPE =
    SUFDIR = UNITNUM = ZIP = address = address_short = x = y = flag =
    geonameid = zip = id = name = addrrecnum = gw_addrrecnum = NULL

  # check for optional return argument
  if (missing(return)){
    return <- NULL
  }

  # download master list
  full <- gw_get_data(data = "Addresses", class = "sf")

  # initial tidy
  ## clean address data
  full <- dplyr::select(full, ADDRRECNUM, HANDLE:ZIP)

  ## optionally filter out units
  if (include_units == FALSE){
    full <- dplyr::filter(full, is.na(UNITNUM) == TRUE)
  }

  ## optionally drop unneeded data
  ### parcel IDs
  if ("parcel" %in% return == FALSE){
    full <- dplyr::select(full, -HANDLE)
  }

  ### zip codes
  if ("zip" %in% return == FALSE){
    full <- dplyr::select(full, -ZIP)
  }

  ### xy coordinates
  if ("coords" %in% return == TRUE){
    full <- gw_get_coords(full, crs = crs)
    sf::st_geometry(full) <- NULL
  } else if ("coords" %in% return == FALSE){
    sf::st_geometry(full) <- NULL
  }

  ## convert to tibble
  full <- dplyr::as_tibble(full)

  # create copy of master data
  short <- full

  # create full geocoder
  ## tidy
  full <- dplyr::mutate(full, UNITNUM = ifelse(HOUSESUF == "E", "E", UNITNUM))
  full <- dplyr::mutate(full, HOUSESUF = ifelse(HOUSESUF == "E", NA, HOUSESUF))
  full <- postmastr::pm_street_std(full, var = STREETNAME, locale = "us")
  full <- postmastr::pm_streetSuf_std(full, var = STREETTYPE, locale = "us")
  full <- tidyr::unite(full, address, HOUSENUM:SUFDIR, sep = " ", remove = TRUE)
  full <- dplyr::mutate(full, address = stringr::str_replace_all(address, pattern = "\\bNA\\b", replacement = ""))
  full <- dplyr::mutate(full, address = stringr::str_squish(address))
  full <- dplyr::distinct(full, address, .keep_all = TRUE)

  ## rename variables
  full <- dplyr::rename(full,
                          gw_addrrecnum = ADDRRECNUM,
                          gw_address = address,
                          gw_x = x,
                          gw_y = y)

  # create short geocoder
  ## tidy
  short <- dplyr::mutate(short, UNITNUM = ifelse(HOUSESUF == "E", "E", UNITNUM))
  short <- dplyr::mutate(short, HOUSESUF = ifelse(HOUSESUF == "E", NA, HOUSESUF))
  short <- postmastr::pm_street_std(short, var = STREETNAME, locale = "us")
  short <- postmastr::pm_streetSuf_std(short, var = STREETTYPE, locale = "us")
  short <- tidyr::unite(short, address, HOUSENUM:SUFDIR, sep = " ", remove = FALSE)
  short <- dplyr::mutate(short,
                          address = stringr::str_replace_all(address, pattern = "\\bNA\\b", replacement = ""))
  short <- dplyr::mutate(short, address = stringr::str_squish(address))
  short <- tidyr::unite(short, address_short, HOUSENUM:STREETNAME, sep = " ", remove = TRUE)
  short <- dplyr::mutate(short,
                          address_short = stringr::str_replace_all(address_short, pattern = "\\bNA\\b", replacement = ""))
  short <- dplyr::mutate(short, address_short = stringr::str_squish(address_short))
  short <- dplyr::select(short, -STREETTYPE, -SUFDIR)
  short <- dplyr::distinct(short, address, .keep_all = TRUE)
  short <- dplyr::select(short, -address)

  sub <- dplyr::distinct(short, address_short, x, y, .keep_all = TRUE)

  dupes <- janitor::get_dupes(sub, address_short)
  dupes <- dplyr::distinct(dupes, address_short)
  dupes <- dplyr::mutate(dupes, flag = TRUE)

  short <- dplyr::left_join(sub, dupes, by = "address_short")
  short <- dplyr::filter(short, is.na(flag) == TRUE)
  short <- dplyr::select(short, -flag)

  ## rename variables
  short <- dplyr::rename(short,
                          gw_addrrecnum = ADDRRECNUM,
                          gw_address = address_short,
                          gw_x = x,
                          gw_y = y)

  # create placename geocoder
  ## obtain master list
  place <- gw_get_data(data = "Placenames", class = "sf")

  ## remove geonameid
  place <- dplyr::select(place, -geonameid)

  ## optionally drop unneeded data
  # if ("parcel" %in% return == FALSE){
  #  place <- dplyr::select(place, -HANDLE)
  #}

  if ("zip" %in% return == FALSE){
    place <- dplyr::select(place, -zip)
  }


  ## create coordinates if class is tibble
  if ("coords" %in% return == TRUE){
    place <- gw_get_coords(place, crs = crs)
    sf::st_geometry(place) <- NULL
  } else if ("coords" %in% return == FALSE){
    sf::st_geometry(place) <- NULL
  }

  ## conver to tibble
  place <- dplyr::as_tibble(place)

  ## rename variables
  place <- dplyr::rename(place,
                          gw_id = id,
                          gw_name = name,
                          gw_addrrecnum = addrrecnum,
                          gw_address = address,
                          gw_x = x,
                          gw_y = y)

  ## convert addrrecnum to character
  place <- dplyr::mutate(place, gw_addrrecnum = as.character(gw_addrrecnum))

  # construct output
  out <- list(
    full = full,
    short = short,
    placename = place
  )

  # return output
  return(out)

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

  # ensure two columns are returned
  stopifnot(length(names) == ncol(ret))

  # name columns with coordinate data
  ret <- dplyr::as_tibble(ret, .name_repair = ~names)

  # combine coordinate data with source data
  out <- cbind(.data, ret)
  out <- dplyr::select(out, -geometry, dplyr::everything())

  # return output
  return(out)

}

#' Geocode Addresses
#'
#' @description Apply a previously build geocoder to target data. This function will
#'    apply whatever unique variables exist in the geocoder. See \code{\link{gw_build_geocoder}}
#'    for options.
#'
#' @usage gw_geocode(.data, type, var, zip, class, geocoder, threshold)
#'
#' @param .data A target data set
#' @param type Geocoder type; one of either \code{"local"}, \code{"local short"}, \code{"city batch"},
#'    \code{"city candidate"}, \code{"census"}, or \code{"osm"}. Composite geocoders consisting of
#'    only local geocoders (\code{"local"}, \code{"local short"}, and \code{"local placename"}) or
#'    both local and the remote geocoders are also available by using \code{"composite, local"} or
#'    \code{"composite, full"} respectively.
#' @param var Address variable in the target data set, which should contain the house number,
#'    street directionals, name, and suffix.
#' @param zip Name of zipcode variable in the target data set (optional).
#' @param class Output class; one of either \code{"sf"} or \code{"tibble"}.
#' @param geocoder Name of object containing a local geocoder built with \code{\link{gw_build_geocoder}}
#' @param threshold For the city candidate geocoder, what score is the minimum acceptable?
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
gw_geocode <- function(.data, type, var, zip, class, geocoder, threshold){

  # set global bindings
  . = ...address = out = addrrecnum = geometry = ...zip = gw_address = NULL

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
    target <- gw_geocode_local(target, geocoder = geocoder$full)
  } else if (type == "local short"){
    target <- gw_geocode_local_short(target, geocoder = geocoder$short)
  } else if (type == "local placename"){
    target <- gw_geocode_local_placename(target, geocoder = geocoder$placename)
  } else if (type == "city batch"){
    batch <- TRUE
    target <- gw_geocode_city_batch(target, crs = 4269, zip = zipPresent)
  } else if (type == "city candidate"){
    target <- gw_geocode_city_candidate(target, threshold = threshold, zip = zipPresent)
  } else if (type == "census"){
    target <- gw_geocode_census_xy(target, zip = zipPresent)
  } else if (type == "composite, local"){
    target <- gw_geocode_composite(target, zip = zipPresent, geocoder = geocoder,
                                   threshold = threshold, offline = TRUE)
  } else if (type == "composite, full"){
    target <- gw_geocode_composite(target, zip = zipPresent, geocoder = geocoder,
                                   threshold = threshold, offline = FALSE)
  }

  # rebuild data
  .data <- gw_geocode_replace(source = .data, target = target, zip = zipPresent, batch = batch)

  # re-order variables
  vars <- gw_reorder_target(.data)

  # re-order data
  .data <- dplyr::select(.data, vars$source.vars, vars$gw.vars)

  # rename variables again
  .data <- dplyr::rename(.data, !!varQ := ...address)

  if (missing(zip) == FALSE){
    .data <- dplyr::rename(.data, !!zipQ := ...zip)
  }

  # change capitalization
  if ("gw_address" %in% names(.data)){
    .data <- dplyr::mutate(.data, gw_address = stringr::str_to_title(gw_address, locale = "en"))
  }

  # set-up output
  if (class == "sf"){
    .data <- sf::st_as_sf(.data, coords = c("gw_x", "gw_y"), crs = 4269)
  }

  # return output
  return(.data)

}


# local geocoder
gw_geocode_local <- function(.data, geocoder){

  # set global bindings
  gw_address = gw_x = NULL

  # rename geocoder address column
  geocoder <- dplyr::rename(geocoder, ...address = gw_address)

  # geocode
  .data <- dplyr::left_join(.data, geocoder, by = "...address")

  # include result
  .data <- dplyr::mutate(.data, gw_source = ifelse(is.na(gw_x) == FALSE, "local", NA))
  .data <- dplyr::mutate(.data, gw_score = ifelse(is.na(gw_x) == FALSE, 100, NA))

  # return output
  return(.data)

}

# local geocoder
gw_geocode_local_short <- function(.data, geocoder){

  # set global bindings
  gw_address = gw_x = NULL

  # rename geocoder address column
  geocoder <- dplyr::rename(geocoder, ...address = gw_address)

  # geocode
  .data <- dplyr::left_join(.data, geocoder, by = "...address")

  # include result
  .data <- dplyr::mutate(.data, gw_source = ifelse(is.na(gw_x) == FALSE, "local, short", NA))
  .data <- dplyr::mutate(.data, gw_score = ifelse(is.na(gw_x) == FALSE, 100, NA))

  # return output
  return(.data)

}

# placename geocoder
gw_geocode_local_placename <- function(.data, geocoder){

  # global variables
  gw_name = gw_x = NULL

  # rename geocoder address column
  geocoder <- dplyr::rename(geocoder, ...address = gw_name)

  # geocode
  .data <- dplyr::left_join(.data, geocoder, by = "...address")

  # include result
  .data <- dplyr::mutate(.data, gw_source = ifelse(is.na(gw_x) == FALSE, "local, placename", NA))
  .data <- dplyr::mutate(.data, gw_score = ifelse(is.na(gw_x) == FALSE, 100, NA))

  # return output
  return(.data)

}

# city api, batch geocoder
gw_geocode_city_batch <- function(.data, crs, zip){

  # global bindings
  ...uid = score = address = x = y = address_match = score = gw_x = gw_y =
    gw_address = gw_score = gw_source = NULL

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

  # reorder
  target <- dplyr::select(target, ...uid, gw_address, gw_score, gw_x, gw_y, gw_source)

  # return output
  return(target)

}

# city api, candidate geocoder
gw_geocode_city_candidate <- function(.data, threshold, zip){

  # global bindings
  ...address = geo = x = y = address_match = score = ...uid = ...zip =
    gw_address = gw_score = gw_x = gw_y = gw_source = NULL

  # generate candidates
  target <- dplyr::mutate(.data, geo = purrr::map(...address, ~
              gw_create_candidates(address = .x, style = "top", threshold = threshold)))

  # remove NAs
  target <- dplyr::filter(target, is.na(geo) == FALSE)

  # modify based on results
  if (nrow(target) > 0){

    # unnest results
    target <- tidyr::unnest(target, cols = c(geo))

    # rename
    target <- dplyr::rename(target,
                            gw_x = x,
                            gw_y = y,
                            gw_address = address_match,
                            gw_score = score)

    # include result
    target <- dplyr::mutate(target, gw_source = "city api, candidate")

    # reorder
    if (zip == TRUE){
      target <- dplyr::select(target, ...uid, ...address, ...zip, gw_address, gw_score, gw_x, gw_y, gw_source)
    } else if (zip == FALSE){
      target <- dplyr::select(target, ...uid, ...address, gw_address, gw_score, gw_x, gw_y, gw_source)
    }

  }

  # return output
  return(target)

}

gw_create_candidates <- function(address, style, threshold){

  if (style == "top"){

    api_result <- gw_add_candidates(address = address, n = 1, threshold = threshold, crs = 4269)

  } else if (style == "all"){

    api_result <- gw_add_candidates(address = address, crs = 4269)
    api_result <- tibble::rowid_to_column(api_result, var = "result_id")

  }

  return(api_result)

}

# census xy
gw_geocode_census_xy <- function(.data, zip){

  # global variables
  ...uid = ...address = ...zip = gw_i_uid = gw_i_address = lon = lat =
    cxy_match = cxy_quality = gw_x = gw_y = gw_i_city = gw_i_state =
    cxy_status = gw_i_zip = gw_address = gw_score = gw_source = NULL

  # rename id
  .data <- dplyr::rename(.data,
                         gw_i_uid = ...uid,
                         gw_i_address = ...address)

  # construct data
  if (zip == TRUE){
    .data <- dplyr::mutate(.data,
                           gw_i_city = "St. Louis",
                           gw_i_state = "MO")

    .data <- dplyr::rename(.data, gw_i_zip = ...zip)

  } else if (zip == FALSE){
    .data <- dplyr::mutate(.data,
                           gw_i_city = "St. Louis",
                           gw_i_state = "MO",
                           gw_i_zip = NA)
  }

  # geocode
  .data <- censusxy::cxy_geocode(.data, street = "gw_i_address", city = "gw_i_city",
                                 state  = "gw_i_state", zip = "gw_i_zip",
                                 class = "dataframe", output = "simple", timeout = 30)
  # rename
  .data <- dplyr::rename(.data,
                         ...uid = gw_i_uid,
                         ...address = gw_i_address,
                         gw_x = cxy_lon,
                         gw_y = cxy_lat)

  # include result
  .data <- dplyr::mutate(.data, gw_source = ifelse(is.na(gw_x) == FALSE, "census api", NA))

  # remove uneeded columns and reorder
  .data <- dplyr::select(.data, -gw_i_city, -gw_i_state)

  if (zip == TRUE){

    # rename
    .data <- dplyr::rename(.data, ...zip = gw_i_zip)

    # re-order
    .data <- dplyr::select(.data, ...uid, ...address, ...zip, gw_x, gw_y, gw_source)

  } else if (zip == FALSE){

    # re-order
    .data <- dplyr::select(.data, ...uid, ...address, gw_x, gw_y, gw_source)

  }

  # return output
  return(.data)

}

gw_geocode_composite <- function(.data, zip, geocoder, threshold, offline){

  # global binding
  gw_x = gw_y = gw_addrrecnum = gw_source = gw_id = gw_address = gw_score =
    ...address = ...zip = NULL

  # local geocoder
  .data <- gw_geocode_local(.data, geocoder = geocoder$full)

  # check results
  result <- any(is.na(.data$gw_x))

  # short geocoder
  if (result == TRUE){

    # subset results
    matched <- dplyr::filter(.data, is.na(gw_x) == FALSE)
    unmatched <- dplyr::filter(.data, is.na(gw_x) == TRUE)
    unmatched <- dplyr::select(unmatched, -gw_addrrecnum, -gw_x, -gw_y, -gw_source)

    # geocode
    unmatched <- gw_geocode_local_short(unmatched, geocoder = geocoder$short)

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
      unmatched <- gw_geocode_local_placename(unmatched, geocoder = geocoder$placename)

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
        results <- gw_geocode_city_batch(unmatched, crs = 4269, zip = zip)

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
          results <- gw_geocode_city_candidate(unmatched, threshold = threshold, zip = zip)

          # put results back into unmatched data
          if (nrow(results) > 0){

            if (zip == TRUE){
              results <- dplyr::select(results, -...address, -...zip)
            } else if (zip == FALSE){
              results <- dplyr::select(results, -...address)
            }

            unmatched <- dplyr::left_join(unmatched, results, by = "...uid")

          }

          # check results
          if (nrow(results) > 0){
            result2 <- any(is.na(unmatched$gw_x))
          } else if (nrow(results) == 0){
            result2 <- TRUE
          }

          # censusxy geocoder
          if (result2 == TRUE){

            # rebuild results
            .data <- dplyr::bind_rows(matched, unmatched)

            # subset results
            matched <- dplyr::filter(.data, is.na(gw_x) == FALSE)
            unmatched <- dplyr::filter(.data, is.na(gw_x) == TRUE)
            unmatched <- dplyr::select(unmatched, -gw_addrrecnum, -gw_x, -gw_y, -gw_source, -gw_id, -gw_address, -gw_score)

            # geocode
            result <- try(gw_geocode_census_xy(unmatched, zip = zip), silent = TRUE)

            # check for error
            if ("try-error" %in% class(result) == FALSE){
              unmatched <- result
            } else if ("try-error" %in% class(result) == TRUE){
              warning("The final stage of the geocoder, censusxy, returned an error. Results from this stage are not included.")
            }

          }
        }

      }
    }

    # re-construct datadev
    # matched <- mutate(matched, gw_score = as.character(gw_score))
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
  ...uid = ...address = ...zip = NULL

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
  . = ...id = ...uid = ...address = ...zip = NULL

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

# re-order variables
gw_reorder_target <- function(.data){

  # create vector of current gw variables in data
  .data %>%
    dplyr::select(dplyr::starts_with("gw_")) %>%
    names() -> gwVarsCurrent

  # create vector of original source data variables
  .data %>%
    dplyr::select(-dplyr::starts_with("gw_")) %>%
    names() -> sourceVars

  # master list of variables for pm objects
  master <- data.frame(
    master.vars = c("gw_addrrecnum", "gw_id","gw_address", "gw_score",
                    "gw_x", "gw_y", "gw_source"),
    stringsAsFactors = FALSE)

  # create data frame of current variables
  working <- data.frame(
    master.vars = c(gwVarsCurrent),
    working.vars = c(gwVarsCurrent),
    stringsAsFactors = FALSE
  )

  # join master and working data
  joined <- dplyr::left_join(master, working, by = "master.vars")

  # create vector of re-ordered variables
  vars <- stats::na.omit(joined$working.vars)

  out <- list(
    gw.vars = c(vars),
    source.vars = c(sourceVars)
  )

  return(out)

}
