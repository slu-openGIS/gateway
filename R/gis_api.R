# Access to the City's ArcGIS REST API

#' City Address Candidates API Access
#'
#' @description Provides access to the City of Saint Louis\href{https://stlgis3.stlouis-mo.gov/arcgis/rest/services/PUBLIC/COMPPARSTRZIPHANDLE/GeocodeServer/findAddressCandidates}{Address Candidates API}
#'
#'
#' @usage gw_add_candidates(street, zip, address, n, threshold, crs, sf)
#'
#' @param street Name of street
#' @param zip 5-digit zipcode
#' @param address Single line address
#' @param n Number of candidates to return
#' @param threshold Numeric from 1 to 100, specifying how precise returned matches should be
#' @param crs Output spatial reference (CRS, WKID, ESRI Code)
#' @param sf Logical, output as simple feature object
#'
#' @return A data.frame or sf containing candidate addresses, x and y coordinates, and score of match
#'
#' @importFrom dplyr bind_rows filter
#' @importFrom httr content GET
#' @importFrom utils URLencode
#' @importFrom jsonlite parse_json
#' @importFrom sf st_as_sf
#'
#' @export
gw_add_candidates <- function(street, zip, address, n, threshold, crs, sf = FALSE){

  # global bindings
  score = address_match = x = y = NULL

  # error checking
  if(missing(street) & missing(address)){
    stop("At least one of street or address must be specified")
  }

  # build a query
  query <- "https://stlgis3.stlouis-mo.gov/arcgis/rest/services/PUBLIC/COMPPARSTRZIPHANDLE/GeocodeServer/findAddressCandidates?"

  if(!missing(street)){
    query <- paste0(query, "Street=", utils::URLencode(street), "&")
  }

  if(!missing(zip)){
    query <- paste0(query, "ZIP=", zip, "&")
  }

  if(!missing(address)){
    query <- paste0(query, "SingleLine=", utils::URLencode(address), "&")
  }

  if(!missing(n)){
    query <- paste0(query, "maxLocations=", n, "&")
  }

 if(!missing(crs)){
    query <- paste0(query, "outSR=", crs, "&")
 }

    # always return JSON
  query <- paste0(query, "f=pjson")

  # get and parse
  request <- httr::GET(query)
  content <- httr::content(request)
  parsed <- jsonlite::parse_json(content)

  # intialize output
  out <- vector("list", length(parsed[["candidates"]]))

  if (length(out) > 0){

    for (i in 1:length(parsed[["candidates"]])){
      out[[i]] <- data.frame(
        address = parsed[["candidates"]][[i]][["address"]],
        x = parsed[["candidates"]][[i]][["location"]][["x"]],
        y = parsed[["candidates"]][[i]][["location"]][["y"]],
        score = parsed[["candidates"]][[i]][["score"]],
        stringsAsFactors = FALSE)
    }

    df <- dplyr::bind_rows(out)

    df <- dplyr::rename(df, address_match = address)

    # score threshold
    if(!missing(threshold)){
      df <- dplyr::filter(df, score >= threshold)
    }

    if (nrow(df) > 0){
      # convert coordinates
      sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = 102696)
      sf <- sf::st_transform(sf, crs = 4269)
      sf <- gw_get_coords(sf)
      sf::st_geometry(sf) <- NULL
      sf <- dplyr::select(sf, address_match, x, y, score)

      # return sf if specified
      # if(sf == TRUE){
      #  sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = 102696)
      #  return(sf)
      #}
      # else{return(df)}

      return(sf)

    } else if (nrow(df) == 0){

      return(NA)

    }

  } else if (length(out) == 0){

    return(NA)

  }

}


#' City Address Batch API
#'
#' @param .data Name of data.frame containing address and id variables
#' @param id Name of column with unique identifier
#' @param address Vector containing addresses (List or Data.frame column)
#' @param threshold Numeric from 1 to 100, specifying how precise returned matches should be
#' @param vars How many variables should be returned? Choices are \code{"minmal"}, \code{"moderate"},
#'     or \code{"all"}.
#' @param crs Output spatial reference (Not yet implemented)
#'
#' @return Returns a data.frame with the API response
#'
#' @importFrom dplyr rename_at select as_tibble filter rename
#' @importFrom httr GET content status_code
#' @importFrom rlang := enquo quo quo_name sym
#' @importFrom janitor clean_names
#' @importFrom jsonlite toJSON minify flatten
#' @importFrom utils URLencode
#'
#' @export
gw_add_batch <- function(.data, id, address, threshold, vars = "minimal", crs){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$id)) {
    idQ <- rlang::enquo(id)
  } else if (is.character(paramList$id)) {
    idQ <- rlang::quo(!! rlang::sym(id))
  }

  if (!is.character(paramList$address)) {
    addressQ <- rlang::enquo(address)
  } else if (is.character(paramList$address)) {
    addressQ <- rlang::quo(!! rlang::sym(address))
  }

  # do not geocode 0 or 1 observation data sets
  if (nrow(.data) < 2){
    stop("This function is for batch geocoding. For single addresses, use the candidates function.")
  }

  # batch geocode
  if (nrow(.data) <= 1000){

    out <- gw_batch_call(.data, id = !!idQ, address = !!addressQ, threshold = threshold, vars = vars)
    out <- dplyr::rename(out, !!idQ := result_id)

  } else if (nrow(.data) > 1000){

    stop("Data sets over 1000 observations not yet implemented")

    dataList <- split(.data,rep(1:ceiling(nrow(.data)/1000),each=1000)[1:nrow(.data)])

    for (i in dataList){

      x <- dataList[i]

      y <- gw_batch_call(.data, id = !!idQ, address = !!addressQ, threshold = threshold, vars = vars)
      y <- dplyr::rename(y, !!idQ := result_id)

      dataList[i] <- y

    }

    out <- dplyr::bind_rows(dataList)

  }

  return(out)

}


gw_batch_call <- function(.data, id, address, threshold, vars = "minimal", crs){

  # global bindings
  . = address_match = match_address = x = y = score = comp_score = add_num_from = add_num_to = country =
    display_x = display_y = distance = everything = funs = lang_code = match_addr = result_id =
    score_2 = user_fld = xmax = xmin = ymax = ymin = NULL

  # quote input variables
  id <- rlang::quo_name(rlang::enquo(id))
  address <- rlang::quo_name(rlang::enquo(address))

  # create empty data.frame
  records <- data.frame(attributes = rep_len(NA, length(.data[[id]])))

  # create empty data frame
  attributes <- data.frame(OBJECTID = .data[[id]],
                  SingleLine = .data[[address]],
                  stringsAsFactors = FALSE)

  records$attributes <- attributes

  x <- list(records = records)
  query <- jsonlite::toJSON(x)
  query <- jsonlite::minify(query)

  baseurl <- "https://stlgis3.stlouis-mo.gov/arcgis/rest/services/PUBLIC/COMPPARSTRZIPHANDLE/GeocodeServer/geocodeAddresses"
  url <- paste0(baseurl, "?addresses=", query, "&category=&sourceCountry=&outSR=", "&f=json") # always return json
  url <- utils::URLencode(url)

  response <- httr::GET(url)
  message(paste0("Status Code: ",httr::status_code(response)))
  content <- httr::content(response, "text")

  # parse json to data.frame
  return <- jsonlite::fromJSON(content)$locations
  return <- jsonlite::flatten(return, recursive = TRUE)

  # clean-up data frame ### funs() is deprecated!
  return <- dplyr::rename_at(return, .vars = dplyr::vars(dplyr::starts_with("attributes.")),
                          .funs = funs(sub("^attributes[.]", "", .)))
  return <- dplyr::select(return, -dplyr::starts_with("location."))
  return <- janitor::clean_names(return, case = "snake")

  # remove duplicate variables
  return <- dplyr::select(return, -score_2, -match_addr)

  # remove additional variables
  if (vars == "minimal"){
    return <- dplyr::select(return, result_id, address, score, x, y)
  } else if (vars == "extended"){
    return <- dplyr::select(return, -c(display_x, display_y, xmin, xmax, ymin, ymax, user_fld,
                                       comp_score, add_num_from, add_num_to, country, lang_code,
                                       distance))
  }

  # id as first var
  return <- dplyr::select(return, result_id, everything())

  # ensure tibble
  out <- dplyr::as_tibble(return)

  # optionally filter
  if (missing(threshold) == FALSE){
    out <- dplyr::filter(out, score >= threshold)
  }

  # return output
  return(out)

}

#' City Reverse Geocoder
#' @param x Numeric X coordinate of the location to reverse geocode
#' @param y Numeric Y coordinate of the location to reverse geocode
#' @param distance The distance in meters from the given location within which a matching address should be searched. If this parameter is not provided or an invalid value is provided, a default value of 0 meters is used.
#' @param crs Numeric CRS or WKID for spatial projection
#' @param intersection Logical, Return nearest Address (FALSE) or Intersection (TRUE)
#'
#' @export
gw_add_reverse <- function(x, y, distance = 0, crs = 102696, intersection = FALSE){

  # build a query
  location <- paste0("{x:", x, ",y:", y, "}")
  baseURL <- "https://stlgis3.stlouis-mo.gov/arcgis/rest/services/PUBLIC/COMPPARSTRZIPHANDLE/GeocodeServer/reverseGeocode"
  query <- paste0(baseURL, "?location=", location, "&distance=", distance, "&outSR=", crs, "&returnIntersection=", intersection,
                  "&f=pjson")
  url <- utils::URLencode(query)

  # get a response
  response <- httr::GET(url)
  message(paste0("Status Code: ",httr::status_code(response)))
  content <- httr::content(response, "text")

  # parse the response
  content = httr::content(response, "text")
  parsed = jsonlite::fromJSON(content)$address

  df = dplyr::as_tibble(parsed)

  # warn for non-matches
  if(nrow(df) == 0){
    warning("No Matches Found, Try Increasing Distance")
  }

  # return

  return(df)

}

