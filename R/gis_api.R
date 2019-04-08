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
#' @importFrom httr content
#' @importFrom httr GET
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
#' @param crs Output spatial reference (Not yet implemented)
#' @param comp_score Logical, used to return composite score from geocoder for diagnostic purposes
#'
#'
#'
#' @return Returns a data.frame with matching address, id and locations, Optionally the composite score as well
#'
#' @importFrom httr GET content status_code
#' @importFrom utils URLencode
#'
#' @export
gw_add_batch <- function(.data, id, address, crs, comp_score = FALSE){

  # global bindings
  address_match = match_address = x = y = score = comp_score = NULL

  # if(class(.data$address) != "character"){stop("Addresses must be of class character")}

  if(length(.data$id) == 1){stop("This function is for batch geocoding. For single addresses, use the candidates function.")}
    # Ugly and innefficient JSON implementation

  query <- '{"records":['
  for (i in 1:length(.data$id)) {
    query <- paste0(query,
                    '
                    {
                     "attributes": {
                      "OBJECTID":', .data$id[i],',
                      "SingleLine":"', .data$address[i], '"}}'
                    )
    if(i != length(.data$id)){
      query <- paste0(query, ",")
    }
  }
  query <- paste0(query, "]}")
  query <- jsonlite::minify(query)
  # -----

  baseurl <- "https://stlgis3.stlouis-mo.gov/arcgis/rest/services/PUBLIC/COMPPARSTRZIPHANDLE/GeocodeServer/geocodeAddresses"
  url <- paste0(baseurl, "?addresses=", query, "&category=&sourceCountry=&outSR=", "&f=json") # always return json
  url <- utils::URLencode(url)

  response <- httr::GET(url)
  message(paste0("Status Code: ",httr::status_code(response)))
  content <- httr::content(response)

  # initialize object
  df <- NULL

  # add original ids
  df$orig_id <- .data$id
  df <- data.frame(df)

  # loop through response to build df
  for (i in 1:length(content[["locations"]])) {
    df$address_match[i] = content[["locations"]][[i]][["address"]]
    df$x[i] = content[["locations"]][[i]][["location"]][["x"]]
    df$y[i] = content[["locations"]][[i]][["location"]][["y"]]
    df$score[i] = content[["locations"]][[i]][["score"]]
  }

  # add comp score for diagnostics
  # if(comp_score == TRUE){
  #  for (i in 1:length(content[["locations"]])) {
  #  df$comp_score[i] = content[["locations"]][[i]][["attributes"]][["Comp_score"]]
  #  }
  # }

  out <- dplyr::as_tibble(df)
  out <- dplyr::mutate(out, x = as.numeric(x))
  out <- dplyr::mutate(out, y = as.numeric(y))

    return(out)
}

#' City Reverse Geocoder
#' @param x Numeric X coordinate of the location to reverse geocode
#' @param y Numeric Y coordinate of the location to reverse geocode
#' @param distance The distance in meters from the given location within which a matching address should be searched. If this parameter is not provided or an invalid value is provided, a default value of 0 meters is used.
#' @param crs Numeric CRS or WKID for spatial projection
#' @param intersection Logical, Return nearest Address (FALSE) or Intersection (TRUE)
#'
#' @importFrom
#'
#' @export
gw_add_reverse <- function(x, y, distance = 0, crs = 102696, intersection = FALSE){
  # build a query
  location <- paste0("{x:", x, ",y:", y, "}")
  baseURL <- "https://stlgis3.stlouis-mo.gov/arcgis/rest/services/PUBLIC/COMPPARSTRZIPHANDLE/GeocodeServer/reverseGeocode"
  query <- paste0(baseURL, "?location=", location, "&distance=", distance, "&outSR=", crs, "&returnIntersection=", intersection,
                  "&f=pjson")
  query <- utils::URLencode(query)

  # get a response
  response <- httr::GET(url = query)
  message(paste0("Status Code: ",httr::status_code(response)))

  # parse the response
  content = httr::content(response)
  parsed = jsonlite::fromJSON(content)

    # warning for non matches.
  code = parsed$error$code # only returned if error
  if(!is.null(code)){
    if(code == 400){
      stop("No Match Found. You may need to increase distance to find a match")
    }
  }
  else{ # build a data.frame from json
    df = NULL # initialize data.frame
    df$street = parsed$address$Street
    df$zip = parsed$address$ZIP
    df$address_match = parsed$address$Match_addr
    df$loc_name = parsed$address$Loc_name
    df$location.x = parsed$location$x
    df$location.y = parsed$location$y

    df = data.frame(df)
    df = dplyr::as_tibble(df)
  }

  # return

  return(df)

}

