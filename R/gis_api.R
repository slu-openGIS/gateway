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

    # score threshold
    if(!missing(threshold)){
      df <- dplyr::filter(df, score >= threshold)
    }

    # return sf if specified
    if(sf == TRUE){
      sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = crs)
      return(sf)
    }
    else{return(df)}

  } else if (length(out) == 0){

    return(NA)

  }

}


gw_add_batch <- function(){

}


gw_add_reverse <- function(){

}

