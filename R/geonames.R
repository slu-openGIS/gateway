#' Access to the geonames general search
#'
#' @description Requires a geonames username. Register for a geonames.org username at http://www.geonames.org/login/ and then you will need to enable access to the geonames webservices by going to http://www.geonames.org/enablefreewebservice
#'
#' @usage gw_geonames(search, username, max_row = 10, fuzzy = 1, country = "US")
#'
#' @param search A string containing search query
#' @param username A string with your geonames username
#' @param max_row Single integer specifying how many rows you would like returned at maximum
#' @param fuzzy Numeric from > 0 to 1 specifying how strict to match spelling
#' @param country String containing country abbreviation to limit results
#'
#' @return A data.frame containing the geonames response, if no results, returns an empty list
#'
#' @importFrom utils URLencode
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @export
gw_geonames <- function(search, username, max_row = 10, fuzzy = 1, country = "US"){

  # build the query
  query <- paste0("http://api.geonames.org/search?q=", search,
                  "&maxRows=", max_row,
                  "&fuzzy=", fuzzy,
                  "&username=", username,
                  "&country=", country,
                  "&type=", "json")
  query <- utils::URLencode(query)

  # send
  response <- httr::GET(query)

  # parse
  response <- httr::content(response, as = "text")
  df = jsonlite::fromJSON(response)[["geonames"]]


  return(df)

}
