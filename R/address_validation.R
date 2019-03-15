#' City Address Validation API
#'
#' @description Provides access to the City of Saint Louis\href{https://www.stlouis-mo.gov/government/departments/information-technology/web-development/city-api/address-validation-api.cfm}{Address Validation API}
#'
#' @details An API key is required to use this function, you can obtain one \href{}{here}.
#'
#' @usage gw_address_validation(search, key)
#'
#' @param search A string containing a single address or 11-digit parcel ID
#' @param key Your API key (will be stored in Renviron in future update)
#'
#' @return A data.frame with 59 variables for all addresses matching the search query
#'
#' @importFrom httr GET warn_for_status content
#' @importFrom utils URLencode
#' @importFrom dplyr bind_rows
#'
#' @export
gw_address_validation <- function(search, key){
  # Build the Query URL
  query <- paste0("http://stlouis-mo.gov/powernap/stlouis/api.cfm/addressValidation/properties.json?api_key=",
   key, "&addresssearch=", search)
  query <- utils::URLencode(query) # this corrects " " to "%20" etc.

  # GET a response
  response <- httr::GET(query)

  # Error Checking for bad return
  httr::warn_for_status(response)

  # convert response to a data.frame
    # extract content
  content <- httr::content(response)
    # iteratively convert lists to df, assign to new list
  rets <- vector("list", length(content))

  for (i in 1:length(content)) {
    rets[[i]] <- data.frame(content[[i]], stringsAsFactors = FALSE)
  }
    # compile list into one data frame
  return = dplyr::bind_rows(rets)

  # return data frame
  return(return)
}
