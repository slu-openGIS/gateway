#' City Address Validation API
#'
#' @description Provides access to the City of Saint Louis\href{https://www.stlouis-mo.gov/government/departments/information-technology/web-development/city-api/address-validation-api.cfm}{Address Validation API}
#'
#' @details An API key is required to use this function, you can obtain one \href{https://www.stlouis-mo.gov/government/departments/information-technology/web-development/city-api/sign-up.cfm}{here}.
#'
#' @usage gw_address_validation(search, key)
#'
#' @param search A string containing a single address or 11-digit parcel ID
#' @param key Your API key (will be stored in Renviron in future update)
#'
#' @return A data.frame with 59 variables for all addresses matching the search query
#'
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate_all
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr warn_for_status
#' @importFrom purrr map
#' @importFrom purrr map_df
#' @importFrom utils URLencode
#'
#' @export
gw_address_validation <- function(search, key){

  # look for City API key
  if (Sys.getenv('CITY_API_KEY') != '') {

    key <- Sys.getenv('CITY_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required. Obtain one at ..., and then supply the key to the `gw_api_key` function to use it throughout your tidycensus session.')

  }

  # Build the Query URL
  query <- paste0("http://stlouis-mo.gov/powernap/stlouis/api.cfm/addressValidation/properties.json?api_key=",
   key, "&addresssearch=", search)
  query <- utils::URLencode(query) # this corrects " " to "%20" etc.

  # GET a response
  response <- httr::GET(query)

  # Error Checking for bad return
  httr::warn_for_status(response)

  # extract content
  content <- httr::content(response)

  # convert response to a tibble
  content %>%
    purrr::map(.f = dplyr::as_tibble) %>%
    purrr::map_df(~ dplyr::mutate_all(.x, as.character)) -> out

  # return tibble
  return(out)

}

#' Install a City of St. Louis API Key in Your \code{.Renviron} File for Repeated Use
#'
#' @param key Your quoted API key.
#' @param install A logical scalar; if \code{TRUE}, your key will be installed in your \code{.Renviron}
#'     file for use in future sessions.  If \code{FALSE} (default), your key will be available only
#'     for your current session.
#' @param overwrite If this is set to TRUE, it will overwrite an existing CENSUS_API_KEY
#'     that you already have in your \code{.Renviron} file.
#'
#' @importFrom utils read.table
#' @importFrom utils write.table
#'
#' @export
gw_api_key <- function(key, overwrite = FALSE, install = FALSE){

  if (install) {

    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    # Backup original .Renviron before doing anything else here.
    if(file.exists(renv)){
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }

    if(!file.exists(renv)){
      file.create(renv)
    }

    else {

      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("CITY_API_KEY", oldenv),]
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("CITY_API_KEY",tv))){
          stop("A CITY_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("CITY_API_KEY='", key, "'")

    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)

    message('Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("CITY_API_KEY"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')

    return(key)

  } else {

    message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(CITY_API_KEY = key)

  }

}
