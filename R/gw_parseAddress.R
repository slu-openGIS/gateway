#' Parse Street Addresses
#'
#' \code{gw_parseAddress} is designed to process a single vector that contains
#' a house number and street information including prefix direction, street
#' name, and street type. The inclusion of the house number is optional.
#' These data look like either "123 Main St" or "Main St". With the
#' \code{keepVars} argument, users can decide whether they want to simply
#' return a cleaned version of their input with standardized spelling and
#' formatting, or whether they also want to return individual variables
#' representing specific address components.
#'
#' @usage gw_parseAddress(.data, address, houseNum = TRUE, overwrite = TRUE,
#'     keepVars = TRUE, locale = 29510)
#'
#' @param .data A tbl
#' @param address A character vector with street address information
#'     formatted as either "123 Main St" or "Main St"
#' @param houseNum A logical scalar. Are house numbers included in
#'     the \code{address} vector?
#' @param overwrite A logical scalar. Should the output overwrite
#'     the given variable?
#' @param keepVars A logical scalar. Should the specific address
#'     components be returned as individual variables?
#' @param locale A numeric scalar. The only current accepted
#'     value is 29510 for the City of St. Louis. This will apply specific
#'     street name corrections that are applicable to St. Louis.
#'
#' @return \code{gw_stName} returns a tibble with the requested output -
#' either the original data with the street address overwritten in the
#' given vector or the original data with a set of new individual variables
#' representing specific address components.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr fixed
#' @importFrom stringr str_count
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_sub
#' @importFrom stringr word
#' @importFrom rlang :=
#'
#' @note \code{gw_parseAddress} requires that a number variable names be unused
#' in the original data - \code{houseNum}, \code{houseNumL}, \code{houseNumU},
#' \code{stFull}, \code{stDir}, \code{stName}, \code{stType}. If these names
#' are present, \code{gw_parseAddress} will return an error.
#'
#' @export
gw_parseAddress <- function(.data, address, houseNum = TRUE, overwrite = TRUE, keepVars = TRUE, locale = 29510){

  # ensure no conflicts with user's data:
  if ( any(names(.data) == "houseNum") == TRUE ) {
    stop('data cannot contain a variable named houseNum')
  }
  if ( any(names(.data) == "houseNumL") == TRUE ) {
    stop('data cannot contain a variable named houseNumL')
  }
  if ( any(names(.data) == "houseNumU") == TRUE ) {
    stop('data cannot contain a variable named houseNumU')
  }
  if ( any(names(.data) == "stFull") == TRUE ) {
    stop('data cannot contain a variable named stFull')
  }
  if ( any(names(.data) == "stDir") == TRUE ) {
    stop('data cannot contain a variable named stDir')
  }
  if ( any(names(.data) == "stName") == TRUE ) {
    stop('data cannot contain a variable named stName')
  }
  if ( any(names(.data) == "stType") == TRUE ) {
    stop('data cannot contain a variable named stType')
  }

  # save parameters to list
  paramList <- as.list(match.call())

  if (!is.character(paramList$address)) {
    var <- rlang::enquo(address)
  } else if (is.character(paramList$address)) {
    var <- rlang::quo(!! rlang::sym(address))
  }

  varQ <- rlang::quo_name(rlang::enquo(var))

  # prevents R CMD check note for undefined gloabl variable:
  count <- NULL
  houseNumL <- NULL
  houseNumS <- NULL
  houseNumU <- NULL
  stFull <- NULL
  stDir <- NULL
  stName <- NULL
  stType <- NULL

  # convert street name variable to Title Case
  .data <- dplyr::mutate(.data, "stFull" := stringr::str_to_title(.data[[varQ]]))

  # split address ranges
  if (houseNum == TRUE) {
    .data %>%
      dplyr::mutate(houseNum = stringr::word(stFull, 1)) %>%
      dplyr::mutate(houseNumS = stringr::str_replace_all(houseNum, stringr::fixed("-"), " ")) %>%
      dplyr::mutate(count = vapply(strsplit(houseNumS, "\\W+"), length, integer(1))) %>%
      dplyr::mutate(houseNumL = stringr::word(houseNumS, 1)) %>%
      dplyr::mutate(houseNumU = ifelse(count == 1, houseNumL, stringr::word(houseNumS, 2))) %>%
      dplyr::mutate(houseNumU = paste(substr(houseNumL, 1, nchar(houseNumL) - nchar(houseNumU)),
                                      houseNumU, sep = '')) %>%
      dplyr::mutate(houseNumL = stringr::str_extract(houseNumL, '\\d+')) %>%
      dplyr::mutate(houseNumL = as.numeric(houseNumL)) %>%
      dplyr::mutate(houseNumU = stringr::str_extract(houseNumU, '\\d+')) %>%
      dplyr::mutate(houseNumU = as.numeric(houseNumU)) %>%
      dplyr::mutate(houseNumU = ifelse(houseNumL == houseNumU, NA, houseNumU)) %>%
      dplyr::select(-count, -houseNumS) -> .data
  }

  # parse address variable
  if (houseNum == TRUE){
    .data %>%
      dplyr::mutate(count = stringr::str_count(stFull, pattern = "\\S+")) %>%
      dplyr::mutate(stDir = ifelse(count > 2 & nchar(word(stFull, 2)) == 1,
                                   stringr::word(stFull, 2), NA)) %>%
      dplyr::mutate(stName = ifelse(count == 3, stringr::word(stFull, 2), NA)) %>%
      dplyr::mutate(stName = ifelse(count > 3 & nchar(stringr::word(stFull, 2)) == 1,
                                    stringr::word(stFull, start = 3, end = count-1),
                                    stName)) %>%
      dplyr::mutate(stName = ifelse(count > 3 & nchar(stringr::word(stFull, 2)) > 1,
                                    stringr::word(stFull, start = 2, end = count-1),
                                    stName)) %>%
      dplyr::mutate(stType = stringr::word(stFull, -1)) %>%
      dplyr::select(-count)  -> .data
  } else if (houseNum == FALSE) {
    .data %>%
      dplyr::mutate(count = stringr::str_count(stFull, pattern = "\\S+")) %>%
      dplyr::mutate(stDir = ifelse(count > 1 & nchar(stringr::word(stFull, 1)) == 1,
                                   stringr::word(stFull, 1), NA)) %>%
      dplyr::mutate(stName = ifelse(count == 2, stringr::word(stFull, 1), NA)) %>%
      dplyr::mutate(stName = ifelse(count > 2 & nchar(word(stFull, 1)) == 1,
                                    stringr::word(stFull, start = 2, end = count-1),
                                    stName)) %>%
      dplyr::mutate(stName = ifelse(count > 2 & nchar(stringr::word(stFull, 1)) > 1,
                                    stringr::word(stFull, start = 1, end = count-1),
                                    stName)) %>%
      dplyr::mutate(stType = stringr::word(stFull, -1)) %>%
      dplyr::select(-count) -> .data
  }

  # directionals
  dirCardinal <- c("North", "South", "East", "West")
  dirCardinalA <- c("N", "S", "E", "W")
  dirInterCardinal <- c("Nw", "Ne", "Sw", "Se")
  dirAll <- c("North", "South", "East", "West", "Nw", "Ne", "Sw", "Se",
             "Northwest", "Northeast", "Southwest", "Southwest")

  # prefix direction
  .data %>%
    dplyr::mutate(count = stringr::str_count(stName, pattern = "\\S+")) %>%
    dplyr::mutate(stDir = ifelse(stringr::word(stName, 1) %in% dirCardinal,
                                 stringr::str_sub(stName, 1, 1), stDir)) %>%
    dplyr::mutate(stDir = ifelse(stringr::word(stName, 1) %in% dirInterCardinal,
                                  toupper(stringr::word(stName, 1)), stDir)) %>%
    dplyr::mutate(stDir = ifelse(stringr::word(stName, 1) == "Northwest", "NW", stDir)) %>%
    dplyr::mutate(stDir = ifelse(stringr::word(stName, 1) == "Northeast", "NE", stDir)) %>%
    dplyr::mutate(stDir = ifelse(stringr::word(stName, 1) == "Southwest", "SW", stDir)) %>%
    dplyr::mutate(stDir = ifelse(stringr::word(stName, 1) == "Southeast", "SE", stDir)) %>%
    dplyr::mutate(stName = ifelse(stringr::word(stName, 1) %in% dirAll,
                                  stringr::word(stName, start = 2, end = count),
                                  stName)) %>%
    dplyr::select(-count) -> .data

  # suffix direction
  .data %>%
    dplyr::mutate(count = stringr::str_count(stName, pattern = "\\S+")) %>%
    dplyr::mutate(stSufDir = ifelse(stType %in% dirCardinalA, stType, NA)) %>%
    dplyr::mutate(stSufDir = ifelse(stType %in% dirCardinal,
                                    stringr::str_sub(stType, 1, 1), stSufDir)) %>%
    dplyr::mutate(stSufDir = ifelse(stType %in% dirInterCardinal, toupper(stType), stSufDir)) %>%
    dplyr::mutate(stSufDir = ifelse(stType == "Northwest", "NW", stSufDir)) %>%
    dplyr::mutate(stSufDir = ifelse(stType == "Northeast", "NE", stSufDir)) %>%
    dplyr::mutate(stSufDir = ifelse(stType == "Southwest", "SW", stSufDir)) %>%
    dplyr::mutate(stSufDir = ifelse(stType == "Southeast", "SE", stSufDir)) %>%
    dplyr::mutate(stType = ifelse(!is.na(stSufDir), stringr::word(stName, -1), stType)) %>%
    dplyr::mutate(stName = ifelse(!is.na(stSufDir), stringr::word(stName, start = 1, end = count-1), stName)) %>%
    dplyr::select(-count) -> .data

  # apply local area corrections
  if (locale == 29510){
    .data %>%
      dplyr::mutate(stName = ifelse(stringr::word(stFull, -1) == "Broadway",
                                    "Broadway", stName)) %>%
      dplyr::mutate(stType = ifelse(stType == "Broadway", NA, stType)) -> .data
  }

  # standardize street names and types
  .data %>%
    gw_stName(street = stName) %>%
    gw_suffix(suffix = stType) -> .data

  # re-construct full address
  if (houseNum == TRUE) {
    .data <- dplyr::mutate(.data, stFull = paste(houseNum, stDir, stName, stType, stSufDir, sep = " "))
  } else if (houseNum == FALSE) {
    .data <- dplyr::mutate(.data, stFull = paste(stDir, stName, stType, stSufDir, sep = " "))
    .data$stFull <- stringr::str_replace(.data$stFull, "NA ", " ")
  }

  # remove NAs
  .data %>%
    dplyr::mutate(stFull = stringr::str_replace(stFull, " NA ", " ")) %>%
    dplyr::mutate(stFull = ifelse(stringr::word(stFull, start = -1, end = -1) == "NA",
                                  stringr::str_replace(stFull, "NA", ""), stFull)) %>%
    dplyr::mutate(stFull = trimws(stFull)) -> .data

  # if overwrite is TRUE, replace original address variable with stFull
  if (overwrite == TRUE){
    .data %>%
      dplyr::mutate(!!varQ := stFull) %>%
      dplyr::select(-stFull) -> .data
  }

  # if keepVars is FALSE, remove all of the parsed address components
  if (keepVars == FALSE & houseNum == TRUE) {
    .data <- dplyr::select(.data, -c(houseNum, houseNumL, houseNumU, stDir, stName, stType, stSufDir))
  } else if (keepVars == FALSE & houseNum == FALSE) {
    .data <- dplyr::select(.data, -c(stDir, stName, stType, stSufDir))
  }

  # keeVars is TRUE, check to see if all address components are needed
  if (keepVars == TRUE & houseNum == TRUE) {
    if (all(is.na(.data$houseNumU)) == TRUE) {
      .data %>%
        dplyr::select(-c(houseNumL, houseNumU)) %>%
        dplyr::mutate(houseNum = as.numeric(houseNum)) -> .data
    }
  }

  # return
  return(.data)
}
