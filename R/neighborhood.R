#' Neighborhood Indentifier Conversion
#'
#' @description Provides a technique for converting neighborhood identifiers between
#'     their numeric, string, and factor formats.
#'
#' @usage gw_nhood(.data, var, new_var, from, to)
#'
#' @param .data Data frame, tibble, or \code{sf} object to be modified
#' @param var Variable to base conversions on
#' @param new_var Name of new variable; if none is specified, the \code{var} variable
#'     will be overwritten
#' @param from The output format of the data \code{var} (one of either \code{"numeric"} or
#'     \code{"string"}).
#'
#' @export
gw_nhood <- function(.data, var, new_var, to){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # unquote new_var
  if (missing(new_var) == TRUE){
    overwrite <- TRUE

    newVarQ <- rlang::enquo(varQ)

  } else if (missing(new_var) == FALSE){
    overwrite <- FALSE

    if (!is.character(paramList$new_var)) {
      newVarQ <- rlang::enquo(new_var)
    } else if (is.character(paramList$new_var)) {
      newVarQ <- rlang::quo(!! rlang::sym(new_var))
    }

  }


  # convert
  if (to == "string"){
    .data <- gw_nhood_str(.data, var = !!varQ, new_var = !!newVarQ)
  } else if (to == "numeric"){
    .data <- gw_nhood_num(.data, var = !!varQ, new_var = !!newVarQ)
  }

  # return output
  return(.data)

}


# Convert to numeric neighborhood numbers
#
#
gw_nhood_num <- function(.data, var, new_var){

  # unquote
  varQ <- rlang::enquo(var)
  newVarQ <- rlang::enquo(new_var)

  # convert to title case
  .data <- dplyr::mutate(.data, !!newVarQ := stringr::str_to_title(!!varQ))

  # convert values
  .data <- dplyr::mutate(.data, !!newVarQ := dplyr::case_when(
    !!varQ == "Carondelet" ~ 1,
    !!varQ == "Patch" ~ 2,
    !!varQ == "Holly Hills" ~ 3,
    !!varQ == "Boulevard Heights" ~ 4,
    !!varQ == "Bevo Mill" | !!varQ == "Bevo" ~ 5,
    !!varQ == "Princeton Heights" ~ 6,
    !!varQ == "South Hampton" ~ 7,
    !!varQ == "St. Louis Hills" ~ 8,
    !!varQ == "Lindenwood Park" ~ 9,
    !!varQ == "Ellendale" ~ 10,
    !!varQ == "Clifton Heights" ~ 11,
    !!varQ == "The Hill" ~ 12,
    !!varQ == "Southwest Garden" | !!varQ == "Sw Garden" ~ 13,
    !!varQ == "North Hampton" ~ 14,
    !!varQ == "Tower Grove South" | !!varQ == "TGS" ~ 15,
    !!varQ == "Dutchtown" ~ 16,
    !!varQ == "Mount Pleasant" ~ 17,
    !!varQ == "Marine Villa" ~ 18,
    !!varQ == "Gravois Park" ~ 19,
    !!varQ == "Kosciusko" ~ 20,
    !!varQ == "Soulard" ~ 21,
    !!varQ == "Benton Park" ~ 22,
    !!varQ == "McKinley Heights" ~ 23,
    !!varQ == "Fox Park" ~ 24,
    !!varQ == "Tower Grove East" | !!varQ == "TGE" ~ 25,
    !!varQ == "Compton Heights" ~ 26,
    !!varQ == "Shaw" ~ 27,
    !!varQ == "Botanical Heights" ~ 28,
    !!varQ == "Tiffany" ~ 29,
    !!varQ == "Benton Park West" ~ 30,
    !!varQ == "The Gate District" | !!varQ == "The Gate" ~ 31,
    !!varQ == "Lafayette Square" | !!varQ == "Lafayette Sq" | !!varQ == "Lafayette Sq." ~ 32,
    !!varQ == "Peabody Darst Webbe" | !!varQ == "Peabody" ~ 33,
    !!varQ == "LaSalle Park" ~ 34,
    !!varQ == "Downtown" ~ 35,
    !!varQ == "Downtown West" ~ 36,
    !!varQ == "Midtown" ~ 37,
    !!varQ == "Central West End" | !!varQ == "CWE" | !!varQ == "C.W.E." ~ 38,
    !!varQ == "Forest Park South East" | !!varQ == "FPSE" | !!varQ == "The Grove" | !!varQ == "Grove" | !!varQ == "Forest Park Southeast" ~ 39,
    !!varQ == "Kings Oak" ~ 40,
    !!varQ == "Cheltenham" ~ 41,
    !!varQ == "Clayton-Tamm" ~ 42,
    !!varQ == "Franz Park" ~ 43,
    !!varQ == "Hi-Pointe" | !!varQ == "Hi Pointe" | !!varQ == "Hi Point" | !!varQ == "Hi-Point" ~ 44,
    !!varQ == "Wydown Skinker" ~ 45,
    !!varQ == "Skinker DeBaliviere" ~ 46,
    !!varQ == "Debaliviere Place" ~ 47,
    !!varQ == "West End" ~ 48,
    !!varQ == "Visitation Park" ~ 49,
    !!varQ == "Wells Goodfellow" ~ 50,
    !!varQ == "Academy" ~ 51,
    !!varQ == "Kingsway West" ~ 52,
    !!varQ == "Fountain Park" ~ 53,
    !!varQ == "Lewis Place" ~ 54,
    !!varQ == "Kingsway East" ~ 55,
    !!varQ == "Greater Ville" ~ 56,
    !!varQ == "The Ville" ~ 57,
    !!varQ == "Vandeventer" ~ 58,
    !!varQ == "Jeff Vanderlou" | !!varQ == "JVL" ~ 59,
    !!varQ == "St. Louis Place" | !!varQ == "St Louis Place" | !!varQ == "Saint Louis Place" ~ 60,
    !!varQ == "Carr Square" ~ 61,
    !!varQ == "Columbus Square" ~ 62,
    !!varQ == "Old North St. Louis" | !!varQ == "Old North" | !!varQ == "Old North St Louis" | !!varQ == "Old North Saint Louis" ~ 63,
    !!varQ == "Near North Riverfront" | !!varQ == "Near North" ~ 64,
    !!varQ == "Hyde Park" ~ 65,
    !!varQ == "College Hill" ~ 66,
    !!varQ == "Fairground Neighborhood" | !!varQ == "Fairground" ~ 67,
    !!varQ == "O'Fallon" ~ 68,
    !!varQ == "Penrose" ~ 69,
    !!varQ == "Mark Twain I-70 Industrial" | !!varQ == "Mark Twain Industrial" ~ 70,
    !!varQ == "Mark Twain" ~ 71,
    !!varQ == "Walnut Park East" ~ 72,
    !!varQ == "North Pointe" | !!varQ == "North Point" ~ 73,
    !!varQ == "Baden" ~ 74,
    !!varQ == "Riverview" ~ 75,
    !!varQ == "Walnut Park West" ~ 76,
    !!varQ == "Covenant Blu-Grand Center" | !!varQ == "Grand Center" ~ 77,
    !!varQ == "Hamilton Heights" ~ 78,
    !!varQ == "North Riverfront" ~ 79))

  # return output
  return(.data)

}


# String or factor neighborhood names
#
# \code{nhood_str} is a function for converting a vector of City of St. Louis neighborhood numbers
# to either character or factor format.
#
# @usage nhood_str(var, asFactor = TRUE)
#
# @param var A numeric or integer vector containing City of St. Louis neighborhood numbers
#
# @param asFactor A logical scalar. Should the output be converted to a factor?
#
# @return A character or factor vector
#
# @source \href{https://www.stlouis-mo.gov/government/departments/planning/documents/citywide-neighborhood-map.cfm}{City of St. Louis Citywide Neighborhood Map}
#
#


gw_nhood_str <- function(.data, var, new_var){

  # unquote
  varQ <- rlang::enquo(var)
  newVarQ <- rlang::enquo(new_var)

  # convert to title case
  .data <- dplyr::mutate(.data, !!newVarQ := stringr::str_to_title(!!varQ))

  # convert values
  .data <- dplyr::mutate(.data, !!newVarQ := dplyr::case_when(
    !!varQ == 1 ~ "Carondelet",
    !!varQ == 2 ~ "Patch",
    !!varQ == 3 ~ "Holly Hills",
    !!varQ == 4 ~ "Boulevard Heights",
    !!varQ == 5 ~ "Bevo Mill",
    !!varQ == 6 ~ "Princeton Heights",
    !!varQ == 7 ~ "South Hampton",
    !!varQ == 8 ~ "St. Louis Hills",
    !!varQ == 9 ~ "Lindenwood Park",
    !!varQ == 10 ~ "Ellendale",
    !!varQ == 11 ~ "Clifton Heights",
    !!varQ == 12 ~ "The Hill",
    !!varQ == 13 ~ "Southwest Garden",
    !!varQ == 14 ~ "North Hampton",
    !!varQ == 15 ~ "Tower Grove South",
    !!varQ == 16 ~ "Dutchtown",
    !!varQ == 17 ~ "Mount Pleasant",
    !!varQ == 18 ~ "Marine Villa",
    !!varQ == 19 ~ "Gravois Park",
    !!varQ == 20 ~ "Kosciusko",
    !!varQ == 21 ~ "Soulard",
    !!varQ == 22 ~ "Benton Park",
    !!varQ == 23 ~ "McKinley Heights",
    !!varQ == 24 ~ "Fox Park",
    !!varQ == 25 ~ "Tower Grove East",
    !!varQ == 26 ~ "Compton Heights",
    !!varQ == 27 ~ "Shaw",
    !!varQ == 28 ~ "Botanical Heights",
    !!varQ == 29 ~ "Tiffany",
    !!varQ == 30 ~ "Benton Park West",
    !!varQ == 31 ~ "The Gate District",
    !!varQ == 32 ~ "Lafayette Square",
    !!varQ == 33 ~ "Peabody Darst Webbe",
    !!varQ == 34 ~ "LaSalle Park",
    !!varQ == 35 ~ "Downtown",
    !!varQ == 36 ~ "Downtown West",
    !!varQ == 37 ~ "Midtown",
    !!varQ == 38 ~ "Central West End",
    !!varQ == 39 ~ "Forest Park South East",
    !!varQ == 40 ~ "Kings Oak",
    !!varQ == 41 ~ "Cheltenham",
    !!varQ == 42 ~ "Clayton-Tamm",
    !!varQ == 43 ~ "Franz Park",
    !!varQ == 44 ~ "Hi-Pointe",
    !!varQ == 45 ~ "Wydown Skinker",
    !!varQ == 46 ~ "Skinker DeBaliviere",
    !!varQ == 47 ~ "DeBaliviere Place",
    !!varQ == 48 ~ "West End",
    !!varQ == 49 ~ "Visitation Park",
    !!varQ == 50 ~ "Wells Goodfellow",
    !!varQ == 51 ~ "Academy",
    !!varQ == 52 ~ "Kingsway West",
    !!varQ == 53 ~ "Fountain Park",
    !!varQ == 54 ~ "Lewis Place",
    !!varQ == 55 ~ "Kingsway East",
    !!varQ == 56 ~ "Greater Ville",
    !!varQ == 57 ~ "The Ville",
    !!varQ == 58 ~ "Vandeventer",
    !!varQ == 59 ~ "Jeff Vanderlou",
    !!varQ == 60 ~ "St. Louis Place",
    !!varQ == 61 ~ "Carr Square",
    !!varQ == 62 ~ "Columbus Square",
    !!varQ == 63 ~ "Old North St. Louis",
    !!varQ == 64 ~ "Near North Riverfront",
    !!varQ == 65 ~ "Hyde Park",
    !!varQ == 66 ~ "College Hill",
    !!varQ == 67 ~ "Fairground Neighborhood",
    !!varQ == 68 ~ "O'Fallon",
    !!varQ == 69 ~ "Penrose",
    !!varQ == 70 ~ "Mark Twain I-70 Industrial",
    !!varQ == 71 ~ "Mark Twain",
    !!varQ == 72 ~ "Walnut Park East",
    !!varQ == 73 ~ "North Pointe",
    !!varQ == 74 ~ "Baden",
    !!varQ == 75 ~ "Riverview",
    !!varQ == 76 ~ "Walnut Park West",
    !!varQ == 77 ~ "Covenant Blu-Grand Center",
    !!varQ == 78 ~ "Hamilton Heights",
    !!varQ == 79 ~ "North Riverfront"))

  # return output
  return(.data)

}
