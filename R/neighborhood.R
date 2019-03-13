#' Neighborhood Indentifier Conversion
#'
#' @description Provides a technique for converting neighborhood identifiers between
#'     their numeric, string, and factor formats.
#'
#' @usage gw_nhood(.data, var, new_var, to)
#'
#' @param .data Data frame, tibble, or \code{sf} object to be modified
#' @param var Variable to base conversions on
#' @param new_var Name of new variable; if none is specified, the \code{var} variable
#'     will be overwritten
#' @param to The output format of the data \code{var} (one of either \code{"numeric"} or
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

  # global binding
  ...nhood = NULL

  # unquote
  varQ <- rlang::enquo(var)
  newVarQ <- rlang::enquo(new_var)

  # convert to title case
  .data <- dplyr::mutate(.data, ...nhood := stringr::str_to_title(!!varQ))

  # convert values
  .data <- dplyr::mutate(.data, !!newVarQ := dplyr::case_when(
    ...nhood == "Carondelet" ~ 1,
    ...nhood == "Patch" ~ 2,
    ...nhood == "Holly Hills" ~ 3,
    ...nhood == "Boulevard Heights" ~ 4,
    ...nhood == "Bevo Mill" | ...nhood == "Bevo" ~ 5,
    ...nhood == "Princeton Heights" ~ 6,
    ...nhood == "South Hampton" ~ 7,
    ...nhood == "St. Louis Hills" ~ 8,
    ...nhood == "Lindenwood Park" ~ 9,
    ...nhood == "Ellendale" ~ 10,
    ...nhood == "Clifton Heights" ~ 11,
    ...nhood == "The Hill" ~ 12,
    ...nhood == "Southwest Garden" | ...nhood == "Sw Garden" ~ 13,
    ...nhood == "North Hampton" ~ 14,
    ...nhood == "Tower Grove South" | ...nhood == "TGS" ~ 15,
    ...nhood == "Dutchtown" ~ 16,
    ...nhood == "Mount Pleasant" ~ 17,
    ...nhood == "Marine Villa" ~ 18,
    ...nhood == "Gravois Park" ~ 19,
    ...nhood == "Kosciusko" ~ 20,
    ...nhood == "Soulard" ~ 21,
    ...nhood == "Benton Park" ~ 22,
    ...nhood == "McKinley Heights" ~ 23,
    ...nhood == "Fox Park" ~ 24,
    ...nhood == "Tower Grove East" | ...nhood == "TGE" ~ 25,
    ...nhood == "Compton Heights" ~ 26,
    ...nhood == "Shaw" ~ 27,
    ...nhood == "Botanical Heights" ~ 28,
    ...nhood == "Tiffany" ~ 29,
    ...nhood == "Benton Park West" ~ 30,
    ...nhood == "The Gate District" | ...nhood == "The Gate" ~ 31,
    ...nhood == "Lafayette Square" | ...nhood == "Lafayette Sq" | ...nhood == "Lafayette Sq." ~ 32,
    ...nhood == "Peabody Darst Webbe" | ...nhood == "Peabody" ~ 33,
    ...nhood == "LaSalle Park" ~ 34,
    ...nhood == "Downtown" ~ 35,
    ...nhood == "Downtown West" ~ 36,
    ...nhood == "Midtown" ~ 37,
    ...nhood == "Central West End" | ...nhood == "CWE" | ...nhood == "C.W.E." ~ 38,
    ...nhood == "Forest Park South East" | ...nhood == "FPSE" | ...nhood == "The Grove" | ...nhood == "Grove" | ...nhood == "Forest Park Southeast" ~ 39,
    ...nhood == "Kings Oak" ~ 40,
    ...nhood == "Cheltenham" ~ 41,
    ...nhood == "Clayton-Tamm" ~ 42,
    ...nhood == "Franz Park" ~ 43,
    ...nhood == "Hi-Pointe" | ...nhood == "Hi Pointe" | ...nhood == "Hi Point" | ...nhood == "Hi-Point" ~ 44,
    ...nhood == "Wydown Skinker" ~ 45,
    ...nhood == "Skinker DeBaliviere" ~ 46,
    ...nhood == "Debaliviere Place" ~ 47,
    ...nhood == "West End" ~ 48,
    ...nhood == "Visitation Park" ~ 49,
    ...nhood == "Wells Goodfellow" ~ 50,
    ...nhood == "Academy" ~ 51,
    ...nhood == "Kingsway West" ~ 52,
    ...nhood == "Fountain Park" ~ 53,
    ...nhood == "Lewis Place" ~ 54,
    ...nhood == "Kingsway East" ~ 55,
    ...nhood == "Greater Ville" ~ 56,
    ...nhood == "The Ville" ~ 57,
    ...nhood == "Vandeventer" ~ 58,
    ...nhood == "Jeff Vanderlou" | ...nhood == "JVL" ~ 59,
    ...nhood == "St. Louis Place" | ...nhood == "St Louis Place" | ...nhood == "Saint Louis Place" ~ 60,
    ...nhood == "Carr Square" ~ 61,
    ...nhood == "Columbus Square" ~ 62,
    ...nhood == "Old North St. Louis" | ...nhood == "Old North" | ...nhood == "Old North St Louis" | ...nhood == "Old North Saint Louis" ~ 63,
    ...nhood == "Near North Riverfront" | ...nhood == "Near North" ~ 64,
    ...nhood == "Hyde Park" ~ 65,
    ...nhood == "College Hill" ~ 66,
    ...nhood == "Fairground Neighborhood" | ...nhood == "Fairground" ~ 67,
    ...nhood == "O'Fallon" ~ 68,
    ...nhood == "Penrose" ~ 69,
    ...nhood == "Mark Twain I-70 Industrial" | ...nhood == "Mark Twain Industrial" ~ 70,
    ...nhood == "Mark Twain" ~ 71,
    ...nhood == "Walnut Park East" ~ 72,
    ...nhood == "North Pointe" | ...nhood == "North Point" ~ 73,
    ...nhood == "Baden" ~ 74,
    ...nhood == "Riverview" ~ 75,
    ...nhood == "Walnut Park West" ~ 76,
    ...nhood == "Covenant Blu-Grand Center" | ...nhood == "Grand Center" ~ 77,
    ...nhood == "Hamilton Heights" ~ 78,
    ...nhood == "North Riverfront" ~ 79))

  # remove ...nhood
  .data <- dplyr::select(.data, -...nhood)

  # return output
  return(.data)

}


# String or factor neighborhood names
#
gw_nhood_str <- function(.data, var, new_var){

  # unquote
  varQ <- rlang::enquo(var)
  newVarQ <- rlang::enquo(new_var)

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
