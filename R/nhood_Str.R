#' String or factor neighborhood names
#'
#' \code{nhood_Str} returns the matching neighborhood name in string or factor format
#'
#' This is a function for converting a vector of City of St. Louis neighborhood numbers
#' to either string or factor format.
#'
#' @usage nhood_Str(var, asFactor = TRUE)
#'
#' @param var A numeric or integer vector containing City of St. Louis neighborhood numbers
#'
#' @param asFactor A logical scalar. Should the output be converted to a factor?
#'
#' @source \href{https://www.stlouis-mo.gov/government/departments/planning/documents/citywide-neighborhood-map.cfm}{City of St. Louis Citywide Neighborhood Map}
#'
#' @examples
#' if (require("dplyr") {
#'     neighborhoods <- data.frame(
#'         id = c(1, 2, 3, 4, 5),
#'         nhoodNum = c(1, 2, 4, 5, 3)
#'     )
#'
#' neighborhoods <- dplyr::mutate(neighborhoods, nhood = nhood_Str(nhoodNum, asFactor = TRUE))
#' }
#'
#' @export
nhood_Str <- function(var, asFactor = TRUE) {
  nhood <- ifelse(var == 1, "Carondelet", NA)
  nhood <- ifelse(var == 2, "Patch", nhood)
  nhood <- ifelse(var == 3, "Holly Hills", nhood)
  nhood <- ifelse(var == 4, "Boulevard Heights", nhood)
  nhood <- ifelse(var == 5, "Bevo Mill", nhood)
  nhood <- ifelse(var == 6, "Princeton Heights", nhood)
  nhood <- ifelse(var == 7, "South Hampton", nhood)
  nhood <- ifelse(var == 8, "St. Louis Hills", nhood)
  nhood <- ifelse(var == 9, "Lindenwood Park", nhood)
  nhood <- ifelse(var == 10, "Ellendale", nhood)
  nhood <- ifelse(var == 11, "Clifton Heights", nhood)
  nhood <- ifelse(var == 12, "The Hill", nhood)
  nhood <- ifelse(var == 13, "Southwest Garden", nhood)
  nhood <- ifelse(var == 14, "North Hampton", nhood)
  nhood <- ifelse(var == 15, "Tower Grove South", nhood)
  nhood <- ifelse(var == 16, "Dutchtown", nhood)
  nhood <- ifelse(var == 17, "Mount Pleasant", nhood)
  nhood <- ifelse(var == 18, "Marine Villa", nhood)
  nhood <- ifelse(var == 19, "Gravois Park", nhood)
  nhood <- ifelse(var == 20, "Kosciusko", nhood)
  nhood <- ifelse(var == 21, "Soulard", nhood)
  nhood <- ifelse(var == 22, "Benton Park", nhood)
  nhood <- ifelse(var == 23, "McKinley Heights", nhood)
  nhood <- ifelse(var == 24, "Fox Park", nhood)
  nhood <- ifelse(var == 25, "Tower Grove East", nhood)
  nhood <- ifelse(var == 26, "Compton Heights", nhood)
  nhood <- ifelse(var == 27, "Shaw", nhood)
  nhood <- ifelse(var == 28, "Botanical Heights", nhood)
  nhood <- ifelse(var == 29, "Tiffany", nhood)
  nhood <- ifelse(var == 30, "Benton Park West", nhood)
  nhood <- ifelse(var == 31, "The Gate District", nhood)
  nhood <- ifelse(var == 32, "Lafayette Square", nhood)
  nhood <- ifelse(var == 33, "Peabody Darst Webbe", nhood)
  nhood <- ifelse(var == 34, "LaSalle Park", nhood)
  nhood <- ifelse(var == 35, "Downtown", nhood)
  nhood <- ifelse(var == 36, "Downtown West", nhood)
  nhood <- ifelse(var == 37, "Midtown", nhood)
  nhood <- ifelse(var == 38, "Central West End", nhood)
  nhood <- ifelse(var == 39, "Forest Park South East", nhood)
  nhood <- ifelse(var == 40, "Kings Oak", nhood)
  nhood <- ifelse(var == 41, "Cheltenham", nhood)
  nhood <- ifelse(var == 42, "Clayton-Tamm", nhood)
  nhood <- ifelse(var == 43, "Franz Park", nhood)
  nhood <- ifelse(var == 44, "Hi-Pointe", nhood)
  nhood <- ifelse(var == 45, "Wydown Skinker", nhood)
  nhood <- ifelse(var == 46, "Skinker DeBaliviere", nhood)
  nhood <- ifelse(var == 47, "DeBaliviere Place", nhood)
  nhood <- ifelse(var == 48, "West End", nhood)
  nhood <- ifelse(var == 49, "Visitation Park", nhood)
  nhood <- ifelse(var == 50, "Wells Goodfellow", nhood)
  nhood <- ifelse(var == 51, "Academy", nhood)
  nhood <- ifelse(var == 52, "Kingsway West", nhood)
  nhood <- ifelse(var == 53, "Fountain Park", nhood)
  nhood <- ifelse(var == 54, "Lewis Place", nhood)
  nhood <- ifelse(var == 55, "Kingsway East", nhood)
  nhood <- ifelse(var == 56, "Greater Ville", nhood)
  nhood <- ifelse(var == 57, "The Ville", nhood)
  nhood <- ifelse(var == 58, "Vandeventer", nhood)
  nhood <- ifelse(var == 59, "Jeff Vanderlou", nhood)
  nhood <- ifelse(var == 60, "St. Louis Place", nhood)
  nhood <- ifelse(var == 61, "Carr Square", nhood)
  nhood <- ifelse(var == 62, "Columbus Square", nhood)
  nhood <- ifelse(var == 63, "Old North St. Louis", nhood)
  nhood <- ifelse(var == 64, "Near North Riverfront", nhood)
  nhood <- ifelse(var == 65, "Hyde Park", nhood)
  nhood <- ifelse(var == 66, "College Hill", nhood)
  nhood <- ifelse(var == 67, "Fairground Neighborhood", nhood)
  nhood <- ifelse(var == 68, "O'Fallon", nhood)
  nhood <- ifelse(var == 69, "Penrose", nhood)
  nhood <- ifelse(var == 70, "Mark Twain I-70 Industrial", nhood)
  nhood <- ifelse(var == 71, "Mark Twain", nhood)
  nhood <- ifelse(var == 72, "Walnut Park East", nhood)
  nhood <- ifelse(var == 73, "North Pointe", nhood)
  nhood <- ifelse(var == 74, "Baden", nhood)
  nhood <- ifelse(var == 75, "Riverview", nhood)
  nhood <- ifelse(var == 76, "Walnut Park West", nhood)
  nhood <- ifelse(var == 77, "Covenant Blu-Grand Center", nhood)
  nhood <- ifelse(var == 78, "Hamilton Heights", nhood)
  nhood <- ifelse(var == 79, "North Riverfront", nhood)

  if (asFactor == TRUE) {
    nhood <- as.factor(nhood)
  }

  return(nhood)
}
