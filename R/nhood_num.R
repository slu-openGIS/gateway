#' Numeric neighborhood numbers
#'
#' \code{nhood_num} returns the matching neighborhood name in string or factor format
#'
#' This is a function for converting a vector of City of St. Louis neighborhood numbers
#' to either string or factor format.
#'
#' @usage nhood_Num(var)
#'
#' @param var A string vector containing City of St. Louis neighborhood names
#'
#' @note Function is capable of handeling vectors that contain various spellings (i.e. "carondelet" or "Carondelet").
#' It is also capable of handleing some common neighborhood abbreviations, such as "Bevo", "CWE", "FPSE", and "The Grove".
#'
#' @source \href{https://www.stlouis-mo.gov/government/departments/planning/documents/citywide-neighborhood-map.cfm}{City of St. Louis Citywide Neighborhood Map}
#'
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'
#'     neighborhoods <- data.frame(
#'         id = c(1, 2, 3, 4, 5),
#'         nhoodStr = c("Patch", "Bevo", "Bevo Mill", "Lindenwood Park", "Carondelet")
#'     )
#'
#'     neighborhoods <- dplyr::mutate(neighborhoods, nhood = nhood_num(nhoodStr))
#'     table(neighborhoods$nhood)
#' }
#'
#' @export
nhood_num <- function(var, asFactor = TRUE) {
  corrected <- lettercase::str_ucfirst(var)

  nhood <- ifelse(corrected == "Carondelet", 1, NA)
  nhood <- ifelse(corrected == "Patch", 2, NA)
  nhood <- ifelse(corrected == "Holly Hills", 3, NA)
  nhood <- ifelse(corrected == "Boulevard Heights", 4, nhood)
  nhood <- ifelse(corrected == "Bevo Mill" |
                    corrected == "Bevo", 5, nhood)
  nhood <- ifelse(corrected == "Princeton Heights", 6, nhood)
  nhood <- ifelse(corrected == "South Hampton", 7, nhood)
  nhood <- ifelse(corrected == "St. Louis Hills", 8, nhood)
  nhood <- ifelse(corrected == "Lindenwood Park", 9, nhood)
  nhood <- ifelse(corrected == "Ellendale", 10, nhood)
  nhood <- ifelse(corrected == "Clifton Heights", 11, nhood)
  nhood <- ifelse(corrected == "The Hill", 12, nhood)
  nhood <- ifelse(corrected == "Southwest Garden", 13, nhood)
  nhood <- ifelse(corrected == "North Hampton", 14, nhood)
  nhood <- ifelse(corrected == "Tower Grove South" |
                    corrected = "TGS", 15, nhood)
  nhood <- ifelse(corrected == "Dutchtown", 16, nhood)
  nhood <- ifelse(corrected == "Mount Pleasant", 17, nhood)
  nhood <- ifelse(corrected == "Marine Villa", 18, nhood)
  nhood <- ifelse(corrected == "Gravois Park", 19, nhood)
  nhood <- ifelse(corrected == "Kosciusko", 20, nhood)
  nhood <- ifelse(corrected == "Soulard", 21, nhood)
  nhood <- ifelse(corrected == "Benton Park", 22, nhood)
  nhood <- ifelse(corrected == "McKinley Heights", 23, nhood)
  nhood <- ifelse(corrected == "Fox Park", 24, nhood)
  nhood <- ifelse(corrected == "Tower Grove East" |
                    corrected == "TGE", 25, nhood)
  nhood <- ifelse(corrected == "Compton Heights", 26, nhood)
  nhood <- ifelse(corrected == "Shaw", 27, nhood)
  nhood <- ifelse(corrected == "Botanical Heights", 28, nhood)
  nhood <- ifelse(corrected == "Tiffany", 29, nhood)
  nhood <- ifelse(corrected == "Benton Park West", 30, nhood)
  nhood <- ifelse(corrected == "The Gate District" |
                    corrected == "The Gate", 31, nhood)
  nhood <- ifelse(corrected == "Lafayette Square" |
                    corrected == "Lafayette Sq" |
                    corrected == "Lafayette Sq.", 32, nhood)
  nhood <- ifelse(corrected == "Peabody Darst Webbe" | corrected == "Peabody", 33, nhood)
  nhood <- ifelse(corrected == "LaSalle Park", 34, nhood)
  nhood <- ifelse(corrected == "Downtown", 35, nhood)
  nhood <- ifelse(corrected == "Downtown West", 36, nhood)
  nhood <- ifelse(corrected == "Midtown", 37, nhood)
  nhood <- ifelse(corrected == "Central West End" |
                    corrected == "CWE" |
                    corrected == "C.W.E.", 38, nhood)
  nhood <- ifelse(corrected == "Forest Park South East" |
                    corrected == "FPSE" |
                    corrected == "The Grove" |
                    corrected == "Grove" |
                    corrected == "Forest Park Southeast", 39, nhood)
  nhood <- ifelse(corrected == "Kings Oak", 40, nhood)
  nhood <- ifelse(corrected == "Cheltenham", 41, nhood)
  nhood <- ifelse(corrected == "Clayton-Tamm", 42, nhood)
  nhood <- ifelse(corrected == "Franz Park", 43, nhood)
  nhood <- ifelse(corrected == "Hi-Pointe" |
                    corrected == "Hi Pointe" |
                    corrected == "Hi Point" |
                    corrected == "Hi-Point", 44, nhood)
  nhood <- ifelse(corrected == "Wydown Skinker", 45, nhood)
  nhood <- ifelse(corrected == "Skinker DeBaliviere", 46, nhood)
  nhood <- ifelse(corrected == "DeBaliviere Place", 47, nhood)
  nhood <- ifelse(corrected == "West End", 48, nhood)
  nhood <- ifelse(corrected == "Visitation Park", 49, nhood)
  nhood <- ifelse(corrected == "Wells Goodfellow", 50, nhood)
  nhood <- ifelse(corrected == "Academy", 51, nhood)
  nhood <- ifelse(corrected == "Kingsway West", 52, nhood)
  nhood <- ifelse(corrected == "Fountain Park", 53, nhood)
  nhood <- ifelse(corrected == "Lewis Place", 54, nhood)
  nhood <- ifelse(corrected == "Kingsway East", 55, nhood)
  nhood <- ifelse(corrected == "Greater Ville", 56, nhood)
  nhood <- ifelse(corrected == "The Ville", 57, nhood)
  nhood <- ifelse(corrected == "Vandeventer", 58, nhood)
  nhood <- ifelse(corrected == "Jeff Vanderlou" |
                    corrected == "JVL", 59, nhood)
  nhood <- ifelse(corrected == "St. Louis Place" |
                    corrected == "St Louis Place" |
                    corrected == "Saint Louis Place", 60, nhood)
  nhood <- ifelse(corrected == "Carr Square", 61, nhood)
  nhood <- ifelse(corrected == "Columbus Square", 62, nhood)
  nhood <- ifelse(corrected == "Old North St. Louis" |
                    corrected == "Old North" |
                    corrected == "Old North St Louis" |
                    corrected == "Old North Saint Louis", 63, nhood)
  nhood <- ifelse(corrected == "Near North Riverfront" |
                    corrected == "Near North", 64, nhood)
  nhood <- ifelse(corrected == "Hyde Park", 65, nhood)
  nhood <- ifelse(corrected == "College Hill", 66, nhood)
  nhood <- ifelse(corrected == "Fairground Neighborhood" |
                    corrected == "Fairground", 67, nhood)
  nhood <- ifelse(corrected == "O'Fallon", 68, nhood)
  nhood <- ifelse(corrected == "Penrose", 69, nhood)
  nhood <- ifelse(corrected == "Mark Twain I-70 Industrial" |
                    corrected == "Mark Twain Industrial", 70, nhood)
  nhood <- ifelse(corrected == "Mark Twain", 71, nhood)
  nhood <- ifelse(corrected == "Walnut Park East", 72, nhood)
  nhood <- ifelse(corrected == "North Pointe" |
                    corrected == "North Point", 73, nhood)
  nhood <- ifelse(corrected == "Baden", 74, nhood)
  nhood <- ifelse(corrected == "Riverview", 75, nhood)
  nhood <- ifelse(corrected == "Walnut Park West", 76, nhood)
  nhood <- ifelse(corrected == "Covenant Blu-Grand Center", 77, nhood)
  nhood <- ifelse(corrected == "Hamilton Heights", 78, nhood)
  nhood <- ifelse(corrected == "North Riverfront", 79, nhood)

  return(nhood)
}

