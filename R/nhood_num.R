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
#'
#' @source \href{https://www.stlouis-mo.gov/government/departments/planning/documents/citywide-neighborhood-map.cfm}{City of St. Louis Citywide Neighborhood Map}
#'
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'
#'     neighborhoods <- data.frame(
#'         id = c(1, 2, 3, 4, 5),
#'         nhoodNum = c(1, 2, 4, 5, 3)
#'     )
#'
#'     neighborhoods <- dplyr::mutate(neighborhoods, nhood = nhood_Str(nhoodNum, asFactor = TRUE))
#'     table(neighborhoods$nhood)
#' }
#'
#' @export
nhood_num <- function(var, asFactor = TRUE) {
  corrected <- lettercase::str_ucfirst(var)

  nhood <- ifelse(corrected == "Carondelet", 1, NA)
}

