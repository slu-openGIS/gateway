#' City of St. Louis Neighborhoods
#'
#' @description An \code{sf} object that contains a polygon for each neighborhood in St. Louis.
#'
#' @note Neighborhoods with identification numbers greater than 79 are not residential neighborhoods
#'     but rather are large parks without a residential population.
#'
#' @docType data
#'
#' @usage data(stl_nhoods)
#'
#' @format A tibble with 88 rows and 3 variables:
#' \describe{
#'   \item{ID}{identification number for feature}
#'   \item{NAME}{neighborhood nham}
#'   \item{AREA}{area in meters^2}
#'   \item{geometry}{\code{sf} object geometric data}
#'   }
#'
#' @source City of St. Louis
#'
#' @examples
#' str(stl_nhoods)
#' head(stl_nhoods)
#'
"stl_nhoods"
