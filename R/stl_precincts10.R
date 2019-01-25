#' City of St. Louis Precincts, 2010
#'
#' @description An \code{sf} object that contains a polygon for each 2010 precinct in St. Louis.
#'
#' @docType data
#'
#' @usage data(stl_precincts10)
#'
#' @format A tibble with 233 rows and 4 variables:
#' \describe{
#'   \item{HANDLE}{identification number for feature}
#'   \item{PREC10}{precinct number}
#'   \item{WARD10}{ward number}
#'   \item{AREA}{area in meters^2}
#'   \item{geometry}{\code{sf} object geometric data}
#'   }
#'
#' @source City of St. Louis
#'
#' @examples
#' str(stl_precincts10)
#' head(stl_precincts10)
#'
"stl_precincts10"
