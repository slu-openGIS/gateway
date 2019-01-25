#' City of St. Louis Wards, 2010
#'
#' @description An \code{sf} object that contains a polygon for each 2010 ward in St. Louis.
#'
#' @docType data
#'
#' @usage data(stl_wards10)
#'
#' @format A tibble with 28 rows and 3 variables:
#' \describe{
#'   \item{ID}{ward number}
#'   \item{AREA}{area in meters^2}
#'   \item{geometry}{\code{sf} object geometric data}
#'   }
#'
#' @source City of St. Louis
#'
#' @examples
#' str(stl_wards10)
#' head(stl_wards10)
#'
"stl_wards10"
