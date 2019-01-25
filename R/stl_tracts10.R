#' City of St. Louis Census Tracts, 2010
#'
#' @description An \code{sf} object that contains a polygon for each 2010 tract in St. Louis.
#'
#' @docType data
#'
#' @usage data(stl_tracts10)
#'
#' @format A tibble with 106 rows and 5 variables:
#' \describe{
#'   \item{GEOID}{identification number for feature}
#'   \item{TRACTCE}{census tract number}
#'   \item{ALAND}{land area in meters^2}
#'   \item{AWATER}{water area in meters^2}
#'   \item{geometry}{\code{sf} object geometric data}
#'   }
#'
#' @source U.S. Census Bureau via \code{tigris} package
#'
#' @examples
#' str(stl_tracts10)
#' head(stl_tracts10)
#'
"stl_tracts10"
