#' City of St. Louis Boundary
#'
#' @description An \code{sf} object that contains a single polygon for the entire city.
#'
#' @docType data
#'
#' @usage data(stlCity)
#'
#' @format A tibble with 1 rows and 5 variables:
#' \describe{
#'   \item{GEOID}{identification number for feature}
#'   \item{NAMELSAD}{string name for feature}
#'   \item{ALAND}{land area in meters^2}
#'   \item{AWATER}{water area in meters^2}
#'   \item{geometry}{\code{sf} object geometric data}
#'   }
#'
#' @source U.S. Census Bureau via \code{tigris} package
#'
#' @examples
#' str(stlCity)
#' head(stlCity)
#'
"stlCity"
