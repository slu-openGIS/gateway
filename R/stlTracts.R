#' City of St. Louis Census Tract Boundaries
#'
#' @description An \code{sf} object that contains polygon features for all census tracts within the city.
#'
#' @docType data
#'
#' @usage data(stlTracts)
#'
#' @format A tibble with 106 rows and 5 variables:
#' \describe{
#'   \item{GEOID}{identification number for feature}
#'   \item{TRACTCE}{short identification number without state and county codes}
#'   \item{ALAND}{land area in meters^2}
#'   \item{AWATER}{water area in meters^2}
#'   \item{geometry}{\code{sf} object geometric data}
#'   }
#'
#' @source U.S. Census Bureau via \code{tigris} package
#'
#' @examples
#' str(stlTracts)
#' head(stlTracts)
#'
"stlTracts"
