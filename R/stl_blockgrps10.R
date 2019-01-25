#' City of St. Louis Census Block Groups, 2010
#'
#' @description An \code{sf} object that contains a polygon for each 2010 block group in St. Louis.
#'
#' @docType data
#'
#' @usage data(stl_blockgrps10)
#'
#' @format A tibble with 360 rows and 6 variables:
#' \describe{
#'   \item{GEOID}{identification number for feature}
#'   \item{TRACTCE}{census tract associated with feature}
#'   \item{BLKGRPCE}{block group number}
#'   \item{ALAND}{land area in meters^2}
#'   \item{AWATER}{water area in meters^2}
#'   \item{geometry}{\code{sf} object geometric data}
#'   }
#'
#' @source U.S. Census Bureau via \code{tigris} package
#'
#' @examples
#' str(stl_blockgrps10)
#' head(stl_blockgrps10)
#'
"stl_blockgrps10"
