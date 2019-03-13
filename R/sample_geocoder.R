#' Sample Geocoder, sf Format
#'
#' @description A sample geocoder built with \code{\link{gw_build_geocoder}}. This
#'     geocoder contains both the \code{sf} geometry and \code{addrrecnum} identifier
#'     for every address on a street included in the \code{\link{sushi}} and
#'     \code{\link{sushi_sf}} example data sets.
#'
#' @docType data
#'
#' @usage data(sample_geocoder)
#'
#' @format A tibble with 6050 rows and 3 variables:
#' \describe{
#'   \item{addrrecnum}{address identification number}
#'   \item{address}{street address}
#'   \item{geometry}{sf geometric data}
#' }
#'
#' @examples
#' head(sample_geocoder)
#'
"sample_geocoder"
