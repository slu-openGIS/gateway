#' Sushi Resturants in the Ciy of St. Louis, Tibble Format
#'
#' @description A selection of sushi resturants in the St. Louis, Missouri region.
#'    These data are in long form, with multiple visits in a single year to some
#'    of the resturants. These data are the parsed and standardized form of the
#'    \code{sushi2} data set in the \pkg{postmastr} package. They are therefore
#'    useful for demonstrating the geocoding features of the \pkg{gateway} package.
#'
#' @docType data
#'
#' @usage data(sushi)
#'
#' @format A tibble with 15 rows and 3 variables:
#' \describe{
#'   \item{name}{resturant name}
#'   \item{address}{street address}
#'   \item{visit}{visit date}
#' }
#'
#' @examples
#' head(sushi)
#'
"sushi"

#' Sushi Resturants in the Ciy of St. Louis, sf Format
#'
#' @description A selection of sushi resturants in the St. Louis, Missouri region.
#'    These data are in long form, with multiple visits in a single year to some
#'    of the resturants. These data are the parsed and standardized form of the
#'    \code{sushi2} data set in the \pkg{postmastr} package. They have been
#'    geocoded and projected, and so can be used to demonstate the aggregate
#'    and identify features of the \pkg{gateway} package.
#'
#' @docType data
#'
#' @usage data(sushi_sf)
#'
#' @format A \code{sf} object with 15 rows and 4 variables:
#' \describe{
#'   \item{name}{resturant name}
#'   \item{address}{street address}
#'   \item{visit}{visit date}
#'   \item{geometry}{sf geometric data}
#' }
#'
#' @examples
#' head(sushi_sf)
#'
"sushi_sf"

