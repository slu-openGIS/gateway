#' Sushi Resturants in the Ciy of St. Louis
#'
#' @description A selection of sushi resturants in the St. Louis, Missouri region.
#'    These data are in long form, with multiple visits in a single year to some
#'    of the resturants. They also contain a degree of messiness, with a mix of
#'    upper and lower case tying, some street types abbreviated (i.e. "St") and
#'    others entered in full (i.e. "Street"), and an example of two ways in which
#'    unit numbers may appear.
#'
#' @docType data
#'
#' @usage data(sushi)
#'
#' @format A tibble with 19 rows and 3 variables:
#' \describe{
#'   \item{name}{resturant name}
#'   \item{address}{street address}
#'   \item{visit}{visit date}
#' }
#'
#' @examples
#' str(sushi)
#' head(sushi)
#'
"sushi"
