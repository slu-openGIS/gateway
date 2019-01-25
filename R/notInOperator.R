#' Not In Operator
#'
#' @description Provides the compliment to the base R \code{\%in\%} operator.
#'
#' @param x vector or \code{NULL}: the values to be matched
#' @param y vector or \code{NULL}: the values to be matched against
#'
#' @source \href{https://github.com/harrelfe/Hmisc/blob/master/R/in.operator.s}{\code{Hmsic}}
#'
#' @examples
#' x <- 2
#' y <- 2
#' z <- 3
#'
#' x %in% y
#' x %nin% y
#'
#' x %in% z
#' x %nin% z
#'
#' @export
"%nin%" <- function(x, y) match(x, y, nomatch = 0) == 0
