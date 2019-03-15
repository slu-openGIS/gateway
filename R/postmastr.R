#' Standardized Street Names
#'
#' @description A data set containing standardized street names and possible alternatives.
#'     This can be used as the basis for a dictionary appendix for the \pkg{postmastr}
#'     package.
#'
#' @docType data
#'
#' @format A data frame with 12 rows and 2 variables:
#' \describe{
#'   \item{st.output}{standardized street name}
#'   \item{st.input}{alternative street name}
#'   }
#'
#' @examples
#' head(stl_std_streets)
#'
"stl_std_streets"

#' Standardized Street Suffix Names
#'
#' @description A data set containing the street suffixes used in St. Louis and possible
#'     alternatives. This can be used as the basis for a dictionary appendix for
#'     the \pkg{postmastr} package.
#'
#' @docType data
#'
#' @format A data frame with 76 rows and 3 variables:
#' \describe{
#'   \item{suf.type}{suffix type}
#'   \item{suf.input}{input suffix}
#'   \item{suf.output}{output suffix}
#'   }
#'
#' @examples
#' head(stl_std_suffix)
#'
"stl_std_suffix"