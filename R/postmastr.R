#' Standardized Directions
#'
#' @description A data set containing standardized street directionals for St. Louis.
#'     This can be used as the basis for a dictionary appendix for the \pkg{postmastr}
#'     package. It contains only the extra directionals used in the City's master list
#'     of street addresses.
#'
#' @docType data
#'
#' @format A data frame with 7 rows and 2 variables:
#' \describe{
#'   \item{dir.output}{directional abbreviation}
#'   \item{dir.input}{directional abbreviation}
#'   }
#'
#' @examples
#' head(stl_std_directions)
#'
"stl_std_directions"

#' Standardized House Suffix Values
#'
#' @description A data set containing standardized house suffix names and possible
#'     alternatives. This can be used as the basis for a dictionary appendix for
#'     the \pkg{postmastr} package.
#'
#' @docType data
#'
#' @format A data frame with 5 rows and 2 variables:
#' \describe{
#'   \item{houseSuf.output}{standardized abbreviation for house suffix}
#'   \item{houseSuf.input}{alternative house suffix}
#'   }
#'
#' @examples
#' head(stl_std_houseSuffix)
#'
"stl_std_houseSuffix"

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
