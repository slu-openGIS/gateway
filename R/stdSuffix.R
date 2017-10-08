#' Standardized Street Suffixes
#'
#' \code{stdSuffix} standardizes a given set of...
#'
#' @usage stdSuffix(data, variable, overwrite = TRUE, newVariable)
#'
#' @param data A tibble or a data frame
#'
#' @param variable A character vector within \code{data} that contains City of St. Louis street suffixes
#'
#' @param overwrite A logical scalar. Should the output overwrite the given variable?
#'
#' @param newVariable A name for a new vector to be created if \code{overwrite = FALSE}
#'
#' @return \code{stdSuffix} returns a tibble with the requested output - either the ...
#'
#' @note \code{stdSuffix} requires two variable names be unused in the original data - \code{suf_com} and
#' \code{suf_pri}. If these names are present, \code{stdSuffix} will return an error.
#'
#' @importFrom rlang :=
#'
#' @export
stdSuffix <- function(data, variable, overwrite = TRUE, newVariable){
  # ensure no conflicts with user's data:
  if ( any(names(data) == "suf_com") == TRUE ) stop('data cannot contain a variable named suf_com')
  if ( any(names(data) == "suf_pri") == TRUE ) stop('data cannot contain a variable named suf_pri')

  # prevents R CMD check note for undefined gloabl variable:
  suf_com <- NULL
  suf_pri <- NULL

  # load standardized data
  correct <- get("stdSuffixTbl")

  # convert street suffix variable to Title Case
  check <- dplyr::mutate(data, "suf_com" := stringr::str_to_title(data$UQ(variable)))

  # join user's data with standardized data
  check <- dplyr::left_join(check, correct, by = "suf_com")

  # create corrected variable with standardized street names
  if (overwrite == TRUE){

    check <- dplyr::mutate(check, !!variable := suf_std)

  } else if (overwrite == FALSE) {

    check <- dplyr::mutate(check, !!newVariable := suf_std)

  }

  # remove variables from standardized data
  check <- dplyr::select(check, -suf_pri, -suf_com, -suf_std)

  # return data as a tibble
  tibble::as_tibble(check)
}
