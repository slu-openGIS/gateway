#' Standardize Street Names
#'
#' \code{stdName} standardizes a given set of City of St. Louis street names by checking
#' them against a master list of street names and returning a preferred name. For example,
#' "1ST" and "FIRST" are both possible variations of "FIRST ST". If "FIRST" is given,
#' \code{stdName} will offer "1ST" as the standardized alternative.
#'
#' @usage stdName(data, variable, overwrite = TRUE, newVariable)
#'
#' @param data A tibble or a data frame
#'
#' @param variable A character vector within \code{data} that contains City of St. Louis street names
#'
#' @param overwrite A logical scalar. Should the output overwrite the given variable?
#'
#' @param newVariable A name for a new vector to be created if \code{overwrite = FALSE}
#'
#' @return \code{stdName} returns a tibble with the requested output - either the original data with
#' non-standard street names overwritten in the given vector or the original data with a new
#' vector containing standardized street names.
#'
#' @note \code{stdName} requires two variable names be unused in the original data - \code{dc_corrrect} and
#' \code{dc_incorrect}. If these names are present, \code{stdName} will return an error.
#'
#' @export
stdName <- function(data, variable, overwrite = TRUE, newVariable){
  if ( any(names(data) == "dc_correct") == TRUE ) stop('data cannot contain a variable named dc_correct')
  if ( any(names(data) == "dc_incorrect") == TRUE ) stop('data cannot contain a variable named dc_incorrect')

  correct <- data(stdSteets)

  check <- dplyr::rename(data, "dc_incorrect" := !!variable)

  check <- dplyr::left_join(check, correct, by = "dc_incorrect")

  if (overwrite == TRUE){
    check <- dplyr::mutate(check, "dc_incorrect" =
                             ifelse(!is.na(dc_correct) == TRUE,
                                    dc_correct,
                                    dc_incorrect))

  } else if (overwrite == FALSE) {
    check <- dplyr::mutate(check, !!newVariable :=
                             ifelse(!is.na(dc_correct) == TRUE,
                                    dc_correct,
                                    dc_incorrect))
  }

  check %>%
    dplyr::select(-dc_correct) %>%
    dplyr::rename(!!variable := "dc_incorrect") -> check

  tibble::as_tibble(check)
}
