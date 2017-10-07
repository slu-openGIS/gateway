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
#' @importFrom rlang :=
#'
#' @export
stdName <- function(data, variable, overwrite = TRUE, newVariable){
  # ensure no conflicts with user's data:
  if ( any(names(data) == "dc_correct") == TRUE ) stop('data cannot contain a variable named dc_correct')
  if ( any(names(data) == "dc_incorrect") == TRUE ) stop('data cannot contain a variable named dc_incorrect')

  # prevents R CMD check note for undefined gloabl variable:
  dc_correct <- NULL
  dc_incorrect <- NULL

  # load standardized data
  correct <- get("stdStreets")

  # convert street name variable to Title Case
  check <- dplyr::mutate(data, "dc_incorrect" := stringr::str_to_title(data$UQ(variable)))

  # join user's data with standardized data
  check <- dplyr::left_join(check, correct, by = "dc_incorrect")

  # create corrected variable with standardized street names
  if (overwrite == TRUE){

    check <- dplyr::mutate(check, !!variable := ifelse(!is.na(dc_correct) == TRUE, dc_correct, dc_incorrect))

  } else if (overwrite == FALSE) {

    check <- dplyr::mutate(check, !!newVariable := ifelse(!is.na(dc_correct) == TRUE, dc_correct, dc_incorrect))

  }

  # remove variables from standardized data
  check <- dplyr::select(check, -dc_correct, -dc_incorrect)

  # return data as a tibble
  tibble::as_tibble(check)
}
