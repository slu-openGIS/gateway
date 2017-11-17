#' Standardize Street Names
#'
#' \code{gw_stName} standardizes a given set of City of St. Louis street names by checking
#' them against a master list of street names and returning a preferred name. For example,
#' "1ST" and "FIRST" are both possible variations of "FIRST ST". If "FIRST" is given,
#' \code{gw_stdName} will offer "1ST" as the standardized alternative.
#'
#' @usage gw_stName(.data, street, overwrite = TRUE, newStreet)
#'
#' @param .data A tbl
#'
#' @param street A character vector within \code{data} that contains City of St. Louis street names
#'
#' @param overwrite A logical scalar. Should the output overwrite the given variable?
#'
#' @param newStreet A name for a new vector to be created if \code{overwrite = FALSE}
#'
#' @return \code{gw_stName} returns a tibble with the requested output - either the original data with
#' non-standard street names overwritten in the given vector or the original data with a new
#' vector containing standardized street names.
#'
#' @note \code{gw_stName} requires two variable names be unused in the original data - \code{dc_corrrect} and
#' \code{dc_incorrect}. If these names are present, \code{gw_stName} will return an error.
#'
#' @importFrom rlang :=
#'
#' @export
gw_stName <- function(.data, street, overwrite = TRUE, newStreet){

  # ensure no conflicts with user's data:
  if ( any(names(.data) == "dc_correct") == TRUE ) {
    stop('data cannot contain a variable named dc_correct')
  }
  if ( any(names(.data) == "dc_incorrect") == TRUE ) {
    stop('data cannot contain a variable named dc_incorrect')
  }

  # check newSuffix argument
  if (missing(newStreet)) {
    newStreet <- "nullStreet"
  }

  # save parameters to list
  paramList <- as.list(match.call())

  if (!is.character(paramList$street)) {
    var <- rlang::enquo(street)
  } else if (is.character(paramList$street)) {
    var <- rlang::quo(!! rlang::sym(street))
  }

  varQ <- rlang::quo_name(rlang::enquo(var))

  if (!is.character(paramList$newStreet)) {
    newVar <- rlang::enquo(newStreet)
  } else if (is.character(paramList$newStreet)) {
    newVar <- rlang::quo(!! rlang::sym(newStreet))
  }

  newVarQ <- rlang::quo_name(rlang::enquo(newVar))


  # prevents R CMD check note for undefined gloabl variable:
  dc_correct <- NULL
  dc_incorrect <- NULL

  # load standardized data
  correct <- get("stdStreets")

  # convert street name variable to Title Case
  check <- dplyr::mutate(.data, "dc_incorrect" := stringr::str_to_title(.data[[varQ]]))

  # join user's data with standardized data
  check <- dplyr::left_join(check, correct, by = "dc_incorrect")

  # create corrected variable with standardized street names
  if (overwrite == TRUE){

    check <- dplyr::mutate(check, !!varQ := ifelse(!is.na(dc_correct) == TRUE, dc_correct, dc_incorrect))

  } else if (overwrite == FALSE) {

    check <- dplyr::mutate(check, !!newVarQ := ifelse(!is.na(dc_correct) == TRUE, dc_correct, dc_incorrect))

  }

  # remove variables from standardized data
  check <- dplyr::select(check, -dc_correct, -dc_incorrect)

  # return data as a tibble
  tibble::as_tibble(check)
}
