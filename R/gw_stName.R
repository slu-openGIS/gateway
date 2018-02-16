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
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
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
  if ( any(names(.data) == "st_id") == TRUE ) {
    stop('data cannot contain a variable named st_id')
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
  st_id <- NULL

  # load standardized data
  correct <- get("stdStreets")

  # create identification variable
  input <- .data
  input <- dplyr::mutate(input, st_id = as.numeric(rownames(input)))

  # convert street name variable to Title Case
  input <- dplyr::mutate(input, "dc_incorrect" := stringr::str_to_title(input[[varQ]]))

  # identify streets needing correction
  matched <- dplyr::filter(input, dc_incorrect %nin% correct$dc_incorrect)
  input %>%
    dplyr::filter(dc_incorrect %in% correct$dc_incorrect) -> unmatched

  # join unmatched data with standardized data
  unmatched <- left_join(unmatched, correct, by = "dc_incorrect")

  # combine data
  output <- dplyr::bind_rows(matched, unmatched)
  output <- dplyr::arrange(output, st_id)

  # create corrected variable with standardized street names
  if (overwrite == TRUE){

    output <- dplyr::mutate(output, !!varQ := ifelse(!is.na(dc_correct) == TRUE, dc_correct, dc_incorrect))

  } else if (overwrite == FALSE) {

    output <- dplyr::mutate(output, !!newVarQ := ifelse(!is.na(dc_correct) == TRUE, dc_correct, dc_incorrect))

  }

  # remove variables from standardized data
  output <- dplyr::select(output, -c(dc_correct, dc_incorrect, st_id))

  # return tibble
  output <- dplyr::as_tibble(output)
  return(output)
}
