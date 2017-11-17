#' Standardized Street Suffixes
#'
#' \code{stdSuffix} standardizes a given set of street suffix values to the USPS preferred suffix abbreviation.
#'
#' @usage stdSuffix(.data, variable, overwrite = TRUE, newVariable)
#'
#' @param .data A tibble or a data frame
#' @param variable A character vector within \code{.data} that contains City of St. Louis street suffixes
#' @param overwrite A logical scalar. Should the output overwrite the given variable?
#' @param newVariable A name for a new vector to be created if \code{overwrite = FALSE}
#'
#' @return \code{stdSuffix} returns a tibble with the requested output - either the suffix variable has
#'     been overwritten or a new variable with corrected suffix abbreviations has been added.
#'
#' @note \code{stdSuffix} requires two variable names be unused in the original data - \code{suf_com} and
#' \code{suf_pri}. If these names are present, \code{stdSuffix} will return an error.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom dplyr left_join
#' @importFrom rlang :=
#'
#' @export
stdSuffix <- function(.data, variable, overwrite = TRUE, newVariable){

  # ensure no conflicts with user's data:
  if ( any(names(.data) == "suf_com") == TRUE ) stop('data cannot contain a variable named suf_com')
  if ( any(names(.data) == "suf_cor") == TRUE ) stop('data cannot contain a variable named suf_cor')
  if ( any(names(.data) == "suf_pri") == TRUE ) stop('data cannot contain a variable named suf_pri')
  if ( any(names(.data) == "suf_std") == TRUE ) stop('data cannot contain a variable named suf_std')

  # prevents R CMD check note for undefined gloabl variable:
  id <- NULL
  suf_com <- NULL
  suf_cor <- NULL
  suf_pri <- NULL
  suf_std <- NULL

  # load standardized data
  correct <- get("stdSuffixTbl")

  # convert street suffix variable to Title Case
  .data <- dplyr::mutate(.data, "suf_com" := stringr::str_to_title(.data$UQ(variable)))

  # fix common issues
  .data %>%
    dplyr::mutate(suf_cor = ifelse(suf_com %in% correct$suf_std, suf_com, NA)) %>%
    dplyr::mutate(suf_cor = ifelse(suf_com == "Street", "St", suf_cor)) %>%
    dplyr::mutate(suf_cor = ifelse(suf_com == "Avenue", "Ave", suf_cor)) %>%
    dplyr::mutate(suf_cor = ifelse(suf_com == "Drive", "Dr", suf_cor)) -> .data

  # subset data
  matched <- dplyr::filter(.data, is.na(suf_cor) == FALSE)
  .data %>%
    dplyr::filter(is.na(suf_cor) == TRUE) %>%
    dplyr::select(-suf_cor) -> unmatched

  # join unmatched data with standardized data
  unmatched <- left_join(unmatched, correct, by = "suf_com")

  unmatched %>%
    dplyr::select(-suf_pri) %>%
    dplyr::rename(suf_cor = suf_std) -> unmatched

  # combine data
  output <- dplyr::bind_rows(matched, unmatched)
  output <- dplyr::arrange(output, id)

  # overwrite data
  if (overwrite == TRUE){

    output <- dplyr::mutate(output, !!variable := suf_cor)

  } else if (overwrite == FALSE) {

    output <- dplyr::mutate(output, !!newVariable := suf_cor)

  }

  # remove suffix variables
  output <- dplyr::select(output, -c(suf_com, suf_cor))

  # return tibble
  output <- dplyr::as_tibble(output)
  return(output)
}
