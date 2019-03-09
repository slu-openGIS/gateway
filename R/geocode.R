#' Geocode
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#'
#' @export
gw_build_geocoder <- function(return = c("id", "coords", "parcel", "zip"), class, include_units = FALSE){

  # obtain master list if none is provided
  master <- gw_get_data(data = "Addresses", class = "sf")

  # clean address data
  master <- dplyr::select(master, ADDRRECNUM, HANDLE:ZIP)

  # optionally filter out units
  if (include_units == FALSE){
    master <- dplyr::filter(master, is.na(UNITNUM) == TRUE)
  }

  # optionally drop unneeded data
  if ("parcel" %in% return == FALSE){
    master <- dplyr::select(master, -HANDLE)
  }

  if ("zip" %in% return == FALSE){
    master <- dplyr::select(master, -ZIP)
  }

  # re-project
  master <- sf::st_transform(master, crs = 4268)

  # create coordinates if class is tibble
  if (class == "tibble" & "coords" %in% return == TRUE){
    master <- gw_coords_as_cols(master)
    sf::st_geometry(master) <- NULL
  } else if (class == "tibble" & "coords" %in% return == FALSE){
    sf::st_geometry(master) <- NULL
  }

  # clean-up data
  master %>%
    dplyr::mutate(UNITNUM = ifelse(HOUSESUF == "E", "E", UNITNUM)) %>%
    dplyr::mutate(HOUSESUF = ifelse(HOUSESUF == "E", NA, HOUSESUF)) %>%
    postmastr::pm_street_std(var = STREETNAME, locale = "us") %>%
    postmastr::pm_streetSuf_std(var = STREETTYPE, locale = "us") -> master

  # return output
  return(master)

}


gw_coords_as_cols <- function(x, names = c("x","y")) {
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# https://github.com/r-spatial/sf/issues/231
