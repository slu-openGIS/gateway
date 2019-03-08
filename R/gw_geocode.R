#' Geocode
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#'
#' @export
gw_geocode <- function(.data, address, return = c("id", "parcel", "coords", "zip"), class){

  # obtain master list if none is provided
  master <- gw_get_data(data = "Addresses", class = "sf")

  # clean address data
  master <- dplyr::select(master, ADDRRECNUM, HANDLE:ZIP)

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
    st_geometry(master) <- NULL
  } else if (class == "tibble" & "coords" %in% return == FALSE){
    st_geometry(master) <- NULL
  }

  # clean-up data
  master %>%
    dplyr::mutate(UNITNUM = ifelse(HOUSESUF == "E", "E", UNITNUM)) %>%
    dplyr::mutate(PREDIR = ifelse(PREDIR == "EW" & STREETNAME == "FLORISSANT", "W", PREDIR)) %>%
    postmastr::pm_streetSuf_std(var = STREETTYPE) %>%
    postmastr::pm_streetDir_std(var = SUFDIR) -> master

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
