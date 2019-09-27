# open street map
gw_geocode_osm <- function(.data){

  # set global bindings
  addrrecnum = ...address = ...address2 = lat = lon = query = NULL

  # identify observations
  id <- gw_geocode_identify(.data)

  # subset distinct observations
  target <- gw_geocode_prep(id)

  # make copy of address
  target <- dplyr::mutate(target, ...address2 = stringr::str_c(...address, ", St. Louis"))

  # geocode
  result <- tryCatch(
    {suppressWarnings(tmaptools::geocode_OSM(target$...address2, as.data.frame = TRUE))},
    error=function(cond) {
      return(NULL)
    })

  # clean-up results
  if (is.null(result) == FALSE){

    # subset results
    result <- dplyr::select(result, query, lon, lat)

    # rename variables
    result <- dplyr::rename(result,
                            ...address2 = query,
                            x = lon,
                            y = lat
    )

    # add source
    result <- dplyr::mutate(result, source = "open street map")

    # combine result and target data
    target <- dplyr::left_join(target, result, by = "...address2")

    # remove ...address2
    target <- dplyr::select(target, -...address2)

    # rebuild data
    out <- gw_geocode_replace(source = id, target = target)

  } else if (is.null(result) == TRUE){

    out <- .data

  }

  # return result
  return(out)

}
