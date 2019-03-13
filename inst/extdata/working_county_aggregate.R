# gw_aggregate county
areas_stl <- c("block group", "tract", "precinct", "ward", "neighborhood", "municipality", "county")

if (region == "city" & to %nin% areas_stl){

  stop(glue::glue("{to} is not a valid input for City of St. Louis data."))

}

areas_slc <- c("block group", "tract", "precinct", "municipality", "county")

if (region == "county" & to %nin% areas_slc){

  stop(glue::glue("{to} is not a valid input for St. Louis County data."))

}

areas_both <- c("block group", "tract", "precinct", "municipality", "county")

if (region == "both" & to %nin% areas_both){

  stop(glue::glue("{to} is not a valid input for combined City of St. Louis and St. Louis County data."))

}

# aggregate data
if (region == "city" | region == "county"){

  out <- gw_aggregate_points(.data, to, region = region, sf = sf, replace.na = replace.na,
                             keep.na = keep.na)

} else if (region == "both"){

  # aggregate city data
  city <- gw_aggregate_points(.data, to, region = "city", sf = sf, replace.na = replace.na,
                              keep.na = keep.na)

  # aggregate county data
  county <- gw_aggregate_points(.data, to, region = "county", sf = sf, replace.na = replace.na,
                                keep.na = keep.na)

  # combine city and county objects
  out <- rbind(city, county)

}


gw_load_areal <- function(name){

  if (name == "block group" & region == "city"){

    areal <- sf::st_transform(gateway::stl_blockgrps10, crs = 6512)
    areal <- dplyr::rename(areal, ID = GEOID)

  } else if (name == "tract" & region == "city"){

    areal <- sf::st_transform(gateway::stl_tracts10, crs = 6512)
    areal <- dplyr::rename(areal, ID = GEOID)

  } else if ((name == "municipality" | name == "county") & region == "city"){

    areal <- sf::st_transform(gateway::stl_city, crs = 6512)
    areal <- dplyr::rename(areal, ID = GEOID)

  } else if (name == "neighborhood" & region == "city") {

    areal <- sf::st_transform(gateway::stl_nhoods, crs = 6512)

  } else if (name == "precinct" & region == "city") {

    areal <- sf::st_transform(gateway::stl_precincts10, crs = 6512)
    areal <- dplyr::rename(areal, ID = HANDLE)

  } else if (name == "ward" & region == "city") {

    areal <- sf::st_transform(gateway::stl_wards10, crs = 6512)

  } else if (name == "block group" & region == "county"){

    areal <- sf::st_transform(gateway::slc_blockgrps10, crs = 6512)
    areal <- dplyr::rename(areal, ID = GEOID)

  } else if (name == "tract" & region == "county"){

    areal <- sf::st_transform(gateway::slc_tracts10, crs = 6512)
    areal <- dplyr::rename(areal, ID = GEOID)

  } else if (name == "county" & region == "county"){

    areal <- sf::st_transform(gateway::slc_county, crs = 6512)
    areal <- dplyr::rename(areal, ID = GEOID)

  }

  return(areal)

}
