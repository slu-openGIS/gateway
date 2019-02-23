#' Download openGIS Data Repositories
#'
#' \code{gw_get_repo} provides access to shapefiles and \code{.geoJSON} files stored on the
#' St. Louis openGIS GitHub organization.
#'
#' @usage gw_get_repo(repo, file)
#'
#' @param repo A character string matching the name of a repository to download and extract
#'     data from
#' @param file An optional character string matching the name of the file within a multi-file
#'     repo (currently only needed for \code{STL_BOUNDARY_Grids})
#'
#' @return \code{gw_get_repo} returns a simple features object with the requested data.
#'
#' @importFrom sf st_read
#' @importFrom utils download.file
#' @importFrom utils unzip
#'
#' @export
gw_get_repo <- function(repo, file) {

  repoList <- c("IL_HYDRO_Islands", "IL_HYDRO_Mississippi", "MO_DEMOS_CountiesRace",
                "MO_DEMOS_JeffCityRegion", "MO_STL_STLTiles", "STL_BOUNDARY_City", "STL_BOUNDARY_Tracts",
                "STL_HOUSING_MedianAge", "STL_BOUNDARY_Grids")

  if (repo %nin% repoList) {
    stop("The given repository is not accessible at this time.")
  }

  if (repo == "IL_HYDRO_Islands") {
    url <- "https://github.com/slu-openGIS/IL_HYDRO_Islands/archive/master.zip"
    path <- "/IL_HYDRO_Islands-master/Shapefile/IL_HYDRO_Islands.shp"
  }
  else if (repo == "IL_HYDRO_Mississippi") {
    url <- "https://github.com/slu-openGIS/IL_HYDRO_Mississippi/archive/master.zip"
    path <- "/IL_HYDRO_Mississippi-master/Shapefile/IL_HYDRO_Mississippi.shp"
  }
  else if (repo == "MO_DEMOS_CountiesRace") {
    url <- "https://github.com/slu-openGIS/MO_DEMOS_CountiesRace/archive/master.zip"
    path <- "/MO_DEMOS_CountiesRace-master/Shapefile/MO_DEMOS_CountiesRace.shp"
  }
  else if (repo == "MO_DEMOS_JeffCityRegion") {
    url <- "https://github.com/slu-openGIS/MO_DEMOS_JeffCityRegion/archive/master.zip"
    path <- "/MO_DEMOS_JeffCityRegion-master/Shapefile/MO_DEMOS_JeffCityRegion.shp"
  }
  else if (repo == "MO_STL_STLTiles") {
    url <- "https://github.com/slu-openGIS/MO_STL_STLTiles/archive/master.zip"
    path <- "/MO_STL_STLTiles-master/Shapefile/STLTiles.shp"
  }
  else if (repo == "STL_BOUNDARY_City") {
    url <- "https://github.com/slu-openGIS/STL_BOUNDARY_City/archive/master.zip"
    path <- "/STL_BOUNDARY_City-master/Shapefile/STL_BOUNDARY_City.shp"
  }
  else if (repo == "STL_HOUSING_MedianAge") {
    url <- "https://github.com/slu-openGIS/STL_HOUSING_MedianAge/archive/master.zip"
    path <- "/STL_HOUSING_MedianAge-master/Shapefile/STL_HOUSING_MedianAge.shp"
  }
  else if (repo == "STL_BOUNDARY_Tracts") {
    url <- "https://github.com/slu-openGIS/STL_BOUNDARY_Tracts/archive/master.zip"
    path <- "/STL_BOUNDARY_Tracts-master/Shapefile/STL_BOUNDARY_Tracts.shp"
  }
  else if (repo == "STL_BOUNDARY_Grids") {

    url <- "https://github.com/slu-openGIS/STL_BOUNDARY_Grids/archive/master.zip"

    if (file == "Grids"){
      path <- "/STL_BOUNDARY_Grids-master/STL_BOUNDARY_Grids/shapefile/STL_BOUNDARY_Grids.shp"
    } else if (file == "GridsClipped"){
      path <- "/STL_BOUNDARY_Grids-master/STL_BOUNDARY_GridsClipped/shapefile/STL_BOUNDARY_GridsClipped.shp"
    } else if (file == "GridsExploded"){
      path <- "/STL_BOUNDARY_Grids-master/STL_BOUNDARY_GridsExploded/shapefile/STL_BOUNDARY_GridsExploded.shp"
    }
  }

  tmpdir <- tempdir()
  utils::download.file(url, paste0(tmpdir,"master.zip"))
  utils::unzip(paste0(tmpdir,"master.zip"), exdir = tmpdir)
  shapefile <- paste0(tmpdir,path)

  simpleFeature <- sf::st_read(shapefile, stringsAsFactors = FALSE)
  unlink(tmpdir)

  return(simpleFeature)

}

