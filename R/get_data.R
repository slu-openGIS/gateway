#' Download Data About St. Louis
#'
#' @description Download data from the City of St. Louis's open data page or the Saint Louis
#'     University openGIS project's repositories.
#'
#' @details Data provided by the City of St. Louis are provided without warrenty. Data from the
#'     Saint Louis openGIS project are provided under a
#'     \href{https://opendatacommons.org/licenses/by/index.html}{Open Data Commons Attribution License}.
#'
#' The following data are currently available:
#'
#' \describe{
#'   \item{\code{Addresses}}{City of St. Louis; street address master list (\code{sf} or tibble)}
#'   \item{\code{Grids}}{SLU OpenGIS; square kilometer grids for spatial statistics (\code{sf})}
#'   \item{\code{Grids, Clipped}}{SLU OpenGIS; square kilometer grids for spatial statistics with major parks clipped out (\code{sf})}
#'   \item{\code{Grids, Exploded}}{SLU OpenGIS; square kilometer grids for spatial statistics with major parks clipped out and slivers exploded (\code{sf})}
#'   \item{\code{Land Records}}{City of St. Louis; City property records (tibble)}
#'   \item{\code{Land Use}}{City of St. Louis; land use data (\code{sf} or tibble)}
#'   \item{\code{Mississippi River, Illinois}}{SLU OpenGIS; extent of the Mississippi River in Madison and St. Clair counties for the length of the City of St. Louis (\code{sf})}
#'   \item{\code{Mississippi River, Islands}}{SLU OpenGIS; group of islands on the Illinois side of the Mississippi (\code{sf})}
#'   \item{\code{Neighborhoods}}{City of St. Louis; City neighborhoods (\code{sf} or tibble)}
#'   \item{\code{Ortho Tile Boundaries}}{SLU OpenGIS; Eest-West Gateway Orthoimagery Tile Boundaries (\code{sf} or tibble)}
#'   \item{\code{Parcels}}{City of St. Louis; parcel data (\code{sf} or tibble)}
#'   \item{\code{Parks}}{City of St. Louis; public parks (\code{sf} or tibble)}
#'   \item{\code{Placenames}}{SLU OpenGIS; place name data set (\code{sf} or tibble)}
#'   \item{\code{Police Districts}}{City of St. Louis; police districts (\code{sf} or tibble)}
#'   \item{\code{Police Districts, Pre-2014}}{City of St. Louis; police districts prior to 2014
#'       reorganization (\code{sf} or tibble)}
#'   \item{\code{Tracts}}{\pkg{tigris}; Census Tracts for the City of St. Louis (\code{sf} or tibble)}
#'   \item{\code{Voting Precincts}}{City of St. Louis; current voting precincts (\code{sf} or tibble)}
#'   \item{\code{Zoning}}{City of St. Louis; zoning data (\code{sf} or tibble)}
#'   \item{\code{Zoning, Multi}}{City of St. Louis; zoning data (\code{sf} or tibble)}
#' }
#'
#' @param data A character string describing the data you wish to download.
#' @param class Either \code{"sf"} to return a simple features object or code
#'    \code{"tibble"} to return a tibble.
#' @param ... Optional arguments to pass on to \pkg{tidycensus} or \pkg{tigris}
#'
#' @return A \code{sf} object or tibble with the requested data.
#'
#' @importFrom dplyr as_tibble
#' @importFrom foreign read.dbf
#' @importFrom sf st_read
#' @importFrom utils download.file
#' @importFrom utils unzip
#'
#' @export
gw_get_data <- function(data, class, ...){

  # vector of items for city data sources
  cityData <- c("Addresses", "Neighborhoods", "Land Records", "Land Use", "Parcels", "Parks",
                "Police Districts", "Police Districts, Pre-2014", "Voting Precincts", "Zoning",
                "Zoning, Multi")

  # vector of items for slu openGIS data sources
  sluData <- c("Grids", "Grids, Clipped", "Grids, Exploded", "Mississippi River, Illinois",
               "Mississippi River, Islands", "Ortho Tile Boundaries", "Placenames")

  # vector of items for tigris data
  tigrisData <- c("Tracts")

  # combined vector
  dataList <- c(cityData, sluData, tigrisData)

  # is the request valid?
  if (data %in% dataList == FALSE) {
    stop("The given data source does not match a currently accessible data set. Use '?gw_get_data' to view a list of possible values for 'data'.")
  }

  # is the request for city or openGIS data?
  if (data %in% cityData == TRUE){
    source <- "city"
    call <- gw_get_city(data = data)
  } else if (data %in% sluData == TRUE){
    source <- "slu"
    call <- gw_get_slu(data = data)
  } else if (data %in% tigrisData == TRUE){
    source <- "tigris"
  }

  # slu and city data
  if (source == "city" | source == "slu"){

    # create temp directory, download, and extract
    tmpdir <- tempdir()
    utils::download.file(call$url, paste0(tmpdir,"archive.zip"))
    utils::unzip(paste0(tmpdir,"archive.zip"), exdir = tmpdir)
    filepath <- paste0(tmpdir,call$path)

    # read data into R
    if (data == "Land Records"){
      output <- foreign::read.dbf(filepath, as.is = TRUE)
      output <- dplyr::as_tibble(output)
    } else if (data == "Placenames") {
      output <- suppressMessages(readr::read_csv(file = filepath))
      output <- sf::st_as_sf(output, coords = c("longitude", "latitude"), crs = 4269)
    } else {
      output <- sf::st_read(dsn = filepath, stringsAsFactors = FALSE, quiet = TRUE)
    }

  } else if (source == "tigris"){
    output <- gw_get_tigris(data = data, ...)
  }

  # convert sf objects to tibble
  if (class == "tibble" & "sf" %in% class(output) == TRUE){
    sf::st_geometry(output) <- NULL
    output <- dplyr::as_tibble(output)
  }

  # remove temporary directory
  if (source == "city" | source == "slu"){
    unlink(tmpdir)
  }

  # return output
  return(output)

}

# Create Calls for City Data
gw_get_city <- function(data){

  # create vectors
  if (data == "Addresses") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/pargeocd.zip"
    path <- "/pargeocd.shp"
  }
  else if (data == "Neighborhoods") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/nbrhds_wards.zip"
    path <- "/Neighborhood_Boundaries.shp"
  }
  else if (data == "Land Records") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/par.zip"
    path <- "/par.dbf"
  }
  else if (data == "Land Use") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/slup.zip"
    path <- "/slup.shp"
  }
  else if (data == "Parcels") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/prcl_shape.zip"
    path <- "/prcl.shp"
  }
  else if (data == "Parks") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/parks.zip"
    path <- "/parks.shp"
  }
  else if (data == "Police Districts") {
    url <- "https://www.stlouis-mo.gov/data/boundaries/upload/STL-Police-Districts-2014-2.zip"
    path <- "/STL POLICE DISTRICTS/GIS.STL.POLICE_DISTRICTS_2014.shp"
  }
  else if (data == "Police Districts, Pre-2014") {
    url <- "https://www.stlouis-mo.gov/data/boundaries/upload/STL-Police-Districts-pre-2014.zip"
    path <- "/STL Police Districts - pre-2014/STLPOLICEDISTRICTSPRE2014.shp"
  }
  else if (data == "Voting Precincts") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/nbrhds_wards.zip"
    path <- "/WARDS_2010.shp"
  }
  else if (data == "Zoning") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/zoning.zip"
    path <- "/prclz.shp"
  }
  else if (data == "Zoning, Multi") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/zoning.zip"
    path <- "/prclzm.shp"
  }

  # create list
  call <- list(
    url = c(url),
    path = c(path)
  )

  # return output
  return(call)

}

# Create Calls for SLU OpenGIS Data
gw_get_slu <- function(data){

  if (data == "Mississippi River, Islands") {
    url <- "https://github.com/slu-openGIS/IL_HYDRO_Islands/archive/master.zip"
    path <- "/IL_HYDRO_Islands-master/Shapefile/IL_HYDRO_Islands.shp"
  }
  else if (data == "Mississippi River, Illinois") {
    url <- "https://github.com/slu-openGIS/IL_HYDRO_Mississippi/archive/master.zip"
    path <- "/IL_HYDRO_Mississippi-master/Shapefile/IL_HYDRO_Mississippi.shp"
  }
  else if (data == "Grids" | data == "Grids, Clipped" | data == "Grids, Exploded") {

    url <- "https://github.com/slu-openGIS/STL_BOUNDARY_Grids/archive/master.zip"

    if (data == "Grids"){
      path <- "/STL_BOUNDARY_Grids-master/STL_BOUNDARY_Grids/shapefile/STL_BOUNDARY_Grids.shp"
    } else if (data == "Grids, Clipped"){
      path <- "/STL_BOUNDARY_Grids-master/STL_BOUNDARY_GridsClipped/shapefile/STL_BOUNDARY_GridsClipped.shp"
    } else if (data == "Grids, Exploded"){
      path <- "/STL_BOUNDARY_Grids-master/STL_BOUNDARY_GridsExploded/shapefile/STL_BOUNDARY_GridsExploded.shp"
    }
  }
  else if (data == "Ortho Tile Boundaries") {
    url <- "https://github.com/slu-openGIS/MO_STL_STLTiles/archive/master.zip"
    path <- "/MO_STL_STLTiles-master/Shapefile/STLTiles.shp"
  } else if (data == "Placenames"){
    url <- "https://github.com/slu-openGIS/STL_GEOCODER_Placename/archive/master.zip"
    path <- "/STL_GEOCODER_Placename-master/data/STL_GEOCODER_Placename.csv"
  }

  # create list
  call <- list(
    url = c(url),
    path = c(path)
  )

  # return output
  return(call)

}

# Download Tigris Data
gw_get_tigris <- function(data, ...){

  if (data == "Tracts"){
    out <- tigris::tracts(state = 29, county = 510, class = "sf", ...)
  }

  # return output
  return(out)

}
