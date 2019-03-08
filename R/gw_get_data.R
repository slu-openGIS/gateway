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
#'   \item{\code{Neighborhoods}}{City of St. Louis; City neighborhoods (\code{sf} or tibble)}
#'   \item{\code{Land Records}}{City of St. Louis; City property records (tibble)}
#'   \item{\code{Land Use}}{City of St. Louis; land use data (\code{sf} or tibble)}
#'   \item{\code{Parcels}}{City of St. Louis; parcel data (\code{sf} or tibble)}
#'   \item{\code{Parks}}{City of St. Louis; public parks (\code{sf} or tibble)}
#'   \item{\code{Police Districts}}{City of St. Louis; police districts (\code{sf} or tibble)}
#'   \item{\code{Police Districts, Pre-2014}}{City of St. Louis; police districts prior to 2014
#'       reorganization (\code{sf} or tibble)}
#'   \item{\code{Voting Precincts}}{City of St. Louis; current voting precincts (\code{sf} or tibble)}
#'   \item{\code{Zoning}}{City of St. Louis; zoning data (\code{sf} or tibble)}
#'   \item{\code{Zoning, Multi}}{City of St. Louis; zoning data (\code{sf} or tibble)}
#' }
#'
#' @param data A character string describing the data you wish to download.
#' @param class Either \code{"sf"} to return a simple features object or code
#'    \code{"tibble"} to return a tibble.
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
gw_get_data <- function(data, class){

  dataList <- c("Addresses", "Neighborhoods", "Land Records", "Land Use", "Parcels", "Parks",
                "Police Districts", "Police Districts, Pre-2014", "Voting Precincts", "Zoning",
                "Zoning, Multi")

  if (data %nin% dataList) {
    stop("The given repository is not accessible at this time.")
  }

  if (data == "Addresses") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/pargeocd.zip"
    path <- "/pargeocd.shp"
  }
  else if (data == "Neighborhoods") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/nbrhds_wards.zip"
    path <- "/nbrhds_wards/BND_Nhd88_cw.shp"
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
    path <- "/nbrhds_wards/POL_WRD_2010_Prec.shp"
  }
  else if (data == "Zoning") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/zoning.zip"
    path <- "/prclz.shp"
  }
  else if (data == "Zoning, Multi") {
    url <- "https://www.stlouis-mo.gov/data/upload/data-files/zoning.zip"
    path <- "/prclzm.shp"
  }

  # create temp directory, download, and extract
  tmpdir <- tempdir()
  utils::download.file(url, paste0(tmpdir,"archive.zip"))
  utils::unzip(paste0(tmpdir,"archive.zip"), exdir = tmpdir)
  filepath <- paste0(tmpdir,path)

  # read data into R
  if (data == "Land Records"){
    output <- foreign::read.dbf(filepath, as.is = TRUE)
    output <- dplyr::as_tibble(output)
  } else {
    output <- sf::st_read(dsn = filepath, stringsAsFactors = FALSE, quiet = TRUE)
  }

  # convert sf objects to tibble
  if (class == "tibble" & "sf" %in% class(output) == TRUE){
    sf::st_geometry(output) <- NULL
    output <- dplyr::as_tibble(output)
  }

  # remove temporary directory
  unlink(tmpdir)

  # return output
  return(output)

}
