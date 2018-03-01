#' Get Data from the City of St. Louis
#'
#' @description The \code{gw_get_city}...
#'
#' @return \code{gw_get_city} returns a table or simple features object with the requested data.
#'
#' @param repo A character vector matching the name of a data source available on the City of
#' St. Louis's open data website.
#'
#' @importFrom sf st_read
#' @importFrom utils download.file
#' @importFrom utils unzip
#'
#' @export
gw_get_city <- function(data) {

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

  tmpdir <- tempdir()
  utils::download.file(url, paste0(tmpdir,"archive.zip"))
  utils::unzip(paste0(tmpdir,"archive.zip"), exdir = tmpdir)
  filepath <- paste0(tmpdir,path)

  if (data == "Land Records"){
    output <- foreign::read.dbf(filepath, as.is = TRUE)
  }
  else {
    output <- sf::st_read(filepath, stringsAsFactors = FALSE)
  }

  unlink(tmpdir)

  return(output)

}
