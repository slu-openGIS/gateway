# Access to the City's ArcGIS REST API
# Currently for experimentation, not export

address_candidates <- function(street, zip, address){
  # build a query
  baseURL <- "https://stlgis3.stlouis-mo.gov/arcgis/rest/services/PUBLIC/COMPPARSTRZIPHANDLE/GeocodeServer/findAddressCandidates?"
  if(!missing(street)){street <- URLencode(street)} # optional arg
  else{street = ""}
  if(!missing(address)){address <- URLencode(address)}
  else{address = ""}# optional arg
  if(missing(zip)){zip = ""}

    # always returns JSON
  json <- "&f=pjson"
    # more fields can be implemented later

  query <- paste0(baseURL, "Street=", street, "&ZIP=", zip, "&SingleLine=", address, json)

  # get and parse
  request <- httr::GET(query)
  content <- httr::content(request)
  parsed <- jsonlite::parse_json(content)

  # intialize output
  out <- vector("list", length(parsed[["candidates"]]))

  for (i in 1:length(parsed[["candidates"]])){
    out[[i]] <- data.frame(
    address = parsed[["candidates"]][[i]][["address"]],
    x = parsed[["candidates"]][[i]][["location"]][["x"]],
    y = parsed[["candidates"]][[i]][["location"]][["y"]],
    score = parsed[["candidates"]][[i]][["score"]],
    stringsAsFactors = FALSE)
  }

  out <- dplyr::bind_rows(out)

  return(out)

}
