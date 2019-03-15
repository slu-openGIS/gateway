x <- tibble(id = c(1:2), address = c("4247 Botancial Ave St Louis 63110", "4247 Botanical Ave St Louis 63110"))

tryCatch(
  {geocode_OSM(x$address, as.sf = TRUE)},
  error=function(cond) {
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  warning=function(cond) {
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  },
  finally={
    message("Some other message at the end")
  })
