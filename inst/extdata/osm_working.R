library(dplyr)
library(sf)
library(tmaptools)

x <- tibble(id = c(1:2), address = c("I 44 and S Vandeventer Ave", "O'Fallon Park"))
y <- tibble(id = c(1:2), address = c("I 44 and S Vandeventer Ave, St. Louis", "I 64 and S Vandeventer Ave, St. Louis"))
z <- tibble(id = c(1:2), address = c("I-44 and S Grand Blvd, St. Louis", "I-64 and S Grand Blvd, St. Louis"))
z2 <- tibble(id = c(1:2), address = c("Interstate 44 and S Grand Blvd, St. Louis", "O'Fallon Park, St. Louis"))
z3 <- tibble(id = c(1:2), address = c("Interstate 44 and S Grand Blvd, St. Louis", "Interstate 64 and S Grand Blvd, St. Louis"))

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

tryCatch(
  {suppressWarnings(geocode_OSM(x$address, as.sf = TRUE))},
  error=function(cond) {
    return(NA)
  })

tryCatch(
  {suppressWarnings(geocode_OSM(y$address, as.sf = TRUE))},
  error=function(cond) {
    return(NA)
  })

tryCatch(
  {suppressWarnings(geocode_OSM(z3$address, as.sf = TRUE))},
  error=function(cond) {
    return(NULL)
  })
