
stl_std_directions <- dplyr::tibble(
  dir.output = c("E", "N", "S", "EN", "W", "EW", "WN", "WW", "SS", "WE", "SE"),
  dir.input = c("E", "N", "S", "EN", "W", "EW", "WN", "WW", "SS", "WE", "SE")
)

stl_std_directions <- dplyr::arrange(stl_std_directions, dir.output)

# save as .RData file for inclusion in stldata package

usethis::use_data(stl_std_directions, overwrite = TRUE)
