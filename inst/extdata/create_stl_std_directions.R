
stl_std_directions <- dplyr::tibble(
  dir.output = c("EN", "W", "EW", "WN", "WW", "SS", "WE"),
  dir.input = c("EN", "W", "EW", "WN", "WW", "SS", "WE")
)

stl_std_directions <- dplyr::arrange(stl_std_directions, dir.output)

# save as .RData file for inclusion in stldata package

usethis::use_data(stl_std_directions, overwrite = TRUE)
