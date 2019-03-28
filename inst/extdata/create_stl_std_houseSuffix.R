
stl_std_houseSuffix <- dplyr::tibble(
  houseSuf.output = c("F", "F", "R", "R", "1/2"),
  houseSuf.input = c("F", "Front", "R", "Rear", "1/2")
)

stl_std_houseSuffix <- dplyr::arrange(stl_std_houseSuffix, houseSuf.output)

# save as .RData file for inclusion in stldata package

usethis::use_data(stl_std_houseSuffix, overwrite = TRUE)
