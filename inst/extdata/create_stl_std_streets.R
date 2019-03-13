

stl_std_streets <- dplyr::tibble(
  correct = c("Kingshighway", "Dr Martin Luther King", "Dr Martin Luther King", "Redd Foxx",
                 "Dr Samuel T Shepard", "Dr Samuel T Shepard", "Dr Samuel T Shepard", "Mount Pleasant",
                 "Morgan Ford", "Lasalle","Lambdin", "Bishop P L Scott"),
  incorrect = c("Kingshighway Memorial", "Martin Luther King", "Mlk", "Red Fox",
                   "Samuel T Shepard", "Samuel Shepard", "Sam Shepard", "Mt Pleasant",
                   "Morganford", "La Salle", "Lamblin", "Bishop Scott")
)

stl_std_streets <- dplyr::arrange(stl_std_streets, correct)

# save as .RData file for inclusion in stldata package

usethis::use_data(stl_std_streets, overwrite = TRUE)
