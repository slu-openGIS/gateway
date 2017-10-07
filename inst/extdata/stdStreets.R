dataCorr1 <- data.frame(
  dc_correct = c("1st", "2nd", "4th", "6th", "7th", "8th", "9th",
                 "10th", "11th", "13th", "14th", "15th",
                 "16th", "17th","18th", "19th", "20th",
                 "21st", "21st", "22nd", "22nd",
                 "23rd", "23rd", "25th", "25th"),
  dc_incorrect = c("First", "Second", "Fourth", "Sixth", "Seventh", "Eighth", "Ninth",
                   "Tenth", "Eleventh", "Thirteenth", "Fourteenth", "Fifteenth",
                   "Sixteenth", "Seventeenth", "Eighteenth", "Nineteenth", "Twentieth",
                   "Twenty-First", "Twenthy First", "Twenty-Second", "Twenty Second",
                   "Twenty-Third", "Twenty Third", "Twenty-Fifth", "Twenty Fifth"),
  stringsAsFactors = FALSE
)

dataCorr2 <- data.frame(
  dc_correct = c("1st", "2nd", "4th", "6th", "7th", "8th", "9th",
                 "10th", "11th", "13th", "14th", "15th",
                 "16th", "17th","18th", "19th", "20th",
                 "21st", "22nd", "23rd", "25th"),
  dc_incorrect = c("1St", "2Nd", "4Th", "6Th", "7Th", "8Th", "9Th",
                   "10Th", "11Th", "13Th", "14Th", "15Th",
                   "16Th", "17Th","18Th", "19Th", "20Th",
                   "21St", "22Nd", "23Rd", "25Th"),
  stringsAsFactors = FALSE
)

dataCorr3 <- data.frame(
  dc_correct = c("Kingshighway", "Dr Martin Luther King", "Dr Martin Luther King", "Redd Foxx",
                 "Dr Samuel T Shepard", "Dr Samuel T Shepard", "Dr Samuel T Shepard", "Mount Pleasant",
                 "Morgan Ford"),
  dc_incorrect = c("Kingshighway Memorial", "Martin Luther King", "Mlk", "Red Fox",
                   "Samuel T Shepard", "Samuel Shepard", "Sam Shepard", "Mt Pleasant",
                   "Morganford"),
  stringsAsFactors = FALSE
)

stdStreets <- dplyr::bind_rows(dataCorr1, dataCorr2)
stdStreets <- dplyr::bind_rows(stdStreets, dataCorr3)
stdStreets <- dplyr::arrange(stdStreets, dc_correct)

# save as .RData file for inclusion in stldata package

save(stdStreets, file = "data/stdSteets.RData")
