dataCorr1 <- data.frame(
  dc_correct = c("1ST", "2ND", "4TH", "6TH", "7TH", "8TH", "9TH",
                 "10TH", "11TH", "13TH", "14TH", "15TH",
                 "16TH", "17TH","18TH", "19TH", "20TH",
                 "21ST", "21ST", "22ND", "22ND",
                 "23RD", "23RD", "25TH", "25TH"),
  dc_incorrect = c("FIRST", "SECOND", "FOURTH", "SIXTH", "SEVENTH", "EIGHTH", "NINTH",
                   "TENTH", "ELEVENTH", "THIRTEENTH", "FOURTEENTH", "FIFTEENTH",
                   "SIXTEENTH", "SEVENTEENTH", "EIGHTEENTH", "NINETEENTH", "TWENTIETH",
                   "TWENTY-FIRST", "TWENTY FIRST", "TWENTY-SECOND", "TWENTY SECOND",
                   "TWENTY-THIRD", "TWENTY THIRD", "TWENTY-FIFTH", "TWENTY FIFTH"),
  stringsAsFactors = FALSE
)

dataCorr2 <- data.frame(
  dc_correct = c("KINGSHIGHWAY", "DR MARTIN LUTHER KING", "DR MARTIN LUTHER KING", "REDD FOXX",
                 "DR SAMUEL T SHEPARD", "DR SAMUEL T SHEPARD", "DR SAMUEL T SHEPARD", "MOUNT PLEASANT",
                 "MORGAN FORD"),
  dc_incorrect = c("KINGSHIGHWAY MEMORIAL", "MARTIN LUTHER KING", "MLK", "RED FOX",
                   "SAMUEL T SHEPARD", "SAMUEL SHEPARD", "SAM SHEPARD", "MT PLEASANT",
                   "MORGANFORD"),
  stringsAsFactors = FALSE
)

stdStreets <- dplyr::bind_rows(dataCorr1, dataCorr2)
stdStreets <- dplyr::arrange(dataCorr, dc_correct)

# save as .RData file for inclusion in stldata package

save(stdStreets, file = "data/stdSteets.RData")
