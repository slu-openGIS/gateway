suffixCorr1 <- data.frame(
  sc_primary = c("Alley", "Alley", "Alley", "Alley", "Anex", "Anex", "Anex", "Anex", "Arcade", "Arcade"),
  sc_commonAbbrev = c("Alley", "Allee", "Ally", "Aly", "Anex", "Annex", "Annx", "Anx", "Arc", "Arcade"),
  sc_prefAbbrev = c("Aly", "Aly", "Aly", "Aly", "Anx", "Anx", "Anx", "Anx", "Arc", "Arc"),
  stringsAsFactors = FALSE
)

suffixCorr2 <- data.frame(
  sc_primary = c("Avenue", "Avenue", "Avenue", "Avenue", "Avenue", "Avenue", "Avenue", "Bayou", "Bayou", "Bayou"),
  sc_commonAbbrev = c("Av", "Ave", "Aven", "Avenu", "Avenue", "Avn", "Avnue", "Bayoo", "Byu", "Bayou"),
  sc_prefAbbrev = c("Ave", "Ave", "Ave", "Ave", "Ave", "Ave", "Ave", "Byu", "Byu", "Byu"),
  stringsAsFactors = FALSE
)

suffixCorr3 <- data.frame(
  sc_primary = c("Beach", "Beach", "Bend", "Bend", "Bluff", "Bluff", "Bluff", "Bluffs"),
  sc_commonAbbrev = c("Beach", "Bch", "Bend", "Bnd", "Blf", "Bluf", "Bluff", "Bluffs"),
  sc_prefAbbrev = c("Bch", "Bch", "Bnd", "Bnd", "Blf", "Blf", "Blf", "Blfs"),
  stringsAsFactors = FALSE
)

suffixCorr4 <- data.frame(
  sc_primary = c("Bottom", "Bottom", "Bottom", "Bottom", "Boulevard", "Boulevard", "Boulevard", "Boulevard"),
  sc_commonAbbrev = c("Bot", "Btm", "Bottm", "Bottom", "Blvd", "Boul", "Boulv", "Boulevard"),
  sc_prefAbbrev = c("Btm", "Btm", "Btm", "Btm", "Blvd", "Blvd", "Blvd", "Blvd"),
  stringsAsFactors = FALSE
)

suffixCorr5 <- data.frame(
  sc_primary = c("Branch", "Branch", "Branch", "Bridge", "Bridge", "Bridge", "Brook", "Brook", "Brooks", "Brooks"),
  sc_commonAbbrev = c("Br", "Brnch", "Branch", "Brdge", "Brg", "Bridge", "Brook", "Brk", "Brooks", "Brks"),
  sc_prefAbbrev = c("Br", "Br", "Br", "Brg", "Brg", "Brg", "Brk", "Brk", "Brks", "Brks"),
  stringsAsFactors = FALSE
)

stdSuffix <- dplyr::bind_rows(suffixCorr1, suffixCorr2)
stdSuffix <- dplyr::bind_rows(stdSuffix, suffixCorr3)
stdSuffix <- dplyr::bind_rows(stdSuffix, suffixCorr4)
stdSuffix <- dplyr::bind_rows(stdSuffix, suffixCorr5)
stdSuffix <- dplyr::arrange(stdSuffix, sc_primary, sc_commonAbbrev)

# save as .RData file for inclusion in stldata package

save(stdSuffix, file = "data/stdSuffix.RData")

suffixCorr5 <- data.frame(
  sc_primary = c("", ""),
  sc_commonAbbrev = c("", ""),
  sc_prefAbbrev = c("", ""),
  stringsAsFactors = FALSE
)
