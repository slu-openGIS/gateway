test <- dplyr::tibble(
  id = c(1:6),
  data = c("high", "low", "high", "low", "low", "high"),
  nhood = c("SOUTHWEST GARDEN", "Marine Villa", "Forest Park South East", "Cheltenham", "DeBaliviere Place", "Hi-Pointe"),
  nhood_num = c(13, 18, 39, 41, 47, 44)
)

gw_nhood(test, var = nhood, to = "numeric")
gw_nhood(test, var = nhood_num, to = "string")
