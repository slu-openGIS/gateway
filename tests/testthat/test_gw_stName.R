context("standardize street name")

suppressPackageStartupMessages(library(dplyr))

test_data <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("1st", "1ST", "MLK", "RED FOX", "grand", "SECOND"),
  streetResult = c("1st", "1st", "Dr Martin Luther King", "Redd Foxx", "Grand", "2nd"),
  stringsAsFactors = FALSE
)

test_that("streets correctly standardized", {
  result <- gw_stName(test_data, "streetStr")

  expect_equal(result$streetStr, test_data$streetResult)

})
