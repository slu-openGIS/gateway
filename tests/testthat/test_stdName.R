context("standardize street name")

suppressPackageStartupMessages(library(dplyr))

test_data <- data.frame(
  id = c(1, 2, 3, 4, 5),
  streetStr = c("1st", "1ST", "MLK", "RED FOX", "grand"),
  streetResult = c("1st", "1st", "Dr Martin Luther King", "Redd Foxx", "Grand"),
  stringsAsFactors = FALSE
)

test_that("streets correctly standardized", {
  result <- stdName(test_data, "streetStr")

  expect_equal(result$streetStr, test_data$streetResult)

})
