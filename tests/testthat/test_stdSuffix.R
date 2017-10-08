context("standardize street suffix")

suppressPackageStartupMessages(library(dplyr))

test_data <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("1st", "1ST", "MLK", "RED FOX", "grand", "SECOND"),
  streetSuf = c("STREET", "Str", "Boulevard", "Drive", "Blvd", "Avenue"),
  streetResult = c("St", "St", "Blvd", "Dr", "Blvd", "Ave"),
  stringsAsFactors = FALSE
)

test_that("suffixes correctly standardized", {
  result <- stdSuffix(test_data, "streetSuf")

  expect_equal(result$streetSuf, test_data$streetResult)

})
