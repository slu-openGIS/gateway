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
  result <- gw_suffix(test_data, suffix = "streetSuf")

  expect_equal(result$streetSuf, test_data$streetResult)

})

test_that("suffix argument accepts bare input", {
  result <- gw_suffix(test_data, suffix = streetSuf)

  expect_equal(result$streetSuf, test_data$streetResult)

})

test_that("newSuffix argument accepts quoted input", {
  result <- gw_suffix(test_data, suffix = streetSuf, overwrite = FALSE, newSuffix = "suffix")

  expect_equal(result$suffix, test_data$streetResult)

})

test_that("newSuffix argument accepts bare input", {
  result <- gw_suffix(test_data, suffix = streetSuf, overwrite = FALSE, newSuffix = suffix)

  expect_equal(result$suffix, test_data$streetResult)

})
