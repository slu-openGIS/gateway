context("standardize street name")

suppressPackageStartupMessages(library(dplyr))

test_data <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("1st", "1ST", "MLK", "RED FOX", "grand", "SECOND"),
  streetResult = c("1st", "1st", "Dr Martin Luther King", "Redd Foxx", "Grand", "2nd"),
  stringsAsFactors = FALSE
)

test_that("streets correctly standardized, quoted street", {
  result <- gw_stName(test_data, street = "streetStr")

  expect_equal(result$streetStr, test_data$streetResult)

})

test_that("streets correctly standardized, bare street", {
  result <- gw_stName(test_data, street = streetStr)

  expect_equal(result$streetStr, test_data$streetResult)

})

test_that("streets correctly standardized, new quoted street", {
  result <- gw_stName(test_data, street = "streetStr", overwrite = FALSE, newStreet = "newStreet")

  expect_equal(result$newStreet, test_data$streetResult)

})

test_that("streets correctly standardized, new bare street", {
  result <- gw_stName(test_data, street = streetStr, overwrite = FALSE, newStreet = newStreet)

  expect_equal(result$newStreet, test_data$streetResult)

})
