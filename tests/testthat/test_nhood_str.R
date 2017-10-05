context("neighborhood string")

suppressPackageStartupMessages(library(dplyr))

test_data <- data.frame(
  id = c(1, 2, 3, 4, 5),
  nhoodNum = c(1, 2, 4, 5, 3),
  nhoodResult = c("Carondelet", "Patch", "Boulevard Heights", "Bevo Mill", "Holly Hills"),
  stringsAsFactors = FALSE
)

test_that("neighborhood numeric correctly identifies neighborhood as string", {
  result <- nhood_str(test_data$nhoodNum, asFactor = FALSE)

  expect_equal(result, test_data$nhoodResult)

})

test_data <- data.frame(
  id = c(1, 2, 3, 4, 5),
  nhoodNum = c(1, 2, 4, 5, 3),
  nhoodResult = c("Carondelet", "Patch", "Boulevard Heights", "Bevo Mill", "Holly Hills"),
  stringsAsFactors = TRUE
)

test_that("neighborhood numeric correctly identifies neighborhood as factor", {
  result <- nhood_str(test_data$nhoodNum, asFactor = TRUE)

  expect_equal(result, test_data$nhoodResult)

})
