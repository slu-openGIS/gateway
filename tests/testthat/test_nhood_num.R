context("neighborhood numeric")

suppressPackageStartupMessages(library(dplyr))

test_data <- data.frame(
    id = c(1, 2, 3, 4, 5),
    nhoodStr = c("Patch", "Bevo", "Bevo Mill", "Lindenwood Park", "Carondelet"),
    stringsAsFactors = FALSE
    )

test_that("neighborhood numeric correctly identifies neighborhood", {
    result <- nhood_num(test_data$nhoodStr)

  expect_equal(result, c(2, 5, 5, 9, 1))

})
