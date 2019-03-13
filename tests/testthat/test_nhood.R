context("test neighborhood conversion")

test_data <- data.frame(
    id = c(1:5),
    nhood_str = c("Patch", "BEVO", "Bevo Mill", "Lindenwood Park", "Carondelet"),
    nhood_str_result = c("Patch", "Bevo Mill", "Bevo Mill", "Lindenwood Park", "Carondelet"),
    nhood_num = c(2, 5, 5, 9, 1),
    stringsAsFactors = FALSE
    )

result1 <- gw_nhood(test_data, var = nhood_str, new_var = result, to = "numeric")
result2 <- gw_nhood(test_data, var = nhood_num, new_var = result, to = "string")

test_that("neighborhood numeric correctly identifies neighborhood", {
  expect_equal(test_data$nhood_num, result1$result)
  expect_equal(test_data$nhood_str_result, result2$result)
})
