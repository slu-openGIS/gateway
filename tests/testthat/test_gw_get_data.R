context("city downloads")

# test errors ------------------------------------------------

test_that("input error trigged", {
  expect_error(gw_get_data("shapefile"), "The given repository is not accessible at this time.")
})

# test data ------------------------------------------------

addresses <- gw_get_data(data = "Addresses", class = "sf")

test_that("data is converted to sf", {
  expect_is(addresses, "sf")
})

neighborhoods <- gw_get_data(data = "Neighborhoods", class = "sf")

test_that("data is converted to sf", {
  expect_is(neighborhoods, "sf")
})

landRecords <- gw_get_data(data = "Land Records", class = "tibble")

test_that("data is converted to sf", {
  expect_is(landRecords, "tbl_df")
})

landUse <- gw_get_data(data = "Land Use", class = "sf")

test_that("data is converted to sf", {
  expect_is(landUse, "sf")
})

parcels <- gw_get_data(data = "Parcels", class = "sf")

test_that("data is converted to sf", {
  expect_is(parcels, "sf")
})

parks <- gw_get_data(data = "Parks", class = "sf")

test_that("data is converted to sf", {
  expect_is(parks, "sf")
})

police <- gw_get_data(data = "Police Districts", class = "sf")

test_that("data is converted to sf", {
  expect_is(police, "sf")
})

policePre14 <- gw_get_data(data = "Police Districts, Pre-2014", class = "sf")

test_that("data is converted to sf", {
  expect_is(policePre14, "sf")
})

voting <- gw_get_data(data = "Voting Precincts", class = "sf")

test_that("data is converted to sf", {
  expect_is(voting, "sf")
})

zoning <- gw_get_data(data = "Zoning", class = "sf")

test_that("data is converted to sf", {
  expect_is(zoning, "sf")
})

zoningM <- gw_get_data(data = "Zoning, Multi", class = "sf")

test_that("data is converted to sf", {
  expect_is(zoningM, "sf")
})
