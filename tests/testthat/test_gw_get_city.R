context("city downloads")

# test errors ------------------------------------------------

test_that("input error trigged", {
  expect_error(gw_get_city("shapefile"), "The given repository is not accessible at this time.")
})

# test data ------------------------------------------------

addresses <- gw_get_city("Addresses")

test_that("data is converted to sf", {
  expect_is(addresses, "sf")
})

neighborhoods <- gw_get_city("Neighborhoods")

test_that("data is converted to sf", {
  expect_is(neighborhoods, "sf")
})

landRecords <- gw_get_city("Land Records")

test_that("data is converted to sf", {
  expect_is(landRecords, "tbl_df")
})

landUse <- gw_get_city("Land Use")

test_that("data is converted to sf", {
  expect_is(landUse, "sf")
})

parcels <- gw_get_city("Parcels")

test_that("data is converted to sf", {
  expect_is(parcels, "sf")
})

parks <- gw_get_city("Parks")

test_that("data is converted to sf", {
  expect_is(parks, "sf")
})

police <- gw_get_city("Police Districts")

test_that("data is converted to sf", {
  expect_is(police, "sf")
})

policePre14 <- gw_get_city("Police Districts, Pre-2014")

test_that("data is converted to sf", {
  expect_is(policePre14, "sf")
})

voting <- gw_get_city("Voting Precincts")

test_that("data is converted to sf", {
  expect_is(voting, "sf")
})

zoning <- gw_get_city("Zoning")

test_that("data is converted to sf", {
  expect_is(zoning, "sf")
})

zoningM <- gw_get_city("Zoning, Multi")

test_that("data is converted to sf", {
  expect_is(zoningM, "sf")
})
