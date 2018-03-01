context("repo downloads")

# test errors ------------------------------------------------

test_that("input error trigged", {
  expect_error(gw_get_repo("shapefile"), "The given repository is not accessible at this time.")
})

# test data ------------------------------------------------

islands <- gw_get_repo("IL_HYDRO_Islands")

test_that("data is converted to sf", {
  expect_is(islands, "sf")
})

mississippi <- gw_get_repo("IL_HYDRO_Mississippi")

test_that("data is converted to sf", {
  expect_is(mississippi, "sf")
})

countyRace <- gw_get_repo("MO_DEMOS_CountiesRace")

test_that("data is converted to sf", {
  expect_is(countyRace, "sf")
})

jeffCityRegion <- gw_get_repo("MO_DEMOS_JeffCityRegion")

test_that("data is converted to sf", {
  expect_is(jeffCityRegion, "sf")
})

tiles <- gw_get_repo("MO_STL_STLTiles")

test_that("data is converted to sf", {
  expect_is(tiles, "sf")
})

stlBoundary <- gw_get_repo("STL_BOUNDARY_City")

test_that("data is converted to sf", {
  expect_is(stlBoundary, "sf")
})

stlHousingAge <- gw_get_repo("STL_HOUSING_MedianAge")

test_that("data is converted to sf", {
  expect_is(stlHousingAge, "sf")
})
