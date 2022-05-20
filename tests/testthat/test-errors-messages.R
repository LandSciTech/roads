context("Test errors, warnings, and messages")

test_that("different CRS is error", {
  cost <- CLUSexample$cost
  terra::crs(cost) <-  "+proj=longlat"
  rds <- CLUSexample$roads
  terra::crs(rds) <-  5070
  expect_error(projectRoads(CLUSexample$landings, cost, rds), "must match")
  

})
