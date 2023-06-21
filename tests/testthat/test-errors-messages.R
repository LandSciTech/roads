
test_that("different CRS is error", {
  cost <- CLUSexample$cost
  raster::crs(cost) <-  "+proj=longlat"
  rds <- CLUSexample$roads
  raster::crs(rds) <-  5070
  expect_error(projectRoads(CLUSexample$landings, cost, rds), "must match")
  

})
