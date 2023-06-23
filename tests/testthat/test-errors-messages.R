CLUSexample <- prepExData(CLUSexample)
test_that("different CRS is error", {
  cost <- CLUSexample$cost
  terra::crs(cost) <-  "+proj=longlat"
  rds <- CLUSexample$roads
  terra::crs(rds) <-  "EPSG:5070"
  expect_error(projectRoads(CLUSexample$landings, cost, rds), "must match")
  

})
