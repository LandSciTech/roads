# test projectRoads works for all the various input and output options

# options:
## ROADS
# 0/1 raster
# 0 < raster
# sp lines
# sf lines
## COST
# raster with 0 for existing roads
# raster with no zeros
## LANDINGS
# raster (currently only 1 layer allowed)
# clumped raster
# sp points
# sp polygons
# sf points
# sf polygons
scen <- demoScen[[1]]
doPlot <- interactive()
test_that("cost and road options work", {
  
  # 0/1 raster
  projectRoads(scen$landings.points.sf, scen$cost.rast,
               scen$road.rast, plotRoads = doPlot)
  
  # output as 

  # 0< raster 
  roadInt <- scen$road.rast
  suppressWarnings(roadInt[roadInt == 1] <- 1:100)
  
  costNo0 <- scen$cost.rast
  costNo0[costNo0 == 0] <- 10
  
  projectRoads(scen$landings.points.sf, scen$cost.rast,
               roadInt, plotRoads = doPlot)
  
  # try with no 0 in cost in case that compensated for roads
  expect_message(projectRoads(scen$landings.points.sf, costNo0,
                              roadInt, plotRoads = doPlot),
                 "No 0s detected")
  
  # sp lines
  projectRoads(scen$landings.points.sf, scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  # sf lines
  projectRoads(scen$landings.points.sf, scen$cost.rast,
               scen$road.line.sf, plotRoads = doPlot)
  
  # burn in roads as 0 when cost raster has no 0
  expect_message(projectRoads(scen$landings.points.sf, costNo0,
                              scen$road.line, plotRoads = doPlot, 
                              roadsInCost = FALSE),
                 "Burning in roads")
})

test_that("landings options work", {
  # raster (currently only 1 layer allowed)
  projectRoads(scen$landings.stack[[1]], scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  expect_error(projectRoads(scen$landings.stack, scen$cost.rast,
                            scen$road.line, plotRoads = doPlot),
               "landings cannot be a RasterStack")
  
  # clumped raster
  projectRoads(raster::rasterize(scen$landings.poly, scen$cost.rast), 
               scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  # sp points
  projectRoads(scen$landings.points, scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  # sp polygons
  projectRoads(scen$landings.poly, scen$cost.rast,
               scen$road.line, plotRoads = doPlot)

  # sf points
  projectRoads(scen$landings.points.sf, scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  # sf polygons
  projectRoads(scen$landings.poly.sf, scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
})