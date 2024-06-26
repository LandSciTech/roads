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
demoScen <- prepExData(demoScen)
scen <- demoScen[[1]]
doPlot <- interactive()
test_that("cost and road options work", {
  
  # 0/1 raster
  out <- projectRoads(scen$landings.points, scen$cost.rast,
               scen$road.rast, plotRoads = doPlot)
  
  expect_s4_class(out$roads, "SpatRaster")
  
  # 0< raster 
  roadInt <- scen$road.rast
  roadInt[roadInt == 1] <- 1:nrow(roadInt[roadInt == 1])
  
  costNo0 <- scen$cost.rast
  costNo0[costNo0 == 0] <- 10
  
  projectRoads(scen$landings.points, scen$cost.rast,
               roadInt, plotRoads = doPlot)
  
  # try with no 0 in cost in case that compensated for roads
  expect_warning(projectRoads(scen$landings.points, costNo0,
                              roadInt, plotRoads = doPlot),
                 "No 0s detected")
  
  # all input roads should be included in output
  out2 <- projectRoads(scen$landings.points, costNo0,
               roadInt, plotRoads = doPlot, roadsInWeight = FALSE)
  
  inrd <- terra::cells(terra::rast(roadInt), 1:100)
  
  expect_true(all(terra::extract(out2$roads, inrd[[1]])[1] == 1))
  
  # sp lines
  out2 <- projectRoads(scen$landings.points, scen$cost.rast,
               scen$road.line %>% sf::as_Spatial(), plotRoads = doPlot)
  expect_s3_class(out2$roads, "sf")
  
  # sf lines
  projectRoads(scen$landings.points, scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  # burn in roads as 0 when cost raster has no 0
  expect_message(projectRoads(scen$landings.points, costNo0,
                              scen$road.line, plotRoads = doPlot, 
                              roadsInWeight = FALSE),
                 "Burning in roads")
})

test_that("landings options work", {
  # raster (currently only 1 layer allowed)
  projectRoads(scen$landings.stack[[1]], scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  expect_error(projectRoads(scen$landings.stack, scen$cost.rast,
                            scen$road.line, plotRoads = doPlot),
               "single layer")
  
  # clumped raster
  projectRoads(terra::rasterize(scen$landings.poly, scen$cost.rast), 
               scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  # sp points
  projectRoads(scen$landings.points %>% sf::as_Spatial(), scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  # sp polygons
  projectRoads(scen$landings.poly, scen$cost.rast,
               scen$road.line, plotRoads = doPlot)

  # sf points
  projectRoads(scen$landings.points, scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  # sf polygons
  projectRoads(scen$landings.poly, scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
  
  # matrix
  projectRoads(sf::st_coordinates(scen$landings.poly), scen$cost.rast,
               scen$road.line, plotRoads = doPlot)
})

test_that("sim list input works", {
  simList <- projectRoads(scen$landings.poly, scen$cost.rast,
                          scen$road.line, plotRoads = doPlot)
  lnd2 <- scen$landings.points %>% filter(set == 2)
  expect_type(projectRoads(sim = simList, landings = lnd2, plotRoads = doPlot), 
              "list")
  
})

test_that("input types are tested", {
  expect_error(projectRoads("string", scen$cost.rast,
               scen$road.line, plotRoads = doPlot),
               "must be either")
  expect_error(projectRoads(scen$landings.points, "sting",
                            scen$road.line, plotRoads = doPlot),
               "must be .* RasterLayer")
  expect_error(projectRoads(scen$landings.points, scen$cost.rast,
                            "string", plotRoads = doPlot),
               "must be either")
})

test_that("duplicate roads are not created", {
  res <- projectRoads(scen$landings.points, scen$cost.rast,
                      scen$road.line, plotRoads = doPlot, roadsInWeight = FALSE)
  
  #useful to visualize need to load fun from RoadsPaper
  # dens <- res$roads %>% rasterizeLineDensity(r = res$weightRaster)
  
  res_mst <- projectRoads(scen$landings.points, scen$cost.rast,
                      scen$road.line, plotRoads = doPlot, roadMethod = "mst")
  
  # dens_mst <- res_mst$roads %>% rasterizeLineDensity(r = res_mst$weightRaster)
  
  expect_equal(sf::st_union(res$roads) %>% sf::st_length(), sf::st_length(res$roads) %>% sum())
  
  # expect_equal(sf::st_union(res_mst$roads) %>% sf::st_length(), sf::st_length(res_mst$roads) %>% sum())
  # # draw extent to get problem tiny line created
  # plot(scen$cost.rast)
  # plot(scen$landings.points, add = TRUE)
  # ext <- terra::draw()
  # 
  # lnds <- sf::st_crop(scen$landings.points, ext)
  # cst <- terra::crop(scen$cost.rast, ext)
  # rds <- sf::st_crop(scen$road.line, ext)
  # 
  # projectRoads(lnds, cst, rds, plotRoads = doPlot, roadsInWeight = FALSE)
  
})

test_that("landings on road or multiple landings in same cell work", {
  CLUSexample <- prepExData(CLUSexample)
  
  CLUSexample$landings <- bind_rows(CLUSexample$landings, 
                                    list(sf::st_point(c(1.5, 4.2)), sf::st_point(c(1.5, 0.6))) %>% 
                                      sf::st_as_sfc() %>%
                                      sf::st_as_sf(crs = sf::st_crs(CLUSexample$landings)) %>%
                                      rename(geometry = x))
  expect_type(
    projectRoads(CLUSexample$landings, CLUSexample$cost, CLUSexample$roads, 
                 roadMethod = "mst"),
    "list")
  
  
})

test_that("Works with GEOMETRY input", {
  lndPoly <- demoScen[[1]]$landings.poly %>% sf::st_as_sf() %>% 
    sf::st_set_agr("constant")
  lndPoly[6, 2] <- lndPoly[6, 2] %>% sf::st_cast("MULTIPOLYGON")
  expect_type(projectRoads(lndPoly, scen$cost.rast, scen$road.line), "list")
})


if(FALSE){
  # checking memory allocations
  bm1 <- bench::mark(projectRoads(scen$landings.points, scen$cost.rast,
                                  scen$road.line, plotRoads = doPlot))
  
  # change sim to an env
  bm2 <- bench::mark(projectRoads(scen$landings.points, scen$cost.rast,
                                  scen$road.line, plotRoads = doPlot))
  
  # look at top memory allocs
  bm1$memory[[1]] %>% as.data.frame() %>% slice_max(bytes, n = 5)
  bm2$memory[[1]] %>% as.data.frame() %>% slice_max(bytes, n = 5)
  
  # was able to reduce so only copies graph once in each iteration of iterativeShortestPAth
  
}
