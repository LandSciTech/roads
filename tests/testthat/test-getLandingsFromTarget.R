context("test getLandingsFromTarget works for different scenarios")


lndsTest <- list("centroid", 1, 0.5, 0.25, 0.1, 0.01, 0.001)

lndPoly <- demoScen[[1]]$landings.poly %>% sf::st_as_sf() %>% 
  sf::st_set_agr("constant")

test_that("sf input polygons work", {
  # replicate because some errors only happened wit specific samples
  outs <- replicate(20, 
                    {purrr::map(lndsTest, 
                                ~getLandingsFromHarvest(lndPoly, 
                                                        numLandings = .x))},
                    simplify = FALSE)
  expect_type(outs, "list")
})

test_that("sp polygon input works",{
 outs <- purrr::map(lndsTest, 
             ~getLandingsFromHarvest(demoScen[[1]]$landings.poly, 
                                     numLandings = .x))
 expect_type(outs, "list")
})

test_that("raster no clumps input works",{
 out <- getLandingsFromHarvest(demoScen[[1]]$landings.stack[[1]])
 expect_warning(getLandingsFromHarvest(demoScen[[1]]$landings.stack[[1]], 0.5),
                "numLandings is ignored")
})

test_that("raster with clumps input works",{
  rast <- demoScen[[1]]$landings.poly %>% raster::rasterize(demoScen[[1]]$cost.rast)
  # make sure that a single celled havest block will work with clumps
  rast[10,10] <- 6
  
  out <- getLandingsFromHarvest(rast, 0.1)
  out <- getLandingsFromHarvest(rast)
  
  expect_type(out, "list")
})

# compare to old version
test_that("old version is similar to new", {
  rast <- demoScen[[1]]$landings.poly %>% raster::rasterize(demoScen[[1]]$cost.rast)
  # make sure that a single celled havest block will work with clumps
  rast[10,10] <- 6
  outOld <- getLandingsFromTarget(rast > 0, numLandings = 0.1)
  outOldRand <- getLandingsFromTarget(rast > 0, numLandings = 0.1, 
                                      sampleType = "random")
})

