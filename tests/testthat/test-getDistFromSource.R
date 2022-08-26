# devtools::install_github("LandSciTech/pfocal")

#make example roads from scratch
rds <- data.frame(x = 1:1000/100, y = cos(1:1000/100)) %>% 
  sf::st_as_sf(coords = c("x", "y")) %>% 
  sf::st_union() %>% 
  sf::st_cast("LINESTRING")

rds_rast <- terra::rasterize(terra::vect(rds), 
                             terra::rast(nrows = 50, ncols = 50, 
                                         xmin = 0, xmax = 10, 
                                         ymin = -5, ymax = 5),
                             touches = TRUE)


test_that("distance to roads has expected values", {
  src <- rds_rast
  maxDist <- 5
  fastRough <- getDistFromSource(src, maxDist, kwidth = 1)
  slowFine <- getDistFromSource(src, maxDist, kwidth = 1, method = "pfocal2")
  wideCircle <- getDistFromSource(src, maxDist, kwidth = 5)
  smootherCircle <- getDistFromSource(src, maxDist, kwidth = 5, method = "pfocal2")
  res <- c(fastRough, slowFine, wideCircle, smootherCircle)
  names(res) <- c("kwidth = 1, terra", "kwidth = 1, pfocal2", "kwidth = 5, terra", 
                  "kwidth = 5, pfocal2")
  tmap::qtm(res %>% terra::`crs<-`(value = "EPSG:5070"), raster.style = "cont")
  
  expect_gt(length(unique(fastRough)[[1]]), length(unique(wideCircle)[[1]]))
  
})


# TODO: add tests for treatment of NAs