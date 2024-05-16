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
  expect_warning(fastRough <- getDistFromSource(src, maxDist, kwidth = 1))
  expect_warning(wideCircle <- getDistFromSource(src, maxDist, kwidth = 5))
  if(requireNamespace("pfocal", quietly = TRUE)){
    expect_warning(slowFine <- getDistFromSource(src, maxDist, kwidth = 1, method = "pfocal2"))
    expect_warning(smootherCircle <- getDistFromSource(src, maxDist, kwidth = 5, method = "pfocal2"))
  }

  if(interactive()){
    res <- c(fastRough, slowFine, wideCircle, smootherCircle)
    names(res) <- c("kwidth = 1, terra", "kwidth = 1, pfocal2", "kwidth = 5, terra", 
                    "kwidth = 5, pfocal2")
    tmap::qtm(res %>% terra::`crs<-`(value = "EPSG:5070"), raster.style = "cont")
  }
  
  # no difference any more since using terra::distance
  expect_equal(length(terra::unique(fastRough)[[1]]), length(terra::unique(wideCircle)[[1]]))
  
})


# TODO: add tests for treatment of NAs