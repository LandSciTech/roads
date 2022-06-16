# devtools::install_github("LandSciTech/pfocal")

# try with real roads using some data from RoadPaper
data_path_raw <- "../RoadPaper/analysis/data/raw_data/"
# modern observed roads
roads <- sf::st_read(paste0(data_path_raw, "roads_revelstoke.shp"))

roadsYear <- 19890000

# filter roads by year to make existing forestry road network
roads[is.na(roads)] <- roadsYear
roadsExist <- filter(roads, AWARD_DATE <= roadsYear)

# cost surface raster layer
bc_cost_surface <- terra::rast(paste0(data_path_raw, "cost_surface_bc_ha.tif"))

# boundary for running projection
tsaBoundary <- sf::st_read(paste0(data_path_raw, "new_tsa27_boundaries.shp"))

tsaCost <- terra::crop(bc_cost_surface, tsaBoundary)

# burn roads into cost raster
roadsExist_rast <- terra::rasterize(terra::vect(roadsExist), terra::rast(tsaCost),
  background = 0
)

src <- roadsExist_rast
maxDist <- 1000

test_that("distance works", {
  # compare the existing version with a version that uses terra::focal instead
  # of pfocal
  
  bm <- bench::mark(
    fastRough = {
      fastRough <- getDistFromSource(src, maxDist, kwidth = 3, dissag = F)
    },
    slowFine = {
      slowFine <- getDistFromSource(src, maxDist, kwidth = 3, dissag = T)
    },
    square = {
      square <- getDistFromSource(src, maxDist, kwidth = 1, dissag = F)
    },
    smootherCircle = {
      smootherCircle <- getDistFromSource(src, maxDist, kwidth = 5, dissag = F)
    },
    check = FALSE
  )

  bm2 <- bench::mark(
    fastRough = {
      fastRough <- getDistFromSource2(src, maxDist, kwidth = 3, dissag = F)
    },
    slowFine = {
      slowFine <- getDistFromSource2(src, maxDist, kwidth = 3, dissag = T)
    },
    square = {
      square <- getDistFromSource2(src, maxDist, kwidth = 1, dissag = F)
    },
    smootherCircle = {
      smootherCircle <- getDistFromSource2(src, maxDist, kwidth = 5, dissag = F)
    },
    check = FALSE
  )

  # Note that the version using terra::focal uses way less memory because it does
  # not read all the values in to memory as a matrix
  bind_rows(bm, bm2) %>% select(expression, total_time, mem_alloc, n_gc)

  # # A tibble: 8 x 4
  # expression     total_time mem_alloc  n_gc
  # <bch:expr>       <bch:tm> <bch:byt> <dbl>
  #   1 fastRough           1.82s   364.2MB     1
  # 2 slowFine           39.35s       8GB    32
  # 3 square              3.38s   910.3MB     3
  # 4 smootherCircle      1.28s   182.1MB     0
  # 5 fastRough           1.77s    83.7KB     0
  # 6 slowFine            41.8s   319.2KB     1
  # 7 square              4.07s   225.6KB     0
  # 8 smootherCircle      1.57s      57KB     0


  res <- c(fastRough, slowFine, square, smootherCircle)
  names(res) <- c("fastRough", "slowFine", "square", "smootherCircle")
  plot(res)
})

test_that("distance to roads has expected values", {
  src <- demoScen[[1]]$road.rast
  maxDist <- 100
  fastRough <- getDistFromSource(src, maxDist, kwidth = 3, dissag = F)
  slowFine <- getDistFromSource(src, maxDist, kwidth = 3, dissag = T)
  square <- getDistFromSource(src, maxDist, kwidth = 1, dissag = F)
  wideCircle <- getDistFromSource(src, maxDist, kwidth = 5, dissag = F)
  smootherCircle <- getDistFromSource(src, maxDist, kwidth = 10, dissag = T)
  res <- c(fastRough, slowFine, square, wideCircle, smootherCircle)
  names(res) <- c("kwidth = 3, dissag = F", "kwidth = 3, dissag = T",
                  "kwidth = 1, dissag = F", "kwidth = 5, dissag = F", 
                  "kwidth = 10, dissag = T")
  tmap::qtm(res %>% terra::`crs<-`(value = "EPSG:5070"), raster.style = "cont")
  
})
