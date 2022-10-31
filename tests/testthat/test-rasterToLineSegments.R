test_that("inspect line segments", {
  roadRast <- demoScen[[8]]$road.rast
  # Note this is imperfect because the line is doubled where the two roads
  # intersect
  expect_warning(roadLine <- rasterToLineSegments(roadRast))
  
  roadLine2 <- rasterToLineSegments(roadRast, method = "nearest")
  
  expect_s3_class(roadLine, "sf")
  expect_s3_class(roadLine2, "sf")
  # plot(roadLine)
  # plot(roadLine2)
  # 
  # bm <- bench::mark(old = rasterToLineSegments(roadRast), 
  #                   new = rasterToLineSegments2(roadRast), 
  #                   check = FALSE)
  # 
  # 
  # # try with real roads using some data from RoadPaper
  # data_path_raw <- "../RoadPaper/analysis/data/raw_data/"
  # #modern observed roads
  # roads <- sf::st_read(paste0(data_path_raw, "roads_revelstoke.shp"))
  # 
  # roadsYear <- 19890000
  # 
  # #filter roads by year to make existing forestry road network
  # roads[is.na(roads)] <- roadsYear
  # roadsExist <- filter(roads, AWARD_DATE <= roadsYear)
  # 
  # #cost surface raster layer
  # bc_cost_surface <- terra::rast(paste0(data_path_raw, "cost_surface_bc_ha.tif"))
  # 
  # #boundary for running projection
  # tsaBoundary <- sf::st_read(paste0(data_path_raw, "new_tsa27_boundaries.shp"))
  # 
  # tsaCost <- terra::crop(bc_cost_surface, tsaBoundary)
  # 
  # # burn roads into cost raster
  # roadsExist_rast <- terra::rasterize(terra::vect(roadsExist), terra::rast(tsaCost),
  #                                     background = 0)
  # bm2 <- bench::mark({
  #   # roadLine <- rasterToLineSegments(as(roadsExist_rast, "Raster"), method = "nearest")
  #   ## fails due to not enough memory
  # },
  # {
  #   roadLine2 <- rasterToLineSegments(as(roadsExist_rast, "Raster"), method = "mst")
  #   ## Aborts R session
  # }, check = FALSE, iterations = 1)
  
})
