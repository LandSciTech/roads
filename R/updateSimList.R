#' Update and existing sim list
#' 
#' Update sim list with new landings and burn in roads to have 0 cost
#' 
#' @param sim sim list from input
#' @param landings input landigs

updateSimList <- function(sim, landings){
  
  # convert landings to sf
  if(!(is(landings, "sf") || is(landings, "sfc"))){
    if(is(landings, "Spatial")){
      
      landings <- sf::st_as_sf(landings)
      
    } else if(is(landings, "Raster")){
      # check if landings are clumps of cells (ie polygons) or single cells
      # (ie points) and if clumps take centroid
      clumpedRast <- raster::clump(landings, gaps = F) 
      
      clumps <- clumpedRast %>% 
        raster::freq(useNA = "no")
      clumps <- max(clumps[,2]) > 1
      
      if(clumps){
        landings <- sf::st_as_sf(raster::rasterToPolygons(clumpedRast, 
                                                          dissolve = TRUE))
      } else {
        landings <- sf::st_as_sf(raster::rasterToPoints(landings, 
                                                        fun = function(x){x > 0}, 
                                                        spatial = TRUE))
      }
      
      
    } else if(is(landings, "matrix")){
      xyind <- which(colnames(landings) %in% c("x", "X", "y", "Y"))
      if(length(xyind) == 0){
        stop("landings matrix must have column name in c('x', 'X', 'y', 'Y')",
             call. = FALSE)
      }
      landings <- sf::st_sf(
        geometry = sf::st_as_sfc(list(sf::st_multipoint(landings[, xyind])))
      ) %>%
        sf::st_cast("POINT")
    } else {
      stop("landings must be either RasterLayer, sf object, SpatialPoints*, ",
           "or SpatialPolygons*",
           call. = FALSE)
    }
  }
  
  if(sf::st_geometry_type(landings, by_geometry = FALSE) %in% 
     c("POLYGON", "MULTIPOLYGON")){
    # Use point on surface not centroid to ensure point is inside irregular polygons
    landings <- sf::st_point_on_surface(landings)
  }
  
  # check crs error if different
  if(!all(raster::compareCRS(raster::crs(sim$roads), raster::crs(landings)))){
    stop("the crs of landings and sim$roads must match")
  }
  
  # Burn roads into cost raster 
  if(is(sim$roads, "Raster")){
    message("Burning in roads to cost raster from road raster")
    sim$costSurface <- sim$costSurface * (sim$roads == 0)
    
    #convert roads to points for analysis
    sim$roads <- raster::rasterToPoints(sim$roads, fun = function(x){x > 0}, 
                                        spatial = TRUE) %>% 
      sf::st_as_sf()
    
  } else {
    message("Burning in roads to cost raster from sf")
    roadsRast <- rasterizeLine(roads, cost, 0) == 0
    cost <- cost * roadsRast
  }
  
  sim$landings <- landings
  
  return(sim)
}