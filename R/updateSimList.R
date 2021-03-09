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
        raster::freq(useNA = "no") %>% 
        .[,2] %>% max() > 1
      
      if(clumps){
        landings <- sf::st_as_sf(raster::rasterToPolygons(clumpedRast, 
                                                          dissolve = TRUE))
      } else {
        landings <- sf::st_as_sf(raster::rasterToPoints(landings, 
                                                        fun = function(x){x > 0}, 
                                                        spatial = TRUE))
      }
      
      
    } else if(is(landings, "matrix")){
      landings <- sf::st_sf(
        geometry = sf::st_as_sfc(list(sf::st_multipoint(landings[, c("x", "y")])))
      ) %>%
        sf::st_cast("POINT")
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
    # use stars package because raster::rasterize and raster::mask are slow
    cost_st <- stars::st_as_stars(sim$costSurface)
    
    # rasterize roads to template
    tmplt <- stars::st_as_stars(sf::st_bbox(cost_st), nx = raster::ncol(sim$costSurface),
                                ny = raster::nrow(sim$costSurface), values = 1)
    
    # road raster is 1 where there are no roads and 0 where there are roads
    roads_st <- stars::st_rasterize(sim$roads %>% select(), template = tmplt,
                                    options = "ALL_TOUCHED=TRUE") == 1
    
    cost_st <- cost_st * roads_st
    
    # convert back to Raster from stars
    sim$costSurface <- as(cost_st, "Raster")
    
    rm(cost_st, roads_st, tmplt)
  }
  
  sim$landings <- landings
  
  return(sim)
}