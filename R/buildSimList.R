#' Build sim list object
#'
#' Build a sim list object to be used in the rest of the functions and returned
#' by \code{projectRoads}. It will convert other types of spatial objects to sf
#' or raster.
#' 

buildSimList <- function(roads, cost, roadMethod, landings){
  if(!is(cost, "RasterLayer")){
    stop("cost must be provided as a RasterLayer", call. = FALSE)
  } 
  
  if(!(is(roads, "sf") || is(roads, "sfc"))){
    if(is(roads, "Spatial")){
      roads <- sf::st_as_sf(roads)
    } else if(is(roads, "Raster")){
      roads <- rasterToLineSegments(roads)
    }
  }
  
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
  if(!all(raster::compareCRS(raster::crs(roads), raster::crs(landings)),
          raster::compareCRS(raster::crs(roads), raster::crs(cost)))){
    stop("the crs of roads, landings and cost must match")
  }
  
  # crop landings and roads to bbox of cost raster
  ext <- sf::st_bbox(cost) %>% as.numeric() %>% 
    `names<-`(c("xmin", "ymin", "xmax", "ymax"))
  landings <- sf::st_crop(landings, ext)
  roads <- sf::st_crop(roads, ext)
  

  sim <- list(roads = roads, costSurface = cost, 
              roadMethod = roadMethod, 
              landings = landings)
}
