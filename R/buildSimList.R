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
      roads <- sf::st_as_sf(raster::rasterToPoints(roads, 
                                                   fun = function(x){x > 0}, 
                                                   spatial = TRUE))
    }
  }
  
  if(!(is(landings, "sf") || is(landings, "sfc"))){
    if(is(landings, "Spatial")){
      landings <- sf::st_as_sf(landings)
    } else if(is(landings, "Raster")){
      landings <- sf::st_as_sf(raster::rasterToPoints(landings, 
                                                      fun = function(x){x > 0}, 
                                                      spatial = TRUE))
    }
  }
  
  # check crs error if different
  if(!all(sf::st_crs(roads) == sf::st_crs(landings),
          sf::st_crs(roads) == sf::st_crs(cost))){
    stop("the crs of roads, landings and cost must match")
  }

  sim <- list(roads = roads, costSurface = cost, 
              roadMethod = roadMethod, 
              landings = landings)
}
