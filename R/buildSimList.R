#' Build sim list object
#'
#' Build a sim list object to be used in the rest of the functions and returned
#' by \code{projectRoads}. It will convert other types of spatial objects to sf
#' or raster.
#' 
#' @param roads roads input
#' @param cost cost input
#' @param roadMethod method of road projection
#' @param landings landings input
#' @param roadsInCost Whether the roads have already been burned into cost

buildSimList <- function(roads, cost, roadMethod, landings, roadsInCost){
  if(!is(cost, "RasterLayer")){
    stop("cost must be provided as a RasterLayer", call. = FALSE)
  } 
  if(0 %in% raster::unique(cost)){
    message("0s detected in cost raster, these will be considered as existing roads")
  } else if(roadsInCost){
    warning("No 0s detected in cost raster. If existing roads have not been ",
            "included in the cost raster set roadsInCost = FALSE to have them ",
            "burned in")
  }
  
  # Burn roads into raster if not already for raster roads before converting 
  if(!roadsInCost && is(roads, "Raster")){
    message("Burning in roads to cost raster from road raster")
    cost <- cost * (roads == 0)
    roadsInCost <- TRUE
  }
  
  if(!(is(roads, "sf") || is(roads, "sfc"))){
    if(is(roads, "Spatial")){
      roads <- sf::st_as_sf(roads) %>% 
        sf::st_set_agr("constant")
      
    } else if(is(roads, "Raster")){
     # roads <- rasterToLineSegments(roads)
      roads <- raster::rasterToPoints(roads, fun = function(x){x > 0}, 
                                      spatial = TRUE) %>% 
        sf::st_as_sf() %>% 
        sf::st_set_agr("constant")
    }else {
      stop("roads must be either RasterLayer, sf object, or SpatialLines*",
           call. = FALSE)
    }
  }
  

  if(!(is(landings, "sf") || is(landings, "sfc"))){
    if(is(landings, "Spatial")){
      
      landings <- sf::st_as_sf(landings) %>% 
        sf::st_set_agr("constant")
      
    } else if(is(landings, "Raster")){
      if(is(landings, "RasterStack")|| is(landings, "RasterBrick")){
        stop("landings cannot be a RasterStack or Brick please supply",
             " a single RasterLayer", call. = FALSE)
      }
      # check if landings are clumps of cells (ie polygons) or single cells
      # (ie points) and if clumps take centroid
      clumpedRast <- raster::clump(landings, gaps = F) 
      
      clumps <- clumpedRast %>% 
        raster::freq(useNA = "no")
      
      clumps <- clumps[,2] %>% max() > 1
      
      if(clumps){
        landings <- sf::st_as_sf(raster::rasterToPolygons(clumpedRast, 
                                                          dissolve = TRUE)) %>% 
          sf::st_set_agr("constant")
      } else {
        landings <- sf::st_as_sf(raster::rasterToPoints(landings, 
                                                        fun = function(x){x > 0}, 
                                                        spatial = TRUE)) %>% 
          sf::st_set_agr("constant")
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
        sf::st_cast("POINT") %>% 
        sf::st_set_agr("constant")
    }else {
      stop("landings must be either RasterLayer, sf object, SpatialPoints*, ",
           "or SpatialPolygons*",
           call. = FALSE)
    }
  }
  
  if(sf::st_geometry_type(landings, by_geometry = FALSE) %in% 
     c("POLYGON", "MULTIPOLYGON")){
    # Use point on surface not centroid to ensure point is inside irregular polygons
    landings <- sf::st_point_on_surface(landings) %>% 
      sf::st_set_agr("constant")
  }
  
  # check crs error if different
  if(!all(raster::compareCRS(raster::crs(roads), raster::crs(landings)),
          raster::compareCRS(raster::crs(roads), raster::crs(cost)))){
    stop("the crs of roads, landings and cost must match", call. = FALSE)
  }
  
  # burn in roads to have 0 cost 
  if(!roadsInCost){
    message("Burning in roads to cost raster from sf")
    # use stars package because raster::rasterize and raster::mask are slow
    cost_st <- stars::st_as_stars(cost)
    
    # The crs is checked above but stars requires that they be identical
    if(!is.na(sf::st_crs(roads))){
      roads <- sf::st_transform(roads, sf::st_crs(cost_st))
    }
    
    # rasterize roads to template
    tmplt <- stars::st_as_stars(sf::st_bbox(cost_st), nx = raster::ncol(cost),
                                ny = raster::nrow(cost), values = 1)
    
    # road raster is 1 where there are no roads and 0 where there are roads
    roads_st <- stars::st_rasterize(roads %>% select(), template = tmplt,
                                    options = "ALL_TOUCHED=TRUE") == 1
   
    cost_st <- cost_st * roads_st
    
    # convert back to Raster from stars
    cost <- as(cost_st, "Raster")

    rm(cost_st, roads_st, tmplt)
  }
  
  # crop landings and roads to bbox of cost raster
  nrland <- nrow(landings)
  nrroads <- nrow(roads)
  
  ext <- sf::st_bbox(cost) %>% as.numeric() %>% 
    `names<-`(c("xmin", "ymin", "xmax", "ymax"))
  landings <- sf::st_crop(landings, ext)
  roads <- sf::st_crop(roads, ext)
  
  if(nrland != nrow(landings)){
    stop(nrland - nrow(landings), " landings are outside the cost surface. ",
         "The cost surface must cover the extent of landings", call. = FALSE)
  }
  
  if(nrroads != nrow(roads)){
    stop(nrroads - nrow(roads), " roads are outside the cost surface. ",
         "The cost surface must cover the extent of roads", call. = FALSE)
  }

  sim <- list(roads = roads, costSurface = cost, 
              roadMethod = roadMethod, 
              landings = landings)
}
