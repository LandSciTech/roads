# Copyright © Her Majesty the Queen in Right of Canada as represented by the
# Minister of the Environment 2021/© Sa Majesté la Reine du chef du Canada
# représentée par le ministre de l'Environnement 2021.
# 
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
# 
#       http://www.apache.org/licenses/LICENSE-2.0
# 
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.

#' Build sim list object
#'
#' Build a sim list object to be used in the rest of the functions and returned
#' by `projectRoads`. It will convert other types of spatial objects to sf
#' or terra objects.
#' 
#' @param roads roads input
#' @param weightRaster weightRaster input
#' @param roadMethod method of road projection
#' @param landings landings input
#' @param roadsInWeight Whether the roads have already been burned into the `weightRaster`.
#' @param sim A sim list to update rather than building new.
#' @noRd

buildSimList <- function(roads, weightRaster, roadMethod, landings, roadsInWeight, 
                         sim = NULL){
  if(!is(weightRaster, "SpatRaster")){
    if(is(weightRaster, "RasterLayer")){
      weightRaster <- terra::rast(weightRaster)
    } else {
      stop("weightRaster must be provided as a SpatRaster or RasterLayer", call. = FALSE)
    }
  } 
  if(terra::minmax(weightRaster)[1] == 0 || 0 %in% terra::unique(weightRaster)[[1]]){
    message("0s detected in weightRaster raster, these will be considered as existing roads")
  } else if(roadsInWeight){
    warning("No 0s detected in weightRaster. If existing roads have not been ",
            "included in the weightRaster set roadsInWeight = FALSE to have them ",
            "burned in", call. = FALSE)
  }
  
  if(is(roads, "Raster")){
    roads <- terra::rast(roads)
  }
  
  # Burn roads into raster if not already for raster roads before converting 
  if(!roadsInWeight && is(roads, "SpatRaster")){
    message("Burning in roads to weightRaster raster from road raster")
    weightRaster <- weightRaster * (roads == 0)
    roadsInWeight <- TRUE
  }
  
  if(!(is(roads, "sf") || is(roads, "sfc"))){
    if(is(roads, "Spatial")){
      roads <- sf::st_as_sf(roads) %>% 
        sf::st_set_agr("constant")
      
    } else if(is(roads, "SpatRaster")){
     # roads <- rasterToLineSegments(roads)
      roads <- terra::subst(roads, from = 0, to = NA) %>% 
        terra::as.points() %>% 
        sf::st_as_sf() %>% 
        sf::st_set_agr("constant") 
    }else {
      stop("roads must be either SpatRaster, RasterLayer, sf object, or SpatialLines*",
           call. = FALSE)
    }
  }
  

  if(!(is(landings, "sf") || is(landings, "sfc"))){
    if(is(landings, "Spatial")){
      
      landings <- sf::st_as_sf(landings) %>% 
        sf::st_set_agr("constant")
      
    } else if(is(landings, "Raster") || is(landings, "SpatRaster")){
      if(is(landings, "Raster")){
        landings <- terra::rast(landings)
      }
      
      if(terra::nlyr(landings) > 1){
        stop("landings should be a single layer SpatRaster")
      }
      
      # check if landings are clumps of cells (ie polygons) or single cells
      # (ie points) and if clumps take centroid
      clumpedRast <- terra::patches(landings, allowGaps = FALSE, zeroAsNA = TRUE) 
      
      clumps <- clumpedRast %>% 
        terra::freq()
      
      clumps <- clumps[,2] %>% max() > 1
      
      if(clumps){
        landings <- sf::st_as_sf(terra::as.polygons(clumpedRast, 
                                                          dissolve = TRUE)) %>% 
          sf::st_set_agr("constant")
      } else {
        landings <- terra::subst(landings, from = 0, to = NA)%>% 
          terra::as.points() %>% 
          sf::st_as_sf() %>% 
          sf::st_set_agr("constant")
      }
      

    } else if(is(landings, "matrix")){
      xyind <- which(colnames(landings) %in% c("x", "X", "y", "Y"))
      if(length(xyind) == 0){
        stop("landings matrix must have column names in c('x', 'X', 'y', 'Y')",
             call. = FALSE)
      }
      landings <- sf::st_sf(
        geometry = sf::st_as_sfc(list(sf::st_multipoint(landings[, xyind])))
        ) %>%
        sf::st_cast("POINT") %>% 
        sf::st_set_agr("constant") %>% 
        sf::st_set_crs(sf::st_crs(weightRaster))
      
      message("CRS of landings supplied as a matrix is assumed to match the weightRaster")
    }else {
      stop("landings must be either SpatRaster, RasterLayer, sf object, SpatialPoints*, ",
           "or SpatialPolygons*",
           call. = FALSE)
    }
  }
  
  if(sf::st_geometry_type(landings, by_geometry = FALSE) %in% 
     c("POLYGON", "MULTIPOLYGON")){
    # Use point on surface not centroid to ensure point is inside irregular polygons
    landings <- sf::st_point_on_surface(sf::st_set_agr(landings, "constant")) %>% 
      sf::st_set_agr("constant")
  }
  
  if(nrow(landings) == 0){
    stop("landings is empty, at least one landing must be provided", call. = FALSE)
  }
  
  # check crs error if different
  if(!all(sf::st_crs(roads) == sf::st_crs(landings),
          sf::st_crs(roads) == sf::st_crs(weightRaster))){
    stop("the crs of roads, landings and weightRaster must match", call. = FALSE)
  }
  
  # burn in roads to have 0 weight 
  if(!roadsInWeight){
    message("Burning in roads to weightRaster from sf")
    
    weightRaster <- burnRoadsInWeight(roads, weightRaster)
    
  }
  
  # crop landings and roads to bbox of weightRaster raster
  nrland <- nrow(landings)
  nrroads <- nrow(roads)
  
  if(nrroads == 0){
    stop("nrow(roads) is 0. Please supply at least one existing road", call. = FALSE)
  }
  
  ext <- sf::st_bbox(weightRaster) %>% as.numeric() %>% 
    `names<-`(c("xmin", "ymin", "xmax", "ymax"))
  landings <- sf::st_crop(sf::st_set_agr(landings, "constant"), ext) %>% sf::st_set_agr("constant")
  roads <- sf::st_crop(sf::st_set_agr(roads, "constant"), ext) %>% sf::st_set_agr("constant")
  
  if(nrland != nrow(landings)){
    stop(nrland - nrow(landings), " landings are outside the weightRaster. ",
         "The weightRaster must cover the extent of landings", call. = FALSE)
  }
  
  if(nrroads != nrow(roads)){
    stop(nrroads - nrow(roads), " roads are outside the weightRaster. ",
         "The weightRaster must cover the extent of roads", call. = FALSE)
  }

  if(is.null(sim)){
    sim <- rlang::env(roads = roads, weightRaster = weightRaster, 
                roadMethod = roadMethod, 
                landings = landings)
  } else {
    sim$roads <- roads
    sim$landings <- landings
    sim$weightRaster <- weightRaster
    sim$roadMethod <- roadMethod
  }
  return(sim)

}



#' Burn roads in to weightRaster
#'
#' Use sf roads object to convert weightRaster to 0 where roads already exist. This is
#' an internal function and does not contain many checks. Use at own risk.
#'
#' @param roads sf object with road lines
#' @param weightRaster SpatRaster with weights
#'
#' @return a SpatRaster with 0 for roads.
#' 
#' @noRd
burnRoadsInWeight <- function(roads, weightRaster){
  # The crs is checked above but stars requires that they be identical
  if(!is.na(sf::st_crs(roads))){
    roads <- sf::st_transform(roads, sf::st_crs(weightRaster))
  }
  
  if(any(grepl("POINT", sf::st_geometry_type(roads, by_geometry = TRUE))) &&
     any(grepl("LINESTRING", sf::st_geometry_type(roads, by_geometry = TRUE)))){
    geom_types <- c("POINT", "LINESTRING")
    
    rasts <- lapply(geom_types, function(x, rds, wt){
      geom_roads <- sf::st_collection_extract(rds, type = x)
      geom_rast <- terra::rasterize(terra::vect(geom_roads), wt,
                                    background = 0) > 0
    }, rds = roads, wt = weightRaster)
    
    roadsRast <- !(rasts[[1]]|rasts[[2]])
  } else {
    roadsRast <- terra::rasterize(terra::vect(roads), weightRaster, touches = TRUE, background = 0) == 0 
  }
  
  weightRaster <- weightRaster * roadsRast
}
