#' Get landing points inside harvest blocks
#'
#' Generate landing points inside polygons representing harvested area. There
#' are three different sampling types available: "centroid" is the default and
#' will return the centroid or a point that is inside the polygon if the
#' centroid is not (see \code{\link[sf:geos_unary]{st_point_on_surface}}); "random" takes a
#' random sample based on the given \code{landingDens} see
#' (\code{\link[sf]{st_sample}}); "regular" intersects the polygons with a
#' regular grid with cellsize \code{sqrt(1/landingDens)}, if a polygon does not
#' intersect with the grid its centroid is used.
#'
#' Note that the \code{landingDens} is in points per unit area where the unit of
#' area is determined by the CRS. For projected CRS this should likely be a very
#' small number ie < 0.001.
#'
#' @param harvest sf, SpatialPolygons or RasterLayer object with harvested
#'   areas. If it is a RasterLayer with more than one unique value other than 0
#'   each value will be run separately which will produce different results from
#'   a 0/1 raster but will be much slower.
#' @param landingDens number of landings per unit area. This should be in the
#'   same units as the crs of the harvest. Note that 0.001 pts per m2 is > 1000
#'   pts per km2 so this number is usually very small for projected crs.
#' @param sampleType character. "centroid" (default), "regular" or "random".
#'   Centroid returns one landing per harvest block, which is guaranteed to be
#'   in the harvest block for sf objects but not for rasters. Regular returns
#'   points from a grid with density \code{landingDens} that overlap the
#'   harvested areas. Random returns a random set of points from each polygon
#'   where the number is determined by the area of the polygons and
#'   \code{landingDens}. If \code{harvest} is a raster the centroid is always
#'   returned as one of the landings to ensure all harvest areas get at least
#'   one landing.
#'   
#' @return an sf simple feature collection with an ID column and POINT geometry
#'
#' @examples
#' # Get centroid
#' outCent <- getLandingsFromTarget(demoScen[[1]]$landings.poly)
#' raster::plot(demoScen[[1]]$landings.poly)
#' plot(outCent, col = "red", add = TRUE)
#'
#' # Get random sample with density 0.1 pts per unit area
#' outRand <- getLandingsFromTarget(demoScen[[1]]$landings.poly, 0.1, sampleType = "random")
#'
#' raster::plot(demoScen[[1]]$landings.poly)
#' plot(outRand, col = "red", add = TRUE)
#'
#' # Get regular sample with density 0.1 pts per unit area
#' outReg <- getLandingsFromTarget(demoScen[[1]]$landings.poly, 0.1, sampleType = "regular")
#'
#' raster::plot(demoScen[[1]]$landings.poly)
#' plot(outReg, col = "red", add = TRUE)
#'
#' @export
getLandingsFromTarget <- function(harvest, 
                                  landingDens = NULL,
                                  sampleType = "centroid"){
  if(!sampleType %in% c("regular", "centroid", "random")){
    stop("sampleType must be one of ", c("regular", "centroid", "random"))
  }
  
  if(sampleType != "centroid" && is.null(landingDens)){
    stop("landingDens must be supplied unless sampleType = 'centroid'")
  }
  if(sampleType == "centroid" && !is.null(landingDens)){
    warning("sampleType is 'centroid' so landingDens will be ignored")
  }
  
  if(!is.numeric(landingDens) && !is.null(landingDens)){
    stop("landingDens must be numeric not", class(landingDens))
  }
  if(!is.na(sf::st_crs(harvest))){
    if(!sf::st_is_longlat(harvest) && 
       !is.null(landingDens) && 
       landingDens > 0.001){
      message("you have asked for > 0.001 pts per m2",
              " which is > 1000 pts per km2 and may take a long time")
    }
  }
  
  if(!(is(harvest, "sf") || is(harvest, "sfc"))){
    if(is(harvest, "Spatial")){

      harvest <- sf::st_as_sf(harvest) %>% sf::st_set_agr("constant")

    } else if(is(harvest, "Raster")){

      # check if harvest are clumps of cells (ie polygons) or single cells
      # (ie points) and if clumps take centroid
      clumpedRast <- raster::clump(harvest, gaps = F)
      
      clumps <- clumpedRast %>%
        raster::freq(useNA = "no") 
      clumps <- max(clumps[,2])  > 1
      
      if(clumps){
        landings <- getLandingsFromTargetRast(harvest, landingDens, sampleType)
      } else {
        landings <- sf::st_as_sf(raster::rasterToPoints(harvest,
                                                        fun = function(x){x > 0},
                                                        spatial = TRUE))
        if(sampleType != "centroid"){
          warning("raster has only single cell havest blocks so",
                  " landingDens is ignored and cells > 0 converted to points",
                  call. = FALSE)
        }
        
      }
      return(landings)
    } 
  }

  if(sf::st_geometry_type(harvest, by_geometry = FALSE) %in%
     c("POLYGON", "MULTIPOLYGON")){
    if(sampleType == "centroid"){
      # Use point on surface not centroid to ensure point is inside irregular
      # polygons
      landings <- sf::st_point_on_surface(harvest)
    } else if (sampleType == "regular"){
      harvest <- mutate(harvest, ID = 1:n()) %>% sf::st_set_agr("constant")
      
      grd <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(harvest)),
                              cellsize = sqrt(1/landingDens), what = "corners")
      
      inter <- sf::st_intersection(harvest, grd)
      
      notInter <- anti_join(harvest, sf::st_drop_geometry(inter), by = "ID") %>% 
        sf::st_set_agr("constant") %>% 
        sf::st_point_on_surface()
      
      landings <- rbind(inter, notInter)
      
    } else if (sampleType == "random"){
      
      landings <- harvest %>% mutate(id = 1:n()) %>% 
        mutate(size = round(landingDens * sf::st_area(.data$geometry)) %>%
                 units::set_units(NULL)) %>% 
        mutate(size = ifelse(.data$size == 0, 1, .data$size))
      
      landings1 <- filter(landings, .data$size == 1) %>% 
        mutate(lands = sf::st_point_on_surface(.data$geometry))
      
      if(nrow(landings1) < nrow(landings)){
        landings2Plus <- filter(landings, .data$size > 1)
        landings2Plus <- sf::st_sample(landings2Plus$geometry, type = "random",
                                       size = landings2Plus$size)
        
        landings <- c(landings1$lands, landings2Plus)
      } else {
      
        landings <- landings1$lands
      }
      sf::st_sf(landings)
    }

  }
}


#' Select random landing locations within patches.
#'
#' @param inputPatches A RasterLayer. Harvested patches should have values
#'   greater than 0
#' @param landingDens number of landings per unit area. This should be in the
#'   same units as the crs of the harvest. Note that 0.001 pts per m2 is > 1000
#'   pts per km2 so this number is usually very small for projected crs.
#' @param sampleType character. "centroid" (default), "regular" or "random".
#'   Centroid returns one landing per harvest block, which is not guaranteed to
#'   be in the harvest block. Regular returns points from a grid with density
#'   \code{landingDens} that overlap the harvested areas. Random returns a
#'   random set of points from each polygon where the number is determined by
#'   the area of the polygons and \code{landingDens}. The centroid is always
#'   returned as one of the landings to ensure all harvest areas get at least
#'   one landing.
#' @param omitCentroidsOutOfPolygons Logical. Default is FALSE in which case
#'   some points may be outside the borders of the harvested patch
#'   
getLandingsFromTargetRast<-function(inputPatches,
                                    landingDens,
                                    sampleType = "regular",
                                    omitCentroidsOutOfPolygons = F){
  # Function to select a specific number of landings withing patches. Landing set
  # will include centroids, and additional randomly selected sample points if
  # landingDens>numCentroids.
  
  # inputPatches=newBlocks[[cm]]
  
  if(length(landingDens) > 1){
    stop("landingDens should have length 1 not ", length(landingDens))
  }
  if(!sampleType %in% c("regular", "centroid", "random")){
    stop("sampleType must be one of ", c("regular", "centroid", "random"))
  }
  
  inputPatches[inputPatches == 0] <- NA
  
  landPts = matrix(0,0,3)
  colnames(landPts)=c("x","y","layer")
  for(i in raster::unique(inputPatches)){
    
    nl <- ifelse(is.null(landingDens), 0, landingDens)
    
    ip <- inputPatches == i
    
    ip[ip == 0] <- NA
    
    landC = getCentroids(ip, withIDs = T) #note centroids are not always in polygons
    if (omitCentroidsOutOfPolygons) {
      landC[is.na(ip)] <- NA
    }
    remL <- ip
    remL[landC > 0] <- NA
    
    landC <- raster::rasterToPoints(landC, fun = function(x){x > 0})
    
    landPts <- rbind(landPts, landC)
    
    if(sampleType == "centroid"){
      next
    }
    
    if(sampleType == "random"){
      #select additional points so total number is equal to small alternative
      #numSamples = nl - raster::cellStats(landC > 0, "sum")
      
      numSamples <- round(raster::cellStats(remL, "sum") * 
                            landingDens * 
                            prod(raster::res(remL)))
      if(numSamples < 1){
        # use just the centroids
        next
      }
      
      if(numSamples >= raster::cellStats(remL, "sum")){
        
        # If more samples than cells return 1 pt per cell and ignore centroid
        landingPts <- raster::rasterToPoints(ip,fun=function(landings){landings>0})
        landPts <- landingPts
        colnames(landPts)=c("x","y","layer")
        
      } else {
        
        landingPts <- raster::sampleStratified(remL, size = numSamples, xy = TRUE)
        landingPts <-  landingPts[, 2:4]
      }
    }
    if(sampleType == "regular"){
      extArea <- prod(raster::res(remL))*raster::ncell(remL)
      nPts <- landingDens * extArea
      
      landingPts <- raster::sampleRegular(remL, size = nPts, xy = TRUE)
      landingPts <- dplyr::filter(as.data.frame(landingPts), !is.na(.data$layer))
    }
    
    #add centroids to ensure all patches are included
    landPts = rbind(landPts,landingPts)
  }
  # make sf object
  landPts <- as.data.frame(landPts) %>%
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(inputPatches))

  return(landPts)
}

#' Get centroids from raster landings
#' 
#' @param newLandings raster landings
#'
#' @param withIDs logical
#'
getCentroids<-function(newLandings, withIDs = TRUE){
  cRes = raster::res(newLandings)
  
  p = raster::as.data.frame(raster::clump(newLandings, gaps = F), xy = TRUE)
  p = p[!is.na(p$clumps), ]
  
  pointLocs = p %>% dplyr::group_by(.data$clumps) %>%
    dplyr::summarize(x = mean(.data$x), y = mean(.data$y))
  
  pointLocs = as.data.frame(subset(pointLocs, select = c('x', 'y', 'clumps')))
  
  newLandingCentroids = newLandings
  newLandingCentroids[!is.na(newLandingCentroids)] = NA
  
  cells = raster::cellFromXY(newLandingCentroids, pointLocs[, 1:2])
  
  if (withIDs) {
    newLandingCentroids[cells] = pointLocs$clumps
  } else{
    newLandingCentroids[cells] = 1
  }
  return(newLandingCentroids)
}