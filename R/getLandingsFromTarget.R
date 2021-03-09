#' Get landing points inside harvest blocks
#'
#'
#' @param harvest harvest blocks
#' @param numLandings number of landings per unit area of harvest block. This
#'   should be in the same units as the crs of the harvest


getLandingsFromHarvest <- function(harvest, numLandings = 1){
  if(!(is(harvest, "sf") || is(harvest, "sfc"))){
    if(is(harvest, "Spatial")){

      harvest <- sf::st_as_sf(harvest)

    } else if(is(harvest, "Raster")){
      # check if harvest are clumps of cells (ie polygons) or single cells
      # (ie points) and if clumps take centroid
      clumpedRast <- raster::clump(harvest, gaps = F)

      clumps <- clumpedRast %>%
        raster::freq(useNA = "no") %>%
        .[,2] %>% max() > 1

      if(clumps){
        harvest <- sf::st_as_sf(raster::rasterToPolygons(clumpedRast,
                                                          dissolve = TRUE))
      } else {
        landings <- sf::st_as_sf(raster::rasterToPoints(harvest,
                                                        fun = function(x){x > 0},
                                                        spatial = TRUE))
      }


    } else if(is(harvest, "matrix")){
      landings <- sf::st_sf(
        geometry = sf::st_as_sfc(list(sf::st_multipoint(harvest[, c("x", "y")])))
      ) %>%
        sf::st_cast("POINT")
    }
  }

  if(sf::st_geometry_type(harvest, by_geometry = FALSE) %in%
     c("POLYGON", "MULTIPOLYGON")){
    if(numLandings == 1){
      # Use point on surface not centroid to ensure point is inside irregular polygons
      landings <- sf::st_point_on_surface(harvest)
    } else {
      landings <- harvest %>% mutate(id = 1:n()) %>% 
        group_by(id) %>% 
        mutate(size = ceiling(numLandings * sf::st_area(geometry)),
               lands = sf::st_sample(geometry, type = "hexagonal", 
                                     size = size) %>%
                 list()) 
      landings2 <- purrr::pmap(list(lands = landings$lands,
                                    size = landings$size,
                                    geo = sf::st_geometry(landings)),
                               ~case_when(length(..1) == 0 ~ 
                                            list(sf::st_point_on_surface(..3)),
                                          length(..1) <= ..2 ~ list(..1),
                                          length(..1) > ..2 ~ 
                                            list(..1[sample(1:length(..1),
                                                            ifelse(..2 > length(..1), length(..1), ..2))]),
                                          TRUE ~ list(..1))) %>% 
        {do.call(c, .)} %>% 
        {do.call(c, .)}
    }

  }
}

#' Select random landing locations within patches.
#'
#' @param inputPatches A RasterLayer. Harvested patches should have values equal
#'   to a unique identifier
#' @param numLandings Numeric. A vector of the number of points to randomly
#'   sample inside harvested patches
#' @param omitCentroidsOutOfPolygons Logical. Default is FALSE in which case
#'   some points may be outside the borders of the harvested patch
#'
#' @export
getLandingsFromTarget<-function(inputPatches,
                                numLandings,
                                omitCentroidsOutOfPolygons=F){
  # Function to select a specific number of landings withing patches. Landing set
  # will include centroids, and additional randomly selected sample points if
  # numLandings>numCentroids.

  # inputPatches=newBlocks[[cm]]

  inputPatches[inputPatches==0]=NA

  landPts = matrix(0,0,3)
  colnames(landPts)=c("x","y","layer")
  for(i in 1:length(numLandings)){
    #i= 1
    nl = numLandings[[i]]
    ip = inputPatches==i
    ip[ip==0]=NA
    if(nl >= raster::cellStats(ip,"sum")){
      landPts= rbind(landPts,raster::rasterToPoints(ip,fun=function(landings){landings>0}))
      next
    }

    landC = getCentroids(ip,withIDs=T) #note centroids are not always in polygons
    if (omitCentroidsOutOfPolygons){
      landC[is.na(ip)]=NA
    }
    remL = ip
    remL[landC>0]=NA
    numSamples = nl-raster::cellStats(landC>0,"sum")#select additional points so total number is equal to small alternative

    landC = raster::rasterToPoints(landC,fun=function(landings){landings>0})
    #split into smaller patches to ensure adequate road density
    #sampleProp = 1/100
    #numSamples = round(cellStats(anthDist,"sum")*sampleProp)

    landPts = rbind(landPts,landC)

    if(numSamples<=0){
      next
    }

    landingPts = raster::sampleStratified(remL, size=numSamples,xy=T)
    landingPts=landingPts[,2:4]
    #add centroids to ensure all patches are included
    landPts = rbind(landPts,landingPts)

  }

  # make sf object
  landPts <- as.data.frame(landPts) %>%
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(inputPatches))

  return(landPts)
}
