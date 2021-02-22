#' Get landing points inside harvest blocks
#'
#'

getLandingsFromHarvest <- function(harvest, numLandings = 1){
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
