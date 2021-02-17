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
    remL = ip;remL[landC>0]=NA
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
