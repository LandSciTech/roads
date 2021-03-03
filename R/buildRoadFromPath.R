#' build road based on paths
#' 
#' @param sim a sim list
#' 
#' Changed from roadCLUS.analysis


buildRoadFromPath <- function(sim){
  if(!is.element('roadMethod',names(sim))||!(sim$roadMethod == 'snap')){
    ras.out<-sim$costSurface
    ras.out[]<-1:raster::ncell(ras.out)
    ras.out[!(ras.out[] %in% as.matrix(sim$paths.v))] <- NA
    #ras.out<-raster::reclassify(ras.out, c(0.000000000001, maxValue(ras.out),0))
    if(is.element('roads',names(sim))){
      sim$roads<-raster::merge(ras.out, sim$roads)
    }else{
      sim$roads = ras.out
    }
  }
  return(invisible(sim))
}