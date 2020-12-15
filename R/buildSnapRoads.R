#' Build roads with the snap method
#'
#' Build roads that simply connect each landing to each road "as the crow
#' flies."
#'
#' @param sim
#'
#' @return sim list
#'
#' @examples
roadCLUS.buildSnapRoads <- function(sim){
  if (is(sim$landings,'SpatialPoints')){
    landings <- sim$landings@coords
  }else if (is(sim$landings,'RasterLayer')){
    landings <- raster::rasterToPoints(sim$landings>0,fun=function(x)x>0)[,c('x','y')]
  }else{
    landings <- sim$landings
  }
  lineslist <- lapply(1:nrow(landings),function(i){
    sp::Lines(sp::Line(rbind(landings[i,c('x','y')],sim$roads.close.XY[i,c('x','y')])),ID=i)
  })
  sim$newRoads.lines <- sp::SpatialLines(lineslist,proj4string=sp::CRS(as.character(sim$costSurface@crs)))
  newRoads.cells <- do.call(rbind,raster::extract(sim$costSurface,sim$newRoads.lines,cellnumbers=TRUE))
  sim$roads[newRoads.cells[,1]] <- 1
  return(invisible(sim))
}

origOut <- roadCLUS.getClosestRoad(list(roads = demoScen[[1]]$road.rast, 
                                        cost = demoScen[[1]]$cost.rast,
                                        landings = demoScen[[1]]$landings.points)) %>% 
  roadCLUS.buildSnapRoads()

buildSnapRoads <- function(sim){
  # union roads to one feature
  roads.pts <- sf::st_union(sim$roads)
  
  # find nearest point between road feature and landings, returns a line between
  # the points
  closest.roads.pts <- sf::st_nearest_points(sim$landings, roads.pts)
  
  # add to existing roads
  sim$roads
  
  return(invisible(sim))
}