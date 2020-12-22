lcpList<- function(sim){
  ##Get a list of of cell indexs for to and from points
  paths.matrix <- cbind(raster::cellFromXY(sim$costSurface,
                                         sf::st_coordinates(sim$landings)), 
                      raster::cellFromXY(sim$costSurface, sim$roads.close.XY))
  
  sim$paths.list <- split(paths.matrix, 1:nrow(paths.matrix))
  
  rm(paths.matrix)
  
  return(invisible(sim))
}