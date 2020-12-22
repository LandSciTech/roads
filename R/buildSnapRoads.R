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

buildSnapRoads <- function(sim){
  # union roads to one feature
  roads.pts <- summarise(sim$roads)
  
  # find nearest point between road feature and landings, returns a line between
  # the points
  snap_roads_lines <- sf::st_nearest_points(sim$landings, roads.pts)
  
  snap_roads_lines <- sf::st_sf(geometry = snap_roads_lines) 
  # snap_roads_lines <- mutate(snap_roads_lines, 
  #                            geometry = sf::st_cast(geometry, "MULTIPOINT"))
  
  # add to existing roads

  sim$roads <- rbind(sim$roads, snap_roads_lines)
  
  return(invisible(sim))
}
