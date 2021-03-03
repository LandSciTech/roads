#' Find the closest point on a road for each landing
#'
#' Find the closest point on the road to each landing and return as an x y
#' matrix
#' 
#' @param sim sim list


# Ideally use this one
getClosestRoad <- function(sim){
  # union roads to one feature
  roads.pts <- sf::st_union(sim$roads)
  
  # find nearest point between road feature and landings, returns a line between
  # the points
  closest.roads.pts <- sf::st_nearest_points(sim$landings, roads.pts)

  # find landings that are within the space of one raster cell from the road
  touching_road <- which(units::set_units(sf::st_length(closest.roads.pts), NULL) < 
                           raster::res(sim$costSurface)[1])
  
  if(length(touching_road) > 0){
    # make snap roads for these ones 
    snap_roads_lines <- sf::st_sf(geometry = closest.roads.pts[touching_road]) 
    sim$roads <- rbind(sim$roads, snap_roads_lines)
    
    # Remove touching roads pts from rest of roads and from landings set 
    # the original landings will be replaced at the end
    closest.roads.pts <- closest.roads.pts[-touching_road]
    sim$landings <- slice(sim$landings, -touching_road)
  }
  
  if(length(closest.roads.pts) == 0){
    sim$roads.close.XY = closest.roads.pts
    return(sim)
  }

  # convert lines to points
  closest.roads.pts <- sf::st_cast(closest.roads.pts, "POINT")
  
  # get every second point which is the ones on the road
  closest.roads.pts <- closest.roads.pts[seq(2, length(closest.roads.pts), 2)]
  
  # assign coord matrix
  sim$roads.close.XY <-  sf::st_coordinates(closest.roads.pts)
  colnames(sim$roads.close.XY) <- c("x", "y")
  
  rm(roads.pts, closest.roads.pts)
  
  return(invisible(sim))
}
