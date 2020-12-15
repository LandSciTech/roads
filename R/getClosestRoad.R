#' Find the closest point on a road for each landing
#'
#' Find the closest point on the road to each landing and return as an x y
#' matrix


# Ideally use this one
getClosestRoad <- function(sim){
  # union roads to one feature
  roads.pts <- sf::st_union(sim$roads)
  
  # find nearest point between road feature and landings, returns a line between
  # the points
  closest.roads.pts <- sf::st_nearest_points(sim$landings, roads.pts)
  
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
