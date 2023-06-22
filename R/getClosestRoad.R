# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#NOTICE: This function has been modified from https://github.com/bcgov/clus/blob/master/R/SpaDES-modules/roadCLUS/roadCLUS.R


#' Find the closest point on a road for each landing
#'
#' Find the closest point on the road to each landing and return as an x y
#' matrix
#' 
#' @param sim sim list
#' @noRd


# Ideally use this one
getClosestRoad <- function(sim, ordering = "closest"){
  # union roads to one feature
  roads.pts <- sf::st_union(sim$roads)
  
  # if roads contains both roads and lines has length 2 need only one so convert
  # lines to points at raster centers
  if(sf::st_geometry_type(roads.pts, by_geometry = FALSE) == "GEOMETRY"){
    rd.lines <- sf::st_collection_extract(roads.pts, type = "LINESTRING")
    rd.lines.pts <- terra::extract(sim$costSurface, terra::vect(rd.lines), xy = TRUE) %>% 
      sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(rd.lines)) %>% 
      sf::st_geometry()
    
    roads.pts <- c(rd.lines.pts, st_collection_extract(roads.pts, type = "POINT")) %>% 
      sf::st_union()
  }
  
  # find nearest point between road feature and landings, returns a line between
  # the points
  closest.roads.pts <- sf::st_nearest_points(sim$landings, roads.pts)
  
  distToRoad <- units::set_units(sf::st_length(closest.roads.pts), NULL)
  
  if(ordering == "closest"){
    closest.roads.pts <- closest.roads.pts[order(distToRoad)]
    sim$landings <- sim$landings[order(distToRoad),]
    distToRoad <- distToRoad[order(distToRoad)]
  }
  
  # find landings that are within the space of one raster cell from the road
  touching_road <- which(distToRoad < 
                           terra::res(sim$costSurface)[1])
  
  if(length(touching_road) > 0){
    # make snap roads for these ones 
    # snap_roads_lines <- sf::st_sf(geometry = closest.roads.pts[touching_road]) 
    # sim$roads <- rbind(sim$roads, snap_roads_lines)
    
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
