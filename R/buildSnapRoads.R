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


#' Build roads with the snap method
#'
#' Build roads that simply connect each landing to each road "as the crow
#' flies."
#'
#' @param sim a sim list
#'
#' @return sim list
#' @noRd

buildSnapRoads <- function(sim, roadsOut){
  # union roads to one feature
  roads.pts <- summarise(sim$roads)
  
  # find nearest point between road feature and landings, returns a line between
  # the points
  snap_roads_lines <- sf::st_nearest_points(sim$landings, roads.pts)
  
  snap_roads_lines <- sf::st_sf(geometry = snap_roads_lines) 
  # snap_roads_lines <- mutate(snap_roads_lines, 
  #                            geometry = sf::st_cast(geometry, "MULTIPOINT"))
  
  # add to existing roads
  sim$roads <- bind_rows(sim$roads, snap_roads_lines)
  
  # burn into cost as 0
  sim$cost <- burnRoadsInCost(sim$roads, sim$costSurface)
  
  if(roadsOut == "raster"){
    sim$roads <- sim$cost == 0
  }
  
  return(invisible(sim))
}
