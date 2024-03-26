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


#' Make list of paths that are part of mst
#' 
#' slight update from roadClus version to use sf
#' 
#' @param sim sim list
#' @noRd

mstList<- function(sim){
  if(nrow(sim$landings) == 0){
    return(invisible(sim))
  }
  # get cell indexs for new road start and end
  mst.v <- rbind(terra::cellFromXY(sim$weightRaster, 
                                              sf::st_coordinates(sim$landings)),
                           terra::cellFromXY(sim$weightRaster,
                                              sim$roads.close.XY))
  mst.v <- as.vector(mst.v)
  paths.matrix <- unique(mst.v) 


  if(length(paths.matrix) > 1){
    # get an adjaceny matrix given the cell numbers
    mst.adj <- igraph::distances(sim$g, paths.matrix, paths.matrix) 
    
    # set the verticies names as the cell numbers in the weightRaster
    rownames(mst.adj) <- paths.matrix
    
    # set the verticies names as the cell numbers in the weightRaster
    colnames(mst.adj) <- paths.matrix 
    
    # create a graph
    mst.g <- igraph::graph_from_adjacency_matrix(mst.adj, weighted=TRUE)
    
    # get the the minimum spanning tree
    mst.paths <- igraph::mst(mst.g, weighted=TRUE) 
    
    # get raster indexs for mst vertices
    paths.matrix <- igraph::as_edgelist(mst.paths, names=TRUE)
    
    paths.matrix <- matrix(as.numeric(paths.matrix), ncol = 2)

    # put the edge combinations in a list used for shortestPaths
    sim$paths.list <- split(paths.matrix, 1:nrow(paths.matrix)) 
    
    rm(mst.paths,mst.g, mst.adj, mst.v, paths.matrix)
  }
  return(invisible(sim))
}
