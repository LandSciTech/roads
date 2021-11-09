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


#' Get shortest path between points in list
#' 
#' @param sim sim list
#' @noRd
shortestPaths<- function(sim){
  # finds the least cost paths between a list of two points
  if(!length(sim$paths.list) == 0){
    #create a list of shortest paths
    paths <- unlist(lapply(sim$paths.list,
                           function(x){
                             igraph::get.shortest.paths(sim$g, x[1], 
                                                        x[2], out = "both") 
                           } ))
    
    # save the verticies for mapping
    sim$paths.v <- rbind(data.table::data.table(paths[grepl("vpath", names(paths))] ), 
                         sim$paths.v) 
    
    # get edges for updating graph
    paths.e <- paths[grepl("epath", names(paths))]
    
    rm(paths)
    
    # updates the cost(weight) associated with the edge that became a road
    igraph::edge_attr(sim$g, 
                      index = igraph::E(sim$g)[igraph::E(sim$g) %in% paths.e],
                      name = "weight") <- 0.00001 
    
    rm(paths.e)
    
    # make new roads
    new_roads <- pathsToLines(sim)
    # add new roads to existing
    sim$roads <- rbind(sim$roads, new_roads)
    
    # remove no longer needed parts of list that aren't be used for update
    sim$roads.close.XY <- NULL
    sim$paths.v <- NULL
    sim$paths.list <- NULL

  }
  return(invisible(sim))
}