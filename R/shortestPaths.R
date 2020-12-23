#' Get shortest path between points in list
#' 
#' 
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
    
    # add new roads to existing
    sim$roads <- rbind(sim$roads, pathsToLines(sim))
    
    # remove no longer needed parts of list that aren't be used for update
    sim$roads.close.XY <- NULL
    sim$paths.v <- NULL
    sim$paths.list <- NULL

  }
  return(invisible(sim))
}