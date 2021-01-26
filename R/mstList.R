#' 
#' 
#' slight update from roadClus version to use sf

mstList<- function(sim){
  # get cell indexs for new road start and end
  mst.v <- rbind(raster::cellFromXY(sim$costSurface, 
                                              sf::st_coordinates(sim$landings)),
                           raster::cellFromXY(sim$costSurface,
                                              sim$roads.close.XY))
  mst.v <- as.vector(mst.v)
  paths.matrix <- unique(mst.v) 


  if(length(paths.matrix) > 1){
    # get an adjaceny matrix given the cell numbers
    mst.adj <- igraph::distances(sim$g, paths.matrix, paths.matrix) 
    
    # set the verticies names as the cell numbers in the costSurface
    rownames(mst.adj) <- paths.matrix
    
    # set the verticies names as the cell numbers in the costSurface
    colnames(mst.adj) <- paths.matrix 
    
    # create a graph
    mst.g <- igraph::graph_from_adjacency_matrix(mst.adj, weighted=TRUE)
    
    # get the the minimum spanning tree
    mst.paths <- igraph::mst(mst.g, weighted=TRUE) 
    
    # get raster indexs for mst vertices
    paths.matrix <- igraph::get.edgelist(mst.paths, names=TRUE)
    
    paths.matrix <- matrix(as.numeric(paths.matrix), ncol = 2)

    # put the edge combinations in a list used for shortestPaths
    sim$paths.list<-split(paths.matrix, 1:nrow(paths.matrix)) 
    
    rm(mst.paths,mst.g, mst.adj, mst.v, paths.matrix)
  }
  return(invisible(sim))
}
