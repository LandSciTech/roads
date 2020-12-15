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
    
    paths.e<-paths[grepl("epath",names(paths))]
    igraph::edge_attr(sim$g, index= igraph::E(sim$g)[igraph::E(sim$g) %in% paths.e], name= 'weight')<-0.00001 #changes the cost(weight) associated with the edge that became a path (or road)
    #reset landings and roads close to them
    sim$landings<-NULL
    sim$roads.close.XY<-NULL
    sim$newRoads.lines<-newRoadsToLines(sim)
    
    rm(paths.e)
    gc()
  }
  return(invisible(sim))
}