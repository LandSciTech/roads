#' Set the graph which determines least cost paths
#' 
#' Creates a graph in inititation phase which can be updated and solved for paths


getGraph<- function(sim, neighbourhood){
  sim$paths.v <- NULL
  
  # prepare the cost surface raster #===========
  # get cost as data.table from raster
  weight <- data.table(weight = raster::getValues(sim$costSurface))
  
  # get the id for ther verticies which is used to merge with the edge list from
  # adj
  weight[, id := seq_len(.N)] 
  
  # get the adjacency using SpaDES function adj #=======
  # rooks case
  if(!is.element(neighbourhood, c("rook", "octagon", "queen"))) {
    stop("neighbourhood type not recognized")
  }
  
  nc <- ncol(sim$costSurface)
  ncel <- ncell(sim$costSurface)
  
  edges <- SpaDES.tools::adj(
      returnDT = TRUE,
      numCol = nc,
      numCell = ncel,
      directions = 4,
      cells = 1:as.integer(ncel)
    )
  
  # adj will return matrix even if returnDT = TRUE if small  
  edges <- data.table::data.table(edges)
  
  # switch to and from where to > from so can remove duplicates
  edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]
  
  edges <- unique(edges)
  
  # merge in the weights from a cost surface
  edges.w1 <- merge(x = edges, y = weight, by.x = "from", by.y = "id") 
  
  # reformat
  data.table::setnames(edges.w1, c("from", "to", "w1"))
  
  # merge in the weights to a cost surface
  edges.w2 <- data.table::setDT(merge(x = edges.w1, y = weight,
                                      by.x = "to", by.y = "id"))
  
  # reformat
  data.table::setnames(edges.w2, c("from", "to", "w1", "w2")) 
  
  # take the average cost between the two pixels
  edges.w2$weight <- (edges.w2$w1 + edges.w2$w2)/2 
  
  if(neighbourhood == "rook"){
    edges.weight = edges.w2
  } else {
    
    # bishop's case - multiply weights by 2^0.5
    if(neighbourhood == "octagon"){
      mW = 2^0.5
    } else {mW = 1}
    weight$weight = weight$weight*mW
    
    edges <- SpaDES.tools::adj(
      returnDT = TRUE,
      numCol = nc,
      numCell = ncel,
      directions = "bishop",
      cells = 1:as.integer(ncel)
    )
    
    edges <- data.table::data.table(edges)
    
    # edges[from < to, c("from", "to") := .(to, from)]
    edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]
    
    edges<-unique(edges)
    
    # merge in the weights from a cost surface
    edges.w1 <- merge(x = edges, y = weight, by.x = "from", by.y = "id") 
    
    # reformat
    data.table::setnames(edges.w1, c("from", "to", "w1")) 
    
    # merge in the weights to a cost surface
    edges.w3 <- data.table::setDT(merge(x = edges.w1, y = weight, 
                                        by.x = "to", by.y = "id"))
    
    # reformat
    data.table::setnames(edges.w3, c("from", "to", "w1", "w2")) 
    
    # take the average cost between the two pixels
    edges.w3$weight<-(edges.w3$w1 + edges.w3$w2)/2 
    
    # get the edges list #==================
    edges.weight = rbind(edges.w2, edges.w3)
  }
  
  # get rid of NAs caused by barriers. Drop the w1 and w2 costs.
  edges.weight <- edges.weight[stats::complete.cases(edges.weight), c(1:2, 5)] 
  
  # set the ids of the edge list. Faster than using as.integer(row.names())
  edges.weight[, id := seq_len(.N)] 
  
  # make the graph #================
  # create the graph using to and from columns. Requires a matrix input
  sim$g <- igraph::graph.edgelist(as.matrix(edges.weight)[,1:2], dir = FALSE)
  
  # assign weights to the graph. Requires a matrix input
  igraph::E(sim$g)$weight <- as.matrix(edges.weight)[,3]
  
  #------clean up
  rm(edges.w1, edges.w2, edges.w3, edges, weight, mW, nc, ncel, edges.weight)
  return(invisible(sim))
}