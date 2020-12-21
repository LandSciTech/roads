#' Set the graph which determines least cost paths
#' 
#' Creates a graph in inititation phase which can be updated and solved for paths


getGraph<- function(sim, neighbourhood){
  sim$paths.v <- NULL
  
  # prepare the cost surface raster #===========
  # get the cost surface as a matrix using the raster package
  ras.matrix <- raster::as.matrix(sim$costSurface)
  
  # transpose then vectorize which matches the same order as adj
  weight <- c(t(ras.matrix))
  
  # convert to a data.table - faster for large objects than data.frame
  weight <- data.table::data.table(weight) 
  
  #Try
  # weight<-data.table(getValues(sim$costSurface))
  
  # get the id for ther verticies which is used to merge with the edge list from
  # adj
  weight[, id := seq_len(.N)] 
  
  # get the adjacency using SpaDES function adj #=======
  # rooks case
  if(!is.element(neighbourhood, c("rook", "octagon", "queen"))) {
    stop("neighbourhood type not recognized")
  }
  
  edges <- SpaDES.tools::adj(
      returnDT = TRUE,
      numCol = ncol(ras.matrix),
      numCell = ncol(ras.matrix) * nrow(ras.matrix),
      directions = 4,
      cells = 1:as.integer(ncol(ras.matrix) * nrow(ras.matrix))
    )
  
  edges <- data.table::data.table(edges)
  # edges[from < to, c("from", "to") := .(to, from)]
  edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]
  
  edges <- unique(edges)
  
  # merge in the weights from a cost surface
  edges.w1 <- merge(x = edges, y = weight, by.x = "from", by.y = "id") 
  
  # reformat
  data.table::setnames(edges.w1, c("from", "to", "w1"))
  
  # merge in the weights to a cost surface
  edges.w2<-data.table::setDT(merge(x = edges.w1, y = weight,
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
      numCol = ncol(ras.matrix),
      numCell = ncol(ras.matrix) * nrow(ras.matrix),
      directions = "bishop",
      cells = 1:as.integer(ncol(ras.matrix) * nrow(ras.matrix))
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
  rm(edges.w1,edges.w2,edges.w3, edges, weight, ras.matrix)
  return(invisible(sim))
}