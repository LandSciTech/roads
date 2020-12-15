# Sarah experimenting
library(roads)
library(dplyr)

CLUSexample
# Note call to gc that was in function caused 3 sec of wasted time!! when rest
# of funciton took 20ms

roadCLUS.getClosestRoad <- function(sim){
  roads.pts <- raster::rasterToPoints(sim$roads, fun=function(x){x > 0})
  closest.roads.pts <- apply(rgeos::gDistance(sp::SpatialPoints(roads.pts),sp::SpatialPoints(sim$landings), byid=TRUE), 1, which.min)
  sim$roads.close.XY <- as.matrix(roads.pts[closest.roads.pts, 1:2,drop=F]) #this function returns a matrix of x, y coordinates corresponding to the closest road
  #The drop =F is needed for a single landing - during the subset of a matrix it will become a column vector because as it converts a vector to a matrix, r will assume you have one column
  rm(roads.pts, closest.roads.pts)
  return(invisible(sim))
}

roadCLUS.getClosestRoad.sf <- function(sim){
  # convert roads to sp points dataframe
  roads.pts <- raster::rasterToPoints(sim$roads, fun=function(x){x > 0}, spatial = TRUE)
  
  # union roads to one feature
  roads.pts <- sf::st_union(sf::st_as_sf(roads.pts))
  
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

roadCLUS.getClosestRoad.sf2 <- function(sim){
  # union roads to one feature
  roads.pts <- sf::st_union(sf::st_as_sf(sim$roads))
  
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

simList <- list(roads = demoScen[[1]]$road.rast, 
                costSurface = demoScen[[1]]$cost.rast,
                landings = rbind(demoScen[[1]]$landings.points, 
                                 demoScen[[1]]$landings.points))

simListSF <- list(roads = demoScen[[1]]$road.rast, 
                  costSurface = demoScen[[1]]$cost.rast,
                  landings = rbind(demoScen[[1]]$landings.points, 
                                   demoScen[[1]]$landings.points) %>%
                    sf::st_as_sf())

simListSF2 <- list(roads = demoScen[[1]]$road.line, 
                costSurface = demoScen[[1]]$cost.rast,
                landings = rbind(demoScen[[1]]$landings.points, 
                                 demoScen[[1]]$landings.points) %>%
                  sf::st_as_sf())

landingsSF <- sf::st_as_sf(demoScen[[1]]$landings.points)
roadsSF <- sf::st_as_sf(raster::rasterToPoints(demoScen[[1]]$road.rast, 
                                               fun=function(x){x > 0}, 
                                               spatial = TRUE)) %>% 
  sf::st_union()

roadsSF2 <- sf::st_as_sf(demoScen[[1]]$road.line) %>% 
  sf::st_union()

nearPts <- sf::st_nearest_points(landingsSF, roadsSF) 
nearPts2 <- sf::st_cast(nearPts, "POINT") %>% .[seq(2, length(.), 2)]

plot(sf::st_geometry(landingsSF))
plot(sf::st_geometry(roadsSF2), add = TRUE, col = "blue")
plot(nearPts2, add= TRUE, col = "red")
plot(nearPts, add= TRUE, col = "green")

spVersion <- roadCLUS.getClosestRoad(simList)

sfVersion <- roadCLUS.getClosestRoad.sf(simListSF)

sfVersion2 <- roadCLUS.getClosestRoad.sf2(simListSF2)

mb <- microbenchmark::microbenchmark(roadCLUS.getClosestRoad(simList),
                               roadCLUS.getClosestRoad.sf(simListSF), 
                               roadCLUS.getClosestRoad.sf2(simListSF2))
ggplot2::autoplot(mb)

#compare results
mismatchSfSp <- bind_cols(as.data.frame(sfVersion$roads.close.XY), 
                      as.data.frame(spVersion$roads.close.XY)) %>% 
  mutate(ind = 1:n()) %>% 
  filter(X != x | Y != y)

mismatchSf2Sp <- bind_cols(as.data.frame(sfVersion2$roads.close.XY), 
                          as.data.frame(spVersion$roads.close.XY)) %>% 
  mutate(ind = 1:n()) %>% 
  filter(X != x | Y != y)

# make points into sf dataframe
closest.roads.pts.sp2 <- as.data.frame(spVersion$roads.close.XY) %>% 
  mutate(ind = 1:n()) %>% 
  group_by(ind) %>% 
  mutate(geometry = sf::st_point(x = c(x, y)) %>% list() %>% sf::st_as_sfc()) %>% 
  sf::st_as_sf()

plot(landingsSF %>% sf::st_geometry())
plot(roadsSF %>% sf::st_geometry(), add = TRUE)
plot(closest.roads.pts.sf[mismatchSfSp$ind], add = TRUE, col = "red")
plot(closest.roads.pts.sp2[mismatchSfSp$ind,] %>% sf::st_geometry(),
     add = TRUE, col = "green")

# seems like a tie is broken differently for the two methods but otherwise the
# same when using raster as road input

plot(landingsSF %>% sf::st_geometry())
plot(roadsSF2 %>% sf::st_geometry(), add = TRUE)
plot(closest.roads.pts.sp2[mismatchSf2Sp$ind,] %>% sf::st_geometry(),
     add = TRUE, col = "green")
plot(closest.roads.pts.sf2, add = TRUE, col = "red", pch = 4)


# lcpList

roadCLUS.lcpList<- function(sim){
  ##Get a list of cell indexs for to and from points
  paths.matrix<-cbind(raster::cellFromXY(sim$costSurface,sim$landings ), raster::cellFromXY(sim$costSurface,sim$roads.close.XY ))
  sim$paths.list<-split(paths.matrix, 1:nrow(paths.matrix))
  rm(paths.matrix)
  gc()
  return(invisible(sim))
}

roadCLUS.lcpListNoGC<- function(sim){
  ##Get a list of cell indexs for to and from points
  paths.matrix<-cbind(raster::cellFromXY(sim$costSurface,sim$landings ), raster::cellFromXY(sim$costSurface,sim$roads.close.XY ))
  sim$paths.list<-split(paths.matrix, 1:nrow(paths.matrix))
  rm(paths.matrix)
  # gc()
  return(invisible(sim))
}
simLCPList <- roadCLUS.lcpList(spVersion)
microbenchmark::microbenchmark(roadCLUS.lcpList(spVersion),
                               roadCLUS.lcpListNoGC(spVersion))
# gc() HUGELY increases time!!
# Unit: microseconds
#                            expr        min          lq        mean      median
# roadCLUS.lcpList(spVersion)     181667.253 185912.3195 189554.0898 188157.7215
# roadCLUS.lcpListNoGC(spVersion)    141.503    190.9205    341.5714    212.7935
#          uq        max neval cld
# 191038.5345 239738.253   100   b
#    332.4225   8583.569   100  a 

roadCLUS.shortestPaths<- function(sim){
  #print('shortestPaths')
  #------finds the least cost paths between a list of two points
  if(!length(sim$paths.list)==0){
    #print(sim$paths.list)
    
    #create a list of shortest paths
    paths <- unlist(lapply(sim$paths.list, 
                         function(x) {
                           igraph::get.shortest.paths(sim$g, x[1], x[2], 
                                                      output = "both")})) 
    
    # save the verticies for mapping
    sim$paths.v <- rbind(data.table::data.table(paths[grepl("vpath", names(paths))]),
                         sim$paths.v) 
    
    paths.e <- paths[grepl("epath", names(paths))]
    
    # changes the cost(weight) associated with the edge that became a path (or
    # road)
    igraph::edge_attr(sim$g, 
                      index = igraph::E(sim$g)[igraph::E(sim$g) %in% paths.e], 
                      name= 'weight') <- 0.00001 
    
    # reset landings and roads close to them
    sim$landings<-NULL
    sim$roads.close.XY<-NULL
    sim$newRoads.lines<-newRoadsToLines(sim)
    rm(paths.e)
    gc()
  }
  return(invisible(sim))
}

simLCPList <- roadCLUS.getGraph(simLCPList, "octagon")

shortPathsList <- roadCLUS.shortestPaths(simLCPList)

# Only uses the cost surface to make graph
roadCLUS.getGraph<- function(sim,neighbourhood){
  ###Set the grpah which determines least cost paths
  #Creates a graph (sim$g) in inititation phase which can be updated and solved for paths
  sim$paths.v<-NULL
  #------prepare the cost surface raster
  ras.matrix<-raster::as.matrix(sim$costSurface)#get the cost surface as a matrix using the raster package
  
  weight<-c(t(ras.matrix)) #transpose then vectorize which matches the same order as adj
  weight<-data.table::data.table(weight) # convert to a data.table - faster for large objects than data.frame
  #weight<-data.table(getValues(sim$costSurface)) #Try
  weight[, id := seq_len(.N)] # get the id for ther verticies which is used to merge with the edge list from adj
  
  #------get the adjacency using SpaDES function adj
  #rooks case
  if(!is.element(neighbourhood,c("rook","octagon","queen"))){
    stop("neighbourhood type not recognized")
  }
  
  edges<-SpaDES.tools::adj(returnDT= TRUE, numCol = ncol(ras.matrix), numCell=ncol(ras.matrix)*nrow(ras.matrix), directions =4, cells = 1:as.integer(ncol(ras.matrix)*nrow(ras.matrix)))
  edges<-data.table::data.table(edges)
  #edges[from < to, c("from", "to") := .(to, from)]
  edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]
  edges<-unique(edges)
  edges.w1<-merge(x=edges, y=weight, by.x= "from", by.y ="id") #merge in the weights from a cost surface
  data.table::setnames(edges.w1, c("from", "to", "w1")) #reformat
  edges.w2<-data.table::setDT(merge(x=edges.w1, y=weight, by.x= "to", by.y ="id"))#merge in the weights to a cost surface
  data.table::setnames(edges.w2, c("from", "to", "w1", "w2")) #reformat
  edges.w2$weight <-(edges.w2$w1 + edges.w2$w2)/2 #take the average cost between the two pixels
  
  if(neighbourhood=="rook"){
    edges.weight =edges.w2
  }else{
    #bishop's case - multiply weights by 2^0.5
    if(neighbourhood=="octagon"){
      # based on length of diagonal of a square
      mW = 2^0.5
    }else{mW=1}
    weight$weight = weight$weight*mW
    edges<-SpaDES.tools::adj(returnDT= TRUE, numCol = ncol(ras.matrix), numCell=ncol(ras.matrix)*nrow(ras.matrix), directions ="bishop", cells = 1:as.integer(ncol(ras.matrix)*nrow(ras.matrix)))
    edges<-data.table::data.table(edges)
    
    #edges[from < to, c("from", "to") := .(to, from)]
    edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]
    edges<-unique(edges)
    
    edges.w1<-merge(x=edges, y=weight, by.x= "from", by.y ="id") #merge in the weights from a cost surface
    data.table::setnames(edges.w1, c("from", "to", "w1")) #reformat
    
    edges.w3<-data.table::setDT(merge(x=edges.w1, y=weight, by.x= "to", by.y ="id"))#merge in the weights to a cost surface
    data.table::setnames(edges.w3, c("from", "to", "w1", "w2")) #reformat
    edges.w3$weight<-(edges.w3$w1 + edges.w3$w2)/2 #take the average cost between the two pixels
    
    #------get the edges list
    edges.weight =rbind(edges.w2,edges.w3)
  }
  
  edges.weight<-edges.weight[stats::complete.cases(edges.weight), c(1:2, 5)] #get rid of NAs caused by barriers. Drop the w1 and w2 costs.
  edges.weight[, id := seq_len(.N)] #set the ids of the edge list. Faster than using as.integer(row.names())
  
  #------make the graph
  sim$g<-igraph::graph.edgelist(as.matrix(edges.weight)[,1:2], dir = FALSE) #create the graph using to and from columns. Requires a matrix input
  igraph::E(sim$g)$weight<-as.matrix(edges.weight)[,3]#assign weights to the graph. Requires a matrix input
  
  #------clean up
  rm(edges.w1,edges.w2,edges.w3, edges, weight, ras.matrix)#remove unused objects
  #gc() #garbage collection
  return(invisible(sim))
}

# try buildSimList

simList <- buildSimList(roads = demoScen[[1]]$road.rast, 
                        cost = demoScen[[1]]$cost.rast,
                        landings = demoScen[[1]]$landings.points, 
                        roadMethod = "snap")

simListSF <- buildSimList(roads = demoScen[[1]]$road.rast, 
                          cost = demoScen[[1]]$cost.rast,
                          landings = demoScen[[1]]$landings.points %>%
                            sf::st_as_sf(), 
                          roadMethod = "snap")

simListSF2 <- buildSimList(roads = demoScen[[1]]$road.line, 
                           cost = demoScen[[1]]$cost.rast,
                           landings = demoScen[[1]]$landings.points %>%
                             sf::st_as_sf(), 
                           roadMethod = "snap")
out <- getClosestRoad(simList)

# raster to lines using polygon and - buffer
rdRast <- demoScen[[1]]$road.rast

rdCont <- raster::rasterToContour(rdRast, nlevels = 1) %>% 
  sf::st_as_sf()

rdLine <- sf::st_simplify(rdCont, dTolerance = 4)

