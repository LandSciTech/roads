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

#NOTICE: These functions have been modified from https://github.com/bcgov/clus/blob/master/R/SpaDES-modules/roadCLUS/roadCLUS.R

#Observations:
# - Using sim as argument for functions decreases transparency and modularity. Need to dig in to figure what inputs and outputs are. And risk of unintended consequences - function writer is free to mess with anything in sim object.
# - Not enough metadata. What exactly is landings eg. Raster ok. But how are values interpreted?

# @import raster
#' @importFrom raster rasterToPoints as.data.frame clump rasterize cellFromXY merge as.matrix
# @import sp
#' @importFrom sp SpatialPoints
# @import gdistance
# @import data.table
#' @importFrom data.table data.table setDT setnames
# @import igraph
#' @importFrom igraph graph.edgelist E distances graph_from_adjacency_matrix mst get.edgelist get.shortest.paths edge_attr
# @import latticeExtra
# @import sf
#' @importFrom sf st_as_sf st_cast st_buffer
# @import rgeos
#' @importFrom rgeos gDistance
#' @import dplyr
NULL
#library(data.table);library(igraph)

#' @export
roadCLUS.analysis <- function(sim){
  if(!is.element("roadMethod",names(sim))||!(sim$roadMethod == 'snap')){
    ras.out<-sim$costSurface
    ras.out[]<-1:ncell(ras.out)
    ras.out[!(ras.out[] %in% as.matrix(sim$paths.v))] <- NA
    #ras.out<-raster::reclassify(ras.out, c(0.000000000001, maxValue(ras.out),0))
    if(is.element("roads",names(sim))){
      sim$roads<-raster::merge(ras.out, sim$roads)
    }else{
      sim$roads = ras.out
    }
  }
  return(invisible(sim))
}

#' @export
roadCLUS.getGraph<- function(sim){
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
  edges<-SpaDES.tools::adj(returnDT= TRUE, numCol = ncol(ras.matrix), numCell=ncol(ras.matrix)*nrow(ras.matrix), directions =8, cells = 1:as.integer(ncol(ras.matrix)*nrow(ras.matrix)))
  edges<-data.table::data.table(edges)
  edges[from < to, c("from", "to") := .(to, from)]

  edges<-unique(edges)
  edges.w1<-merge(x=edges, y=weight, by.x= "from", by.y ="id") #merge in the weights from a cost surface
  data.table::setnames(edges.w1, c("from", "to", "w1")) #reformat
  edges.w2<-data.table::setDT(merge(x=edges.w1, y=weight, by.x= "to", by.y ="id"))#merge in the weights to a cost surface
  data.table::setnames(edges.w2, c("from", "to", "w1", "w2")) #reformat
  edges.w2$weight<-(edges.w2$w1 + edges.w2$w2)/2 #take the average cost between the two pixels

  #------get the edges list
  edges.weight<-edges.w2[complete.cases(edges.w2), c(1:2, 5)] #get rid of NAs caused by barriers. Drop the w1 and w2 costs.
  edges.weight[, id := seq_len(.N)] #set the ids of the edge list. Faster than using as.integer(row.names())

  #------make the graph
  sim$g<-igraph::graph.edgelist(as.matrix(edges.weight)[,1:2], dir = FALSE) #create the graph using to and from columns. Requires a matrix input
  igraph::E(sim$g)$weight<-as.matrix(edges.weight)[,3]#assign weights to the graph. Requires a matrix input

  #------clean up
  rm(edges.w1,edges.w2, edges, weight, ras.matrix)#remove unused objects
  gc() #garbage collection
  return(invisible(sim))
}

roadCLUS.lcpList<- function(sim){
  ##Get a list of paths from which there is a to and from point
  paths.matrix<-cbind(raster::cellFromXY(sim$costSurface,sim$landings ), raster::cellFromXY(sim$costSurface,sim$roads.close.XY ))
  sim$paths.list<-split(paths.matrix, 1:nrow(paths.matrix))
  rm(paths.matrix)
  gc()
  return(invisible(sim))
}

roadCLUS.mstList<- function(sim){
  #print('mstList')
  mst.v <- as.vector(rbind(raster::cellFromXY(sim$costSurface,sim$landings ), raster::cellFromXY(sim$costSurface,sim$roads.close.XY )))
  paths.matrix<-as.matrix(mst.v)
  paths.matrix<- paths.matrix[!duplicated(paths.matrix[,1]),]
  #print(paths.matrix)
  if(length(paths.matrix) > 1){
    mst.adj <- igraph::distances(sim$g, paths.matrix, paths.matrix) # get an adjaceny matrix given then cell numbers
    #print(mst.adj)
    rownames(mst.adj)<-paths.matrix # set the verticies names as the cell numbers in the costSurface
    colnames(mst.adj)<-paths.matrix # set the verticies names as the cell numbers in the costSurface
    mst.g <- igraph::graph_from_adjacency_matrix(mst.adj, weighted=TRUE) # create a graph
    mst.paths <- igraph::mst(mst.g, weighted=TRUE) # get the the minimum spanning tree
    paths.matrix<-noquote(igraph::get.edgelist(mst.paths, names=TRUE))
    class(paths.matrix) <- "numeric"
    sim$paths.list<-split(paths.matrix, 1:nrow(paths.matrix)) # put the edge combinations in a list used for shortestPaths
    #print(sim$paths.list)
    rm(mst.paths,mst.g, mst.adj, mst.v, paths.matrix)
    gc()
  }
  return(invisible(sim))
}

roadCLUS.shortestPaths<- function(sim){
  #print('shortestPaths')
  #------finds the least cost paths between a list of two points
  if(!length(sim$paths.list)==0){
    #print(sim$paths.list)
    paths<-unlist(lapply(sim$paths.list, function(x) igraph::get.shortest.paths(sim$g, x[1], x[2], out = "both"))) #create a list of shortest paths
    sim$paths.v<-unique(rbind(data.table::data.table(paths[grepl("vpath",names(paths))] ), sim$paths.v))#save the verticies for mapping
    paths.e<-paths[grepl("epath",names(paths))]
    igraph::edge_attr(sim$g, index= igraph::E(sim$g)[igraph::E(sim$g) %in% paths.e], name= 'weight')<-0.00001 #changes the cost(weight) associated with the edge that became a path (or road)

    #reset landings and roads close to them
    sim$landings<-NULL
    sim$roads.close.XY<-NULL
    rm(paths.e)
    gc()
  }
  return(invisible(sim))
}

roadCLUS.getClosestRoad <- function(sim){
  roads.pts <- raster::rasterToPoints(sim$roads, fun=function(x){x > 0})
  closest.roads.pts <- apply(rgeos::gDistance(sp::SpatialPoints(roads.pts),sp::SpatialPoints(sim$landings), byid=TRUE), 1, which.min)
  sim$roads.close.XY <- as.matrix(roads.pts[closest.roads.pts, 1:2,drop=F]) #this function returns a matrix of x, y coordinates corresponding to the closest road
  #The drop =F is needed for a single landing - during the subset of a matrix it will become a column vector because as it converts a vector to a matrix, r will assume you have one column
  rm(roads.pts, closest.roads.pts)
  gc()
  return(invisible(sim))
}

roadCLUS.buildSnapRoads <- function(sim){
  rdptsXY<-data.frame(sim$roads.close.XY) #convert to a data.frame
  rdptsXY$id<-as.numeric(row.names(rdptsXY))
  landings<-data.frame(sim$landings)
  landings$id<-as.numeric(row.names(landings))
  coodMatrix<-rbind(rdptsXY,subset(landings,select=names(rdptsXY)))
  coodMatrix$attr_data<-100

  #remove anything that is a point, rather than a line
  checkPts = as.data.frame(table(unique(coodMatrix)$id))
  checkPts = subset(checkPts,Freq>1)
  coodMatrix=merge(coodMatrix,data.frame(id=checkPts$Var1))

  mt<-coodMatrix %>% sf::st_as_sf(coords=c("x","y"))%>% dplyr::group_by(id) %>% dplyr::summarize(m=mean(attr_data)) %>% sf::st_cast("LINESTRING")
  test<-fasterize::fasterize(sf::st_buffer(mt,50),sim$roads, field = "m")
  sim$roads<-raster::merge(test, sim$roads)
  rm(rdptsXY, landings, mt, coodMatrix, test)
  gc()
  return(invisible(sim))
}

#' @export
getCentroids<-function(newLandings,withIDs=T){
  cRes = raster::res(newLandings)
  p = raster::as.data.frame(raster::clump(newLandings,gaps=F), xy = TRUE)
  #p =  data.frame(raster::rasterToPoints(raster::clump(newLandings)))
  p = p[!is.na(p$clumps),]
  pointLocs = plyr::ddply(p,plyr::.(clumps),plyr::summarize,x=mean(x),y=mean(y))
  pointLocs$x = cRes[1]*round(pointLocs$x/cRes[1])
  pointLocs$y=cRes[2]*round(pointLocs$y/cRes[2])

  pointLocs=subset(pointLocs,select=c(x,y,clumps))

  newLandingCentroids = newLandings
  newLandingCentroids[!is.na(newLandingCentroids)]=NA
  cells = raster::cellFromXY(newLandingCentroids,pointLocs[,1:2])
  if(withIDs){
    newLandingCentroids[cells] = pointLocs$clumps
  }else{
    newLandingCentroids[cells] = 1
  }
  return(newLandingCentroids)
}
#' @export
getLandingsFromTarget<-function(inputPatches,numLandings){
  #Function to select a specific number of landings withing patches.
  #Landing set will include centroids, and additional randomly selected sample points if numLandings>numCentroids.
  #inputPatches=anthDist;numLandings=numADSmall
  inputPatches[inputPatches==0]=NA

  landings = getCentroids(inputPatches,withIDs=T)
  landings = raster::rasterToPoints(landings,fun=function(landings){landings>0})
  #split into smaller patches to ensure adequate road density
  #sampleProp = 1/100
  #numSamples = round(cellStats(anthDist,"sum")*sampleProp)
  numSamples = numLandings-nrow(landings)#select additional points so total number is equal to small alternative

  if(numSamples<=0){
    return(landings)
  }
  landingPts = raster::sampleStratified(inputPatches, size=numSamples,xy=T)
  landingPts=landingPts[,2:4]
  #add centroids to ensure all patches are included
  landings = rbind(landingPts,landings)
  return(landings)
}


#' @export
simpleCost<-function(roads,newLandings,water){
  #roads=cRoadsRaster
  if(class(roads)!="RasterLayer"){
    roads= raster::rasterize(roads,newLandings)
  }
  cost = (is.na(roads))&(is.na(newLandings))
  cost[(water>0)&(cost>0)]=water[(water>0)&(cost>0)]
  return(cost)
}

#' @export
adjustBrickNames<-function(x,ctag="X",cType="this"){
  #x=myTransitionGroup;ctag=tag
  cNames = names(x);cNames=gsub(ctag,"",cNames,fixed=T)

  outTag = "t"

  nNames =suppressWarnings(as.integer(cNames))

  if (is.element(NA,nNames)){
    if(length(nNames)==1){

      #if given a single unnamed map, assume it is static over time.
      warning(paste0("Name cannot be interpreted as an integer so assuming ",cType," doesn't change over time."))
      names(x) = "static"
    }else{

      #assume this is an ordered time-series and rename
      warning(paste0("Names cannot be interpreted as integers, so assuming ",cType," is an ordered time-series starting at time 1."))
      names(x) = paste0(outTag,seq(1,length(cNames)))
    }
    return(x)
  }

  outOrder = data.frame(outName = nNames,outID=seq(1:length(nNames)))
  outOrder = outOrder[order(outOrder$outName),]
  names(x)=paste0(outTag,nNames)

  if(identical(outOrder$outName,nNames)){
    return(x)
  }

  if(class(x)=="list"){
    return(x[outOrder$outName])

  }
  if(class(x)=="RasterStack"){
    x = raster::brick(x)
  }
  if(class(x)=="RasterBrick"){
    return(subset(x,outOrder$outID))
  }

  stop("Class of x not recognized. Expecting a list or RasterBrick")
}


