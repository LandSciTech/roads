#simple example test case - compare to results from kylesCLUSExample.Rmd
library(igraph)
library(SpaDES)
library(raster)
library(data.table)
library(rgeos)
library(sf)
library(dplyr)

# function for comparing outputs from Kyle's CLUS road sim example to those from the roads::projectRoads function.
#
# landings can be any of the classes accecpted by roads::projectRoads (SpatialPolygon not supported yet)
#   - if left as default, NULL, the landings that were in the CLUS road sim example will be used
#   - if landings is a raster brick or stack, the tests will be performed across all landings scenarios
# cost is a cost raster
#   - landings are expected to fall within this raster
#   - cost under existing roads is expected to be zero
#   - if left as default, NULL, the cost raster (ras) that was used in the CLUS road sim example will be used
# roads is a raster representing existing roads
#   - expected to have same dimensions as cost
#   - expected to be TRUE where existing roads are, and NA elsewhere
#   - can just use cost==0 here
#   - if left as default, NULL, the same initial roads setup as was used in the CLUS road sim example will be used
# roadMethod is a character string representing which road sim method(s) to test
#   - expected to be one of:  "snap","lcp","mst","all"
#
# returns:
#   - if roadMethod is not all (i.e. a particular simulation method of interest):
#       - if there was only one landings scenario (i.e. not a raster stack/brick):
#            returns TRUE if roads::projectRoads output matches output from Kyle's CLUS road sim example
#            returns FALSE if roads::projectRoads output matches output from Kyle's CLUS road sim example
#       - if there were many landings scenarios (i.e. a raster stack/brick):
#            returns a logical vector with length equal to the number of landings scenarios
#                TRUE if roads::projectRoads output matches output from Kyle's CLUS road sim example, under given landings scenario
#                FALSE if roads::projectRoads output matches output from Kyle's CLUS road sim example, under given landings scenario
#   - if roadMethod is all (i.e. testing all methods):
#       - if there was only one landings scenario (i.e. not a raster stack/brick):
#            returns a named list (names: "snap","lcp","mst" representing method) of logical elements
#                TRUE if roads::projectRoads output matches output from Kyle's CLUS road sim example, under particular method
#                FALSE if roads::projectRoads output matches output from Kyle's CLUS road sim example, under particular method
#       - if there were many landings scenario (i.e. a raster stack/brick):
#            returns a list of named lists (each stuctured same as the one-landings scenario above) with length equal to the number of landings scenarios
#
compareRoadSimResults <- function(landings=NULL,cost=NULL,roads=NULL,roadMethod="all"){
  #### for testing in the global env, uncomment the following line
  #landings=NULL;cost=NULL;roads=NULL;roadMethod="all"
  #### checks and defaults
  # if cost is NULL, prepare it as was set up in the CLUS example
  if(is.null(cost)){
    ## FROM CLUS example:
    #Empty raster0 
    x.size = 5
    y.size = 5
    ras = raster(extent(0, x.size, 0, y.size),res =1, vals =1)
    set.seed(1)
    ras[]<-runif(as.integer(x.size*x.size), 1,20)
    ras[1:5]<-0
    #####
    cost<-ras
  }
  # if roads is NULL, prepare it as was set up in the CLUS example
  if(is.null(roads)){
    if(!any((cost==0)[])){
      stop("If roads is null, cost raster should contain values of 0, representing roads")
    }else{
      roads <- cost==0
    }
  }
  # if landings is NULL, prepare it as was set up in the CLUS example
  # if landings is not a "SpatialPoints", prepare a comparable "SpatialPoints" object to run throught the CLUS example  
  if(is.null(landings)){
    sC.list<-list(SpatialPoints(xyFromCell(cost, as.integer(c(11,13,22,25)), spatial=FALSE)))
    landings <- sC.list[[1]]
  }else if(class(landings)=="matrix"){
    sC.list<-list(SpatialPoints(landings))
  }else if(class(landings)=="RasterLayer"){
    sC.list<-list(SpatialPoints(xyFromCell(cost,which((landings==TRUE)[]), spatial=FALSE)))
  }else if(class(landings)=="SpatialPoints"){
    sC.list<-list(landings)
  }else if(class(landings)%in%c("RasterBrick","RasterStack")){
    sC.list<-list()
    for(i in 1:dim(landings)[3]){
      sC.list[[i]] <- SpatialPoints(xyFromCell(cost,which((landings[[i]]==TRUE)[]), spatial=FALSE))
    }
  }else if(class(landings)=="SpatialPolygons"){
    stop("Check for landings of class SpatialPolygons is not supported yet")
  }else{
    stop("landings expected to be NULL or one of the following classes:\n    ",
         "'matrix','RasterLayer','SpatialPolygons','SpatialPoints','RasterStack','RasterBrick'")
  }
  # prepare logical objects (mst,lcp,snap) rerpresenting which road sim methods are to be compared agains the CLUS example 
  mst<-F;lcp<-F;snap<-F
  if (roadMethod=="all"){mst<-T;lcp<-T;snap<-T
  }else if(roadMethod=="mst"){mst<-T}else if(roadMethod=="lcp"){lcp<-T}else if(roadMethod=="snap"){snap<-T
  }else{stop("roadMethod expected to be one of:  \"all\",\"mst\",\"lcp\",\"snap\"")}
  # cycle through landings list (sC.list) running the user-specified comparisons
  out.list <- list()
  ras <- cost
  for (i in 1:length(sC.list)){
    out.list[[i]]<-list(mst=NA,lcp=NA,snap=NA)
    sC <- sC.list[[i]]
    ##### FROM CLUS example: 
    # convert the raster pixels that are roads (i.e., cost = 0) to points
    roads.pts <- rasterToPoints(ras, fun=function(x){x == 0})  
    # get the distance between the two geometries (road points and targets) and select the minimum
    closest.roads.pts <- apply(rgeos::gDistance(SpatialPoints(roads.pts),SpatialPoints(sC), byid=TRUE), 1, which.min)
    # convert to a matrix
    roads.close.XY <- as.matrix(roads.pts[closest.roads.pts, 1:2,drop=F]) 
    #####
    if (mst | lcp){
      #### FROM CLUS example: 
      #convert the cost surface raster to a matrix
      ras.matrix<-raster::as.matrix(ras)
      weight<-c(t(ras.matrix)) # transpose then vectorize. This follows how raster layer objects are read
      weight<-data.table(weight) # convert to a data.table
      weight$id<-as.integer(row.names(weight)) # add an ID
      #---------------
      #build the graph
      #---------------
      #get the adjaceny
      edges<-adj(returnDT= TRUE, numCol = 5, numCell=25, directions =8, cells = 1:25)
      #merge and average between the to and from 
      test<-merge(x=edges, y=weight, by.x= "from", by.y ="id")
      setnames(test, c("from", "to", "w1"))
      test2<-setDT(merge(x=test, y=weight, by.x= "to", by.y ="id"))
      setnames(test2, c("from", "to", "w1", "w2"))
      test2$weight<-(test2$w1 + test2$w2) /2 # take the average between the two pixels
      edges.weight<-test2[complete.cases(test2), ]
      edges.weight$id<-1:nrow(edges.weight)
      g<-graph.edgelist(as.matrix(edges.weight)[,1:2], dir = FALSE)
      ####
      #assign weights to the graph
      E(g)$weight<-as.matrix(edges.weight)[,5]
    }
    if(mst){
      #### FROM CLUS example:
      # Minimum Spanning Tree (MST) with Least Cost Paths (LCP) Approach 
      mst.v <- as.vector(rbind(cellFromXY(ras,sC ), cellFromXY(ras, roads.close.XY )))
      paths.matrix<-as.matrix(mst.v)
      paths.matrix<- paths.matrix[!duplicated(paths.matrix[,1]),]
      mst.adj <- distances(g, paths.matrix, paths.matrix) # get an adjaceny matrix given then cell numbers
      # set the verticies names as the cell numbers in the costSurface
      rownames(mst.adj)<-paths.matrix 
      # set the verticies names as the cell numbers in the costSurface
      colnames(mst.adj)<-paths.matrix 
      mst.g <- graph_from_adjacency_matrix(mst.adj, weighted=TRUE) # create a graph
      mst.paths <- mst(mst.g, weighted=TRUE) # get the the minimum spanning tree
      paths.matrix<-noquote(get.edgelist(mst.paths, names=TRUE)) #get the paths needed for solving the LCP
      class(paths.matrix) <- "numeric"
      paths.list<-split(paths.matrix, 1:nrow(paths.matrix))
      # Get the LCP's
      paths<-unlist(lapply(paths.list, function(x) get.shortest.paths(g, x[1], x[2], out = "both")))
      paths.v<-NULL
      paths.v<-unique(rbind(data.table(paths[grepl("vpath",names(paths))] ), paths.v))
      paths.e<-paths[grepl("epath",names(paths))]
      ####
      ### MODIFICATION OF CLUS example (same as the 'rasterize the paths' section, but not specific to that one scenario):
      CLUS.roads.mst.ras <- raster(extent(0,ncol(ras),0,nrow(ras)),res=res(ras)[1],vals=NA)
      CLUS.roads.mst.ras[unlist(paths.v)] <- 1
      CLUS.roads.mst.ras[ras==0] <- 0
      CLUS.roads.mst.ras[is.na(CLUS.roads.mst.ras)]<-0
      ### USING roads::projectRoads
      if(class(landings)%in%c("RasterBrick","RasterStack")){
        pR.res.mst <- roads::projectRoads(landings=landings[[i]],cost=cost,roads=roads,roadMethod = "mst")
      }else{
        pR.res.mst <- roads::projectRoads(landings=landings,cost=cost,roads=roads,roadMethod = "mst")
      }
      pR.roads.mst.ras <- pR.res.mst$roads==0
      pR.roads.mst.ras[is.na(pR.roads.mst.ras)] <- 0
      if(any((CLUS.roads.mst.ras-pR.roads.mst.ras)[]!=0)){
        out.list[[i]]$mst <- FALSE
      }else{
        out.list[[i]]$mst <- TRUE
      }
      rm(mst.v,paths.matrix,mst.adj,mst.g,mst.paths,paths.list,paths,paths.v,paths.e)
    } ## END OF mst CHECK
    if(lcp){
      #### FROM CLUS example:
      paths.matrix<-cbind(cellFromXY(ras,sC ), cellFromXY(ras, roads.close.XY ))
      paths.list<-split(paths.matrix, 1:nrow(paths.matrix)) #convert to a list for lapply
      #get the shortest paths
      paths<-unlist(lapply(paths.list, function(x) get.shortest.paths(g, x[1], x[2], out = "both"))) #both the epath (edge paths) and vpath (vertex path) are required
      paths.v<-NULL
      paths.v<-unique(rbind(data.table(paths[grepl("vpath",names(paths))] ), paths.v)) #remove same paths
      paths.e<-paths[grepl("epath",names(paths))] #get the edge paths because these have the raster id labels
      ### MODIFICATION OF CLUS example (same as the 'rasterize the paths' section, but not specific to that one scenario):
      CLUS.roads.lcp.ras <- raster(extent(0,ncol(ras),0,nrow(ras)),res=res(ras)[1],vals=NA)
      CLUS.roads.lcp.ras[unlist(paths.v)] <- 1
      CLUS.roads.lcp.ras[ras==0] <- 0
      CLUS.roads.lcp.ras[is.na(CLUS.roads.lcp.ras)]<-0
      ### USING roads::projectRoads
      if(class(landings)%in%c("RasterBrick","RasterStack")){
        pR.res.lcp <- roads::projectRoads(landings=landings[[i]],cost=cost,roads=roads,roadMethod = "lcp")
      }else{
        pR.res.lcp <- roads::projectRoads(landings=landings,cost=cost,roads=roads,roadMethod = "lcp")
      }
      pR.roads.lcp.ras <- pR.res.lcp$roads==0
      pR.roads.lcp.ras[is.na(pR.roads.lcp.ras)] <- 0
      if(any((CLUS.roads.lcp.ras-pR.roads.lcp.ras)[]!=0)){
        out.list[[i]]$lcp <- FALSE
      }else{
        out.list[[i]]$lcp <- TRUE
      }
    } ## END OF lcp CHECK
    if(snap){
      #### FROM CLUS example (some slight changes made in order to not conflict with other objects used in this comparison):
      rdptsXY<-data.frame(roads.close.XY) #convert to a data.frame
      rdptsXY$id<-as.numeric(row.names(rdptsXY))
      sC_CLUS <- sC
      sC_CLUS$ID <- 1:length(sC_CLUS) 
      landingsCLUS<-data.frame(sC_CLUS)
      landingsCLUS<-landingsCLUS[,2:3]
      landingsCLUS$id<-as.numeric(row.names(landingsCLUS))
      coordMatrix<-rbind(rdptsXY,landingsCLUS)
      coordMatrix$attr_data<-100
      mt<-coordMatrix %>% 
        st_as_sf(coords=c("x","y"))%>% 
        group_by(id) %>% 
        summarize(m=mean(attr_data)) %>% 
        st_cast("LINESTRING")
      # added for conversion of lines (mt) to roads raster
      mt_cells <- do.call(rbind,extract(ras,mt,cellnumber=TRUE))
      mt_cells <- mt_cells[,which(colnames(mt_cells)=="cell")]
      CLUS.roads.snap.ras <- raster(extent(0,ncol(ras),0,nrow(ras)),res=res(ras)[1],vals=0)
      CLUS.roads.snap.ras[mt_cells] <- 1
      CLUS.roads.snap.ras[ras==0] <- 0
      ### USING roads::projectRoads
      if(class(landings)%in%c("RasterBrick","RasterStack")){
        pR.res.snap <- roads::projectRoads(landings=landings[[i]],cost=cost,roads=roads,roadMethod = "snap")
      }else{
        pR.res.snap <- roads::projectRoads(landings=landings,cost=cost,roads=roads,roadMethod = "snap")
      }
      pR.roads.snap.ras <- pR.res.snap$roads==0
      pR.roads.snap.ras[is.na(pR.roads.snap.ras)] <- 0
      if(any((CLUS.roads.snap.ras-pR.roads.snap.ras)[]!=0)){
        out.list[[i]]$snap <- FALSE
      }else{
        out.list[[i]]$snap <- TRUE
      }
    }## END OF snap CHECK
  } ## END OF CYCLING THROUGH LANDINGS/START CELLS LIST
  if(roadMethod=="snap"){
    return(unlist(lapply(out.list,function(x)x$snap)))
  }elif(roadMethod=="lcp"){
    return(unlist(lapply(out.list,function(x)x$lcp)))
  }elif(roadMethod=="mst"){
    return(unlist(lapply(out.list,function(x)x$mst)))
  }elif(roadMethod=="all"){
    if(length(out.list)==1){
      return(out.list[[1]])
    }else{
      return(out.list)
    }
  }
}


#Empty raster0 
x.size = 5
y.size = 5
ras = raster(extent(0, x.size, 0, y.size),res =1, vals =1)
set.seed(1)
ras[]<-runif(as.integer(x.size*x.size), 1,20)
ras[1:5]<-0
plot(ras)
title('Cost Surface')
startCells <- xyFromCell(ras, as.integer(c(11,13,22,25)), spatial=FALSE)
sC<-SpatialPoints(startCells)
sC$ID<-paste(1:4, sep = "")
plot(sC, col ='red', add=TRUE)
segments(0,5,5,5, lwd =2)
text(sC, labels=sC$ID, pos=2)
lines(c(0,5),c(4.5,4.5), lwd =2)
sC$ID = as.numeric(sC$ID)

cost=ras
landings=sC
roads = ras==0

#devtools::install_github("LandSciTech/roads")
library(roads)
roadMethod="mst"
outRoads = projectRoads(landings=landings,cost=cost,roads=roads,roadMethod=roadMethod,plotRoads=T)
pdf("roadNetworkGrowthMST.pdf")
plot(outRoads$roads>0)#
dev.off()

roadMethod="lcp"
outRoads = projectRoads(landings=landings,cost=cost,roads=roads,roadMethod=roadMethod,plotRoads=T)
pdf("roadNetworkGrowthLCP.pdf")
plot(outRoads$roads>0)#
dev.off()

roadMethod="snap"
#NOTE:something is wrong with snap. On TO DO list for Josie
outRoads = projectRoads(landings=landings,cost=cost,roads=roads,roadMethod=roadMethod,plotRoads=T)
pdf("roadNetworkGrowthSnap.pdf")
plot(outRoads$roads>0)#
dev.off()

#TASKS
# Get Scott to help setup git/github/local copy of repository. Workflow is [pull, do stuff, commit, push] each time you work on the project.
# Formally (return TRUE/FALSE) compare to Kyle's output - confirm methods are returning the same thing
# Repeat for "mst" and "lcp" roadMethods
# Set up as formal test suite: http://r-pkgs.had.co.nz/tests.html
# Add examples to projectRoads documentation - Josie will explain how to when you get to this step
