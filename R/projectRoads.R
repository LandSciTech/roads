#' Project road network.
#'
#' @details
#' some details...
#'
#' @param landings RasterLayer, SpatialPolygons*, SpatialPointsDataFrame*, matrix, or RasterStack. Features to be connected to the road network. matrix should contain x,y,v columns, as returned by rasterToPoints etc. If RasterStack assume an ordered time-series.
#' @param cost RasterLayer. Cost surface.
#' @param roads RasterLayer. Existing road network.
#' @param roadMethod Character. Options are "mst", "lcp", "snap".
#' @param plotRoads Boolean. Set FALSE to save time if output road rasters are not required. Default TRUE
#' @param neighbourhood Character. 'rook','queen', or 'octagon'. 'octagon' option is a modified version of the queen's 8 cell neighbourhood in which diagonals weights are 2^0.5x higher than horizontal/vertical weights.
#' @param sim Sim list. Returned from a previous iteration of projectRoads. cost, roads, and roadMethod are ignored if a sim list is provided.
#' @examples
#'
#' ## visualize function's height parameter represents graphics window height in cm.
#' ## Increase it for larger visualizations.
#'
#' ### using:  scenario 1 / SpatialPointsDataFrame landings / least-cost path ("lcp")
#' scen <- demoScen[[1]] # demo scenario 1
#' # landing set 1 of scenario 1:
#' land.pnts <- scen$landings.points[scen$landings.points$set==1,]
#' prRes <- projectRoads(land.pnts,scen$cost.rast,scen$road.rast,"lcp")
#' visualize(scen$cost.rast,land.pnts,prRes,height=15)
#'
#' ### using: scenario 1 / RasterLayer landings / minimum spanning tree ("mst")
#' scen <- demoScen[[1]] # demo scenario 1
#' # the RasterLayer version of landing set 1 of scenario 1:
#' land.rLyr <- scen$landings.stack[[1]]
#' prRes <- projectRoads(land.rLyr,scen$cost.rast,scen$road.rast,"mst")
#' visualize(scen$cost.rast,land.rLyr,prRes)
#'
#' ### using: scenario 2 / matrix landings / snapping ("snap")
#' scen <- demoScen[[2]] # demo scenario 2
#' # landing set 5 of scenario 2, as matrix:
#' land.mat  <- scen$landings.points[scen$landings.points$set==5,]@coords
#' prRes <- projectRoads(land.mat,scen$cost.rast,scen$road.rast,"snap")
#' visualize(scen$cost.rast,land.mat,prRes,height=15)
#'
#' ### using: scenario 3 / RasterStack landings / minimum spanning tree ("mst")
#' scen <- demoScen[[3]] # demo scenario 3
#' # landing sets 1 to 4 of scenario 3, as RasterStack:
#' land.rstack <- scen$landings.stack[[1:4]]
#' prRes <- projectRoads(land.rstack,scen$cost.rast,scen$road.rast,"mst")
#' visualize(scen$cost.rast,land.rstack,prRes,height=15)
#'
#' ### using: scenario 7 / SpatialPolygonsDataFrame landings / minimum spanning tree ("mst")
#' scen <- demoScen[[7]] # demo scenario 7
#' # polygonal landings of demo scenario 7:
#' land.poly <- scen$landings.poly
#' prRes <- projectRoads(land.poly,scen$cost.rast,cen$road.rast,mst")
#' visualize(scen$cost.rast,land.poly,prRes,height=15)
#' @export
setGeneric('projectRoads',function(landings,cost=NULL,roads=NULL,roadMethod="mst",plotRoads=T,neighbourhood="octagon",sim=list()) standardGeneric('projectRoads'))

#' @return  sim list.
#' @rdname projectRoads
setMethod('projectRoads', signature(landings="matrix"), function(landings,cost,roads,roadMethod,plotRoads,neighbourhood,sim) {
  #x=newLandingCentroids;roadMethod="mst";cost=cCost;roads=cRoadsRaster[[ym]];
  #sim=list();roadMethod="lcp"
  recognizedRoadMethods = c("mst","lcp","snap")

  if (length(sim)>0){ #ignore cost, roads, roadMethod
    expectBits = c("roads","costSurface","roadMethod")
    missingNames = setdiff(expectBits,names(sim))
    if(length(missingNames)>0){
      stop("sim list missing expected elements: ",paste(missingNames, collapse=","))
    }
  }else{
    if(!is.element(roadMethod,recognizedRoadMethods)){
      stop("Invalid road method ",roadMethod,". Options are:",paste(recognizedRoadMethods,collapse=','))
    }
    #set up sim list
    roads=roads>0
    sim$roads=roads
    sim$costSurface=cost
    sim$roadMethod = roadMethod
  }

  sim$landings=landings
  if(!is.element("g",names(sim))){
    sim <- roadCLUS.getGraph(sim,neighbourhood)
  }

  if(!is.null(sim$landings)){
    switch(sim$roadMethod,
           snap= {
             sim <- roadCLUS.getClosestRoad(sim)
             sim <- roadCLUS.buildSnapRoads(sim)
           } ,
           lcp ={
             sim <- roadCLUS.getClosestRoad(sim)
             sim <- roadCLUS.lcpList(sim)
             sim <- roadCLUS.shortestPaths(sim)# includes update graph
           },
           mst ={
             sim <- roadCLUS.getClosestRoad(sim)
             sim <- roadCLUS.mstList(sim)# will take more time than lcpList given the construction of a mst
             sim <- roadCLUS.shortestPaths(sim)# update graph is within the shortestPaths function
           }
    )
    if(plotRoads){
      sim <- roadCLUS.analysis(sim)

    }
  }
  return(sim)
})

#' @return  sim list.
#' @rdname projectRoads
setMethod('projectRoads', signature(landings="RasterLayer"), function(landings,cost,roads,roadMethod,plotRoads,neighbourhood,sim) {
  landings = getCentroids(landings,withIDs=T)
  landings = raster::rasterToPoints(landings,fun=function(landings){landings>0})
  return(projectRoads(landings=landings,cost=cost,roads=roads,roadMethod=roadMethod,plotRoads=plotRoads,neighbourhood=neighbourhood,sim=sim))
})

#' @return  sim list.
#' @rdname projectRoads
setMethod('projectRoads', signature(landings="SpatialPolygons"), function(landings,cost,roads,roadMethod,plotRoads,neighbourhood,sim) {
  landings = raster::rasterize(landings,cost)
  return(projectRoads(landings=landings,cost=cost,roads=roads,roadMethod=roadMethod,plotRoads=plotRoads,neighbourhood=neighbourhood,sim=sim))
})

#' @return  sim list.
#' @rdname projectRoads
setMethod('projectRoads', signature(landings="SpatialPoints"), function(landings,cost,roads,roadMethod,plotRoads,neighbourhood,sim) {
    #landings=sC
    #landings = raster::subset(raster::rasterize(landings,cost),1)
    cco = landings@coords
    if(is.element("data",slotNames(landings))){
      cco=cbind(cco,landings@data$ID)
    }else{
      cco = cbind(cco,seq(1,nrow(cco)))
    }
    dimnames(cco)[[2]] = c("x", "y", "layer")
    return(projectRoads(landings=cco,cost=cost,roads=roads,roadMethod=roadMethod,plotRoads=plotRoads,neighbourhood=neighbourhood,sim=sim))
})

#' @return  RasterBrick. Road network over time.
#' @rdname projectRoads
setMethod('projectRoads', signature(landings="RasterStack"), function(landings,cost,roads,roadMethod,plotRoads,neighbourhood,sim) {
  return(projectRoads(raster::brick(landings),cost,roads,roadMethod,plotRoads,neighbourhood,sim))
})

#' @return  RasterBrick. Road network over time.
#' @rdname projectRoads
setMethod('projectRoads', signature(landings="RasterBrick"), function(landings,cost,roads,roadMethod,plotRoads,neighbourhood,sim) {
  checkAllign = raster::compareRaster(cost,roads)
  if(!checkAllign){
    stop("Problem with roads. All rasters must have the same same extent, number of rows and columns,
         projection, resolution, and origin.")
  }
  checkAllign = raster::compareRaster(cost,landings)
  if(!checkAllign){
    stop("Problem with landings. All rasters must have the same same extent, number of rows and columns,
         projection, resolution, and origin.")
  }
  landings[landings>0] = 1
  doYrs = names(landings)
  cRoadsRaster = raster::brick(roads)
  ym="t0"; names(cRoadsRaster)=ym
  cRoadsRaster@history <- list(t0=NA)
  sim=list()
  #NOTE: looping over years here to speed calculations. The graph is g is only constructed once.
  for (cm in doYrs){
    print(paste("building roads year",cm))
    if(length(sim)==0){
      sim = projectRoads(landings[[cm]],cost,cRoadsRaster[[ym]],roadMethod=roadMethod,neighbourhood=neighbourhood,plotRoads=plotRoads)
    }else{
      #TO DO: how to preferentially route roads through cutblocks after the first iteration?
      sim = projectRoads(landings[[cm]],plotRoads=plotRoads,neighbourhood=neighbourhood,sim=sim)
    }
    cRoadsRaster[[cm]] = sim$roads>0 #ignoring values for now.
    cRoadsRaster@history[[length(cRoadsRaster@history)+1]] = sim$newRoads.lines
    names(cRoadsRaster@history)[-1] = cm
  }
  return(cRoadsRaster)
})
