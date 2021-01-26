#' @include AAAClassDefinitions.R
NULL

#' Project road network
#'
#' Project road locations based on existing roads, planned landings, and a cost
#' surface that defines the cost of building roads.
#'
#' Three different methods for projecting road networks have been implemented:
#' \itemize{ \item{"snap":} {Connects each landing directly to the closest road
#' without reference to the cost or other landings} \item{"lcp":} {Least Cost
#' Path connects each landing to the closest point on the road by determining
#' the least cost path based on the cost surface provided, it does not consider
#' other landings} \item{"mst":} {Minimum Spanning Tree connects all landings to
#' the road by determining the least cost path to the road or other landings
#' based on the cost surface} }
#'
#' @param landings sf polygons or points, RasterLayer, SpatialPolygons*,
#'   SpatialPoints*, matrix, or RasterStack containing features to be connected
#'   to the road network. Matrix should contain columns x, y with coordinates,
#'   all columns will be ignored. If RasterStack assume an ordered time-series.
#' @param cost RasterLayer. Cost surface.
#' @param roads sf lines, SpatialLines*, RasterLayer. Existing road network.
#' @param roadMethod Character. Options are "mst", "lcp", "snap".
#' @param plotRoads Boolean. Should the resulting road network be ploted.
#'   Default FALSE.
#' @param mainTitle Character. A title for the plot
#' @param neighbourhood Character. 'rook','queen', or 'octagon'. The cells that
#'   should be considered adjacent. 'octagon' option is a modified version of
#'   the queen's 8 cell neighbourhood in which diagonals weights are 2^0.5x
#'   higher than horizontal/vertical weights.
#' @param sim Sim list. Returned from a previous iteration of projectRoads.
#'   cost, roads, and roadMethod are ignored if a sim list is provided.
#'
#' @examples
#' ### using:  scenario 1 / SpatialPointsDataFrame landings / least-cost path ("lcp")
#' # demo scenario 1
#' scen <- demoScen[[1]]
#' 
#' # landing set 1 of scenario 1:
#' land.pnts <- scen$landings.points[scen$landings.points$set==1,]
#' 
#' prRes <- projectRoadsNew(land.pnts ,scen$cost.rast, scen$road.rast, "lcp",
#'                          plotRoads = TRUE, mainTitle = "Scen 1: SPDF-LCP")
#' 
#' ### using: scenario 1 / RasterLayer landings / minimum spanning tree ("mst")
#' # demo scenario 1
#' scen <- demoScen[[1]] 
#'
#' # the RasterLayer version of landing set 1 of scenario 1:
#' land.rLyr <- scen$landings.stack[[1]]
#' 
#' prRes <- projectRoadsNew(land.rLyr, scen$cost.rast, scen$road.rast, "mst",
#'                          plotRoads = TRUE, mainTitle = "Scen 1: Raster-MST")
#' 
#' 
#' ### using: scenario 2 / matrix landings / snapping ("snap")
#' # demo scenario 2
#' scen <- demoScen[[2]] 
#' 
#' # landing set 5 of scenario 2, as matrix:
#' land.mat  <- scen$landings.points[scen$landings.points$set==5,]@coords
#' 
#' prRes <- projectRoadsNew(land.mat, scen$cost.rast, scen$road.rast, "snap",
#'                          plotRoads = TRUE, mainTitle = "Scen 2: Matrix-Snap")
#' 
#' # TODO: Make this and a list of sf objects, (or maybe a year column?) work
#' # ### using: scenario 3 / RasterStack landings / minimum spanning tree ("mst")
#' # # demo scenario 3
#' # scen <- demoScen[[3]] 
#' # 
#' # # landing sets 1 to 4 of scenario 3, as RasterStack:
#' # land.rstack <- scen$landings.stack[[1:4]]
#' # 
#' # prRes <- projectRoadsNew(land.rstack, scen$cost.rast, scen$road.rast ,"mst",
#' #                          plotRoads = TRUE, mainTitle = "Scen 3: RasterStack-MST")
#' 
#' 
#' ### using: scenario 7 / SpatialPolygonsDataFrame landings / minimum spanning tree ("mst")
#' # demo scenario 7
#' scen <- demoScen[[7]]
#' 
#' # polygonal landings of demo scenario 7:
#' land.poly <- scen$landings.poly
#' 
#' prRes <- projectRoadsNew(land.poly, scen$cost.rast, scen$road.rast, "mst",
#'                          plotRoads = TRUE, mainTitle = "Scen 7: SpPoly-MST")
#'                          

### using scenario 7 / Polygon landings raster / minimum spanning tree
# # demo scenario 7
# scen <- demoScen[[7]]
# # rasterize polygonal landings of demo scenario 7:
# land.polyR <- raster::rasterize(scen$landings.poly, scen$cost.rast)
# 
# prRes <- projectRoadsNew(land.polyR, scen$cost.rast, scen$road.rast, "mst",
#                          plotRoads = TRUE, mainTitle = "Scen 7: PolyRast-MST")

#'                        
#' @export
#' 
setGeneric('projectRoadsNew', function(landings = NULL,
                                       cost = NULL,
                                       roads = NULL,
                                       roadMethod = "mst",
                                       plotRoads = FALSE,
                                       mainTitle = NULL,
                                       neighbourhood = "octagon",
                                       sim = NULL)
  standardGeneric('projectRoadsNew'))

setMethod(
  'projectRoadsNew', signature(sim = "missing"),
  function(landings, cost, roads, roadMethod, plotRoads, mainTitle,
           neighbourhood, sim) {
    
    # check required args
    missingNames = names(which(sapply(lst(roads, cost, roadMethod, landings), 
                                      is.null)))
    if(length(missingNames) > 0){
      stop("Argument(s): ", paste0(missingNames, collapse = ", "),
           " are required if sim is not supplied")
    }
    
    recognizedRoadMethods = c("mst", "lcp", "snap")
    
    if(!is.element(roadMethod,recognizedRoadMethods)){
      stop("Invalid road method ", roadMethod, ". Options are:", 
           paste(recognizedRoadMethods, collapse=','))
    }

    # set up sim list
    #roads = roads > 0
    sim <- buildSimList(roads = roads, cost = cost, 
                        roadMethod = roadMethod, 
                        landings = landings)
    
    # make sure the name of the sf_column is "geometry" 
    geoColInL <- attr(sim$landings, "sf_column")
    if(geoColInL != "geometry"){
      sim$landings <- rename(sim$landings, geometry = all_of(geoColInL))
    }
    
    geoColInR <- attr(sim$roads, "sf_column")
    sim$roads <- select(sim$roads, geometry = all_of(geoColInR))
    
    sim <- getGraph(sim, neighbourhood)
    
    sim <- switch(sim$roadMethod,
                  snap= {
                    sim <- buildSnapRoads(sim)
                  } ,
                  lcp ={
                    sim <- getClosestRoad(sim)
                    
                    sim <- lcpList(sim)
                    
                    # includes update graph
                    sim <- shortestPaths(sim)
                  },
                  mst ={
                    sim <- getClosestRoad(sim)
                    
                    # will take more time than lcpList given the construction of
                    # a mst
                    sim <- mstList(sim)
                    
                    # update graph is within the shortestPaths function
                    sim <- shortestPaths(sim)
                  }
    )
    
    # sim <- new("SimList", sim)
    
    if(plotRoads){
      print({raster::plot(sim$costSurface)
        plot(sf::st_geometry(sim$roads), add = TRUE)
        plot(sf::st_geometry(sim$landings), add = TRUE)
        if(is(landings, "SpatialPolygons")){
          sp::plot(landings, add = TRUE)
        } else if(is(landings, "sf") && 
                  sf::st_geometry_type(landings, by_geometry = FALSE) %in%
                  c("POLYGON", "MULTIPOLYGON")){
          plot(landings, add = TRUE)
        }
        title(main = mainTitle, sub = paste0("Method: ", sim$roadMethod))})
    }
    
    # put back original geometry column names
    if(geoColInL != attr(sim$landings, "sf_column")){
      sim$landings <- rename(sim$landings, geoColInL = geometry)
    }

    if(geoColInR != attr(sim$roads, "sf_column")){
      sim$roads <- rename(sim$roads, geoColInR = geometry)
    }
    
    return(sim)
  })

