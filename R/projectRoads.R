# Copyright © Her Majesty the Queen in Right of Canada as represented by the
# Minister of the Environment 2021/© Sa Majesté la Reine du chef du Canada
# représentée par le ministre de l'Environnement 2021.
# 
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
# 
#       http://www.apache.org/licenses/LICENSE-2.0
# 
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.


#' Project road network
#'
#' Project road locations based on existing roads, planned landings, and a cost
#' surface that defines the cost of building roads.
#'
#' Four different methods for projecting road networks have been implemented:
#' \itemize{
#'   \item{"snap":} {Connects each landing directly to the closest road without
#'   reference to the cost or other landings}
#'   \item{"lcp":} {Least Cost Path connects each landing to the closest point
#'   on the road by determining the least cost path based on the cost surface
#'   provided, it does not consider other landings}
#'   \item{"dlcp":} {Dynamic Least Cost Path, same as "lcp" but it builds each
#'   path sequentially so that later roads will use earlier roads. The sequence
#'   of landings is determined by `ordering` and is "closest" by default, the
#'   other option is "none" which will use the order that landings are supplied
#'   in.}
#'   \item{"mst":} {Minimum Spanning Tree connects all landings to the road by
#'   determining the least cost path to the road or other landings based on the
#'   cost surface}
#' }
#' 
#' @param landings sf polygons or points, RasterLayer, SpatialPolygons*,
#'   SpatialPoints*, matrix, containing features to be connected
#'   to the road network. Matrix should contain columns x, y with coordinates,
#'   all other columns will be ignored.
#' @param cost RasterLayer. Cost surface where existing roads must be the only
#'   cells with a cost of 0. If existing roads do not have 0 cost set
#'   \code{roadsInCost = FALSE} and they will be burned in.
#' @param roads sf lines, SpatialLines*, RasterLayer. Existing road network.
#' @param roadMethod Character. Options are "mst", "dlcp", "lcp", "snap".
#' @param plotRoads Boolean. Should the resulting road network be plotted.
#'   Default FALSE.
#' @param mainTitle Character. A title for the plot
#' @param neighbourhood Character. 'rook','queen', or 'octagon'. The cells that
#'   should be considered adjacent. 'octagon' option is a modified version of
#'   the queen's 8 cell neighbourhood in which diagonals weights are 2^0.5x
#'   higher than horizontal/vertical weights.
#' @param sim list. Returned from a previous iteration of \code{projectRoads}.
#'   cost, roads, and \code{roadMethod} are ignored if a \code{sim} list is provided.
#' @param roadsOut Character. Either "raster", "sf" or NULL. If "raster" roads
#'   are returned as a raster in the \code{sim} list. If "sf" the roads are returned as
#'   an sf object which will contain lines if the roads input was sf lines but a
#'   geometry collection of lines and points if the roads input was a raster.
#'   The points in the geometry collection represent the existing roads while
#'   new roads are created as lines. If NULL (default) then the returned roads
#'   are sf if the input is sf or Spatial* and raster if the input was a raster.
#' @param roadsInCost Logical. The default is TRUE which means the cost raster
#'   is assumed to include existing roads as 0 in its cost surface. If FALSE
#'   then the roads will be "burned in" to the cost raster with a cost of 0.
#' @param ordering character. The order in which roads should be built to
#'   landings when `roadMethod = "dlcp"`. Options are "closest" (default) where
#'   landings closest to existing roads are accessed first, or "none" where
#'   landings are accessed in the order they are provided in.
#'
#' @return 
#' a list with components:
#' \itemize{
#' \item{roads: }{the projected road network, including new and input roads.}
#' \item{costSurface: }{the cost surface, updated to have 0 for new roads that
#'       were added.}
#' \item{roadMethod: }{the road simulation method used.}
#' \item{landings: }{the landings used in the simulation.}
#' \item{g: }{the graph that describes the cost of paths between each cell in the
#'       cost raster. This is updated based on the new roads so that vertices 
#'       were connected by new roads now have a cost of 0. This can be used to 
#'       avoid recomputing the graph in a simulation with multiple time steps.}
#' }
#' @examples
#' doPlots <- interactive()
#' ### using:  scenario 1 / sf landings / least-cost path ("lcp")
#' # demo scenario 1
#' scen <- demoScen[[1]]
#'
#' # landing set 1 of scenario 1:
#' land.pnts <- scen$landings.points.sf[scen$landings.points.sf$set==1,]
#'
#' prRes <- projectRoads(land.pnts, scen$cost.rast, scen$road.line.sf, "lcp",
#'                          plotRoads = doPlots, mainTitle = "Scen 1: SPDF-LCP")
#'
#' ### using: scenario 1 / RasterLayer landings / minimum spanning tree ("mst")
#' # demo scenario 1
#' scen <- demoScen[[1]]
#'
#' # the RasterLayer version of landing set 1 of scenario 1:
#' land.rLyr <- scen$landings.stack[[1]]
#'
#' prRes <- projectRoads(land.rLyr, scen$cost.rast, scen$road.line.sf, "mst",
#'                          plotRoads = doPlots, mainTitle = "Scen 1: Raster-MST")
#'
#'
#' ### using: scenario 2 / matrix landings raster roads / snapping ("snap")
#' # demo scenario 2
#' scen <- demoScen[[2]]
#'
#' # landing set 5 of scenario 2, as matrix:
#' land.mat  <- scen$landings.points[scen$landings.points$set==5,]@coords
#'
#' prRes <- projectRoads(land.mat, scen$cost.rast, scen$road.rast, "snap",
#'                          plotRoads = doPlots, mainTitle = "Scen 2: Matrix-Snap")
#'                          
#' ### using: scenario 7 / SpatialPolygonsDataFrame landings / minimum spanning tree ("mst")
#' # demo scenario 7
#' scen <- demoScen[[7]]
#'
#' # polygonal landings of demo scenario 7:
#' land.poly <- scen$landings.poly
#'
#' prRes <- projectRoads(land.poly, scen$cost.rast, scen$road.rast, "mst",
#'                          plotRoads = doPlots, mainTitle = "Scen 7: SpPoly-MST")
#'
#'# don't run to avoid examples being too long
#'\dontrun{
#' ## using scenario 7 / Polygon landings raster / minimum spanning tree
#' # demo scenario 7
#' scen <- demoScen[[7]]
#' # rasterize polygonal landings of demo scenario 7:
#' land.polyR <- raster::rasterize(scen$landings.poly, scen$cost.rast)
#'
#' prRes <- projectRoads(land.polyR, scen$cost.rast, scen$road.rast, "mst",
#'                          plotRoads = doPlots, mainTitle = "Scen 7: PolyRast-MST")
#' }
#' @import dplyr
#' @importFrom methods is as
#' @importFrom stats end na.omit
#' @importFrom rlang .data
# @importFrom raster rasterToPoints as.data.frame clump rasterize cellFromXY merge as.matrix ncell plot
#' @importFrom sp SpatialPoints Line Lines SpatialLines CRS
#' @importFrom data.table data.table := .N setDT setnames
#' 
#' @export
#' 
setGeneric('projectRoads', function(landings = NULL,
                                    cost = NULL,
                                    roads = NULL,
                                    roadMethod = "mst",
                                    plotRoads = FALSE,
                                    mainTitle = "",
                                    neighbourhood = "octagon",
                                    sim = NULL,
                                    roadsOut = NULL,
                                    roadsInCost = TRUE, 
                                    ordering = "closest")
  standardGeneric('projectRoads'))

#' @rdname projectRoads
setMethod(
  'projectRoads', signature(sim = "missing"),
  function(landings, cost, roads, roadMethod, plotRoads, mainTitle,
           neighbourhood, sim, roadsOut, roadsInCost, ordering) {
    #landings=outObj$landings;cost=outObj$cost;roads=outObj$roads;roadMethod="mst";roadsOut = "raster"
    #mainTitle = NULL;neighbourhood = "queen";sim = NULL;roadsInCost = TRUE

    # check required args
    missingNames = names(which(sapply(lst(roads, cost, roadMethod, landings),
                                      is.null)))
    if(length(missingNames) > 0){
      stop("Argument(s): ", paste0(missingNames, collapse = ", "),
           " are required if sim is not supplied")
    }

    recognizedRoadMethods = c("mst", "lcp", "dlcp", "snap")

    if(!is.element(roadMethod,recognizedRoadMethods)){
      stop("Invalid road method ", roadMethod, ". Options are:",
           paste(recognizedRoadMethods, collapse=','))
    }
    
    # if method is not dlcp ignore ordering
    if(roadMethod != "dlcp"){
      ordering <- "none"
    }

    # If roads in are raster return as raster
    if((is(roads, "Raster") || is(roads, "SpatRaster")) && is.null(roadsOut) ){
      roadsOut <- "raster"
    } else if(is.null(roadsOut)) {
      roadsOut <- "sf"
    }

    # set up sim list
    sim <- buildSimList(roads = roads, cost = cost,
                        roadMethod = roadMethod,
                        landings = landings,
                        roadsInCost = roadsInCost)
    
    sim$landingsIn <- sim$landings

    # make sure the name of the sf_column is "geometry"
    geoColInL <- attr(sim$landings, "sf_column")
    if(geoColInL != "geometry"){
      sim$landings <- rename(sim$landings, geometry = tidyselect::all_of(geoColInL))
    }

    #library(dplyr);library(sf)
    geoColInR <- attr(sim$roads, "sf_column")
    sim$roads <- select(sim$roads, geometry = tidyselect::all_of(geoColInR))

    sim <- getGraph(sim, neighbourhood)

    sim <- switch(sim$roadMethod,
                  snap= {
                    sim <- buildSnapRoads(sim, roadsOut)
                  } ,
                  lcp ={
                    sim <- getClosestRoad(sim, ordering)

                    sim <- lcpList(sim)

                    # includes update graph
                    sim <- shortestPaths(sim)
                    
                    sim <- outputRoads(sim, roadsOut)
                  },
                  dlcp ={
                    sim <- getClosestRoad(sim, ordering)
                    
                    sim <- lcpList(sim)
                    
                    # includes dynamic update graph
                    sim <- dynamicShortestPaths(sim)
                    
                    sim <- outputRoads(sim, roadsOut)
                  },
                  mst ={
                    sim <- getClosestRoad(sim, ordering)

                    # will take more time than lcpList given the construction of
                    # a mst
                    sim <- mstList(sim)

                    # update graph is within the shortestPaths function
                    sim <- shortestPaths(sim)
                    
                    sim <- outputRoads(sim, roadsOut)
                  }
    )

    # put back original geometry column names
    if(is(sim$roads, "sf")){
      if(geoColInR != attr(sim$roads, "sf_column")){
        sim$roads <- rename(sim$roads, geoColInR = .data$geometry)
      }
    }

    # reset landings to include all input landings
    sim$landings <- sim$landingsIn
    sim$landingsIn <- NULL
    
    if(plotRoads){
      plotRoads(sim, mainTitle)
    }
    
    return(sim)
  })

#' @rdname projectRoads
setMethod(
  'projectRoads', signature(sim = "list"),
  function(landings, cost, roads, roadMethod, plotRoads, mainTitle,
           neighbourhood, sim, roadsOut, roadsInCost, ordering) {

    # If roads in are raster return as raster
    if((is(sim$roads, "Raster") || is(sim$roads, "SpatRaster")) && is.null(roadsOut) ){
      roadsOut <- "raster"
    } else if(is.null(roadsOut)) {
      roadsOut <- "sf"
    }

    # add landings to sim list. Should involve all the same checks as before
    sim <- buildSimList(sim$roads, sim$cost, sim$roadMethod, landings, 
                        roadsInCost = TRUE, sim = sim)
    
    sim$landingsIn <- sim$landings
    
    # make sure the name of the sf_column is "geometry"
    geoColInL <- attr(sim$landings, "sf_column")
    if(geoColInL != "geometry"){
      sim$landings <- rename(sim$landings, geometry = tidyselect::all_of(geoColInL))
    }

    geoColInR <- attr(sim$roads, "sf_column")
    sim$roads <- select(sim$roads, geometry = tidyselect::all_of(geoColInR))

    sim <- switch(sim$roadMethod,
                  snap= {
                    sim <- buildSnapRoads(sim, roadsOut)
                  } ,
                  lcp ={
                    sim <- getClosestRoad(sim, ordering)

                    sim <- lcpList(sim)

                    # includes update graph
                    sim <- shortestPaths(sim)
                    
                    sim <- outputRoads(sim, roadsOut)
                  },
                  dlcp ={
                    sim <- getClosestRoad(sim, ordering)
                    
                    sim <- lcpList(sim)
                    
                    # includes dynamic update graph
                    sim <- dynamicShortestPaths(sim)
                    
                    sim <- outputRoads(sim, roadsOut)
                  },
                  mst ={
                    sim <- getClosestRoad(sim, ordering)

                    # will take more time than lcpList given the construction of
                    # a mst
                    sim <- mstList(sim)

                    # update graph is within the shortestPaths function
                    sim <- shortestPaths(sim)
                    
                    sim <- outputRoads(sim, roadsOut)
                  }
    )

    # put back original geometry column names
    if(geoColInL != attr(sim$landings, "sf_column")){
      sim$landings <- rename(sim$landings, geoColInL = .data$geometry)
    }

    if(geoColInR != attr(sim$roads, "sf_column")){
      sim$roads <- rename(sim$roads, geoColInR = .data$geometry)
    }

    # reset landings to include all input landings
    sim$landings <- sim$landingsIn
    sim$landingsIn <- NULL
    
    if(plotRoads){
      plotRoads(sim, mainTitle)
    }

    return(sim)
  })

outputRoads <- function(sim, roadsOut){
  if(roadsOut == "raster"){
    sim$roads <- sim$costSurfaceNew == 0
  } else {
    # make new roads
    new_roads <- pathsToLines(sim)
    # add new roads to existing
    sim$roads <- rbind(sim$roads, new_roads)
  }
  
  sim$costSurface <- sim$costSurfaceNew
  sim$costSurfaceNew <- NULL
  
  # remove no longer needed parts of list that aren't being used for update
  sim$roads.close.XY <- NULL
  sim$paths.v <- NULL
  sim$paths.list <- NULL
  
  return(sim)
}
