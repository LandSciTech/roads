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
#' Project road locations based on existing roads, planned landings, and a weights
#' raster that in conjunction with a weighting function determines the cost to
#' build a road between two cells in the raster.
#'
#' Four different methods for projecting road networks have been implemented:
#'   * "snap": Connects each landing directly to the closest road without
#'   reference to the weights or other landings.
#'   * "lcp": Least Cost Path connects each landing to the closest point
#'   on the road by determining the least cost path based on the weights raster
#'   provided, it does not consider other landings.
#'   * "ilcp": Iterative Least Cost Path, same as "lcp" but it builds each
#'   path sequentially so that later roads will use earlier roads. The sequence
#'   of landings is determined by `ordering` and is "closest" by default, the
#'   other option is "none" which will use the order that landings are supplied
#'   in.
#'   * "mst": Minimum Spanning Tree connects all landings to the road by
#'   determining the least cost path to the road or other landings based on the
#'   weight raster.
#'
#' @param landings sf polygons or points, RasterLayer, SpatialPolygons*,
#'   SpatialPoints*, matrix, containing features to be connected
#'   to the road network. Matrix should contain columns x, y with coordinates,
#'   all other columns will be ignored.
#' @param weightRaster SpatRaster or RasterLayer. weights Raster where existing
#'   roads must be the only cells with a weight of 0. If existing roads do not
#'   have 0 weight set `roadsInWeight = FALSE` and they will be burned in.
#' @param roads sf lines, SpatialLines*, RasterLayer, SpatRaster. Existing road network.
#' @param roadMethod Character. Options are "ilcp", "mst", "lcp", "snap".
#' @param plotRoads Boolean. Should the resulting road network be plotted.
#'   Default FALSE.
#' @param mainTitle Character. A title for the plot
#' @param neighbourhood Character. 'rook','queen', or 'octagon'. The cells that
#'   should be considered adjacent. 'octagon' option is a modified version of
#'   the queen's 8 cell neighbourhood in which diagonals weights are 2^0.5x
#'   higher than horizontal/vertical weights.
#' @param weightFunction function. Method for calculating the weight of an edge
#'   between two nodes from the value of the weights raster at each of those nodes
#'   (x1 and x2). Default is the mean. Functions should be symmetric, meaning
#'   that the value returned does not depend on the ordering of x1 and x2. All
#'   functions must include the arguments `x1`, `x2` and `...`.
#' @param sim list. Returned from a previous iteration of `projectRoads`.
#'   weightRaster, roads, and `roadMethod` are ignored if a `sim` list is provided.
#' @param roadsOut Character. Either "raster", "sf" or NULL. If "raster" roads
#'   are returned as a raster in the `sim` list. If "sf" the roads are returned as
#'   an sf object which will contain lines if the roads input was sf lines but a
#'   geometry collection of lines and points if the roads input was a raster.
#'   The points in the geometry collection represent the existing roads while
#'   new roads are created as lines. If NULL (default) then the returned roads
#'   are sf if the input is sf or Spatial* and raster if the input was a raster.
#' @param roadsInWeight Logical. The default is TRUE which means the
#'   `weightRaster` is assumed to include existing roads as 0. If FALSE then the
#'   roads will be "burned in" to the `weightRaster` with a weight of 0.
#' @param ordering character. The order in which roads should be built to
#'   landings when `roadMethod = "ilcp"`. Options are "closest" (default) where
#'   landings closest to existing roads are accessed first, or "none" where
#'   landings are accessed in the order they are provided in.
#' @param ... Optional additional arguments to weightFunction
#'
#' @return
#' a list with components:
#' * roads: the projected road network, including new and input roads.
#' * weightRaster: the weights raster, updated to have 0 for new roads that
#'       were added.
#' * roadMethod: the road simulation method used.
#' * landings: the landings used in the simulation.
#' * g: the graph that describes the cost of paths between each cell in the
#'       `weightRaster`. This is updated based on the new roads so that vertices
#'       that were connected by new roads now have a weight of 0. This can be used to
#'       avoid recomputing the graph in a simulation with multiple time steps.
#'
#' @examples
#' CLUSexample <- prepExData(CLUSexample)
#' doPlots <- interactive()
#'
#' projectRoads(CLUSexample$landings, CLUSexample$cost, CLUSexample$roads,
#'              "lcp", plotRoads = doPlots, mainTitle = "CLUSexample")
#'
#'
#'# More realistic examples that take longer to run
#'\donttest{
#'
#' demoScen <- prepExData(demoScen)
#'
#' ### using:  scenario 1 / sf landings / iterative least-cost path ("ilcp")
#' # demo scenario 1
#' scen <- demoScen[[1]]
#'
#' # landing set 1 of scenario 1:
#' land.pnts <- scen$landings.points[scen$landings.points$set==1,]
#'
#' prRes <- projectRoads(land.pnts, scen$cost.rast, scen$road.line, "ilcp",
#'                          plotRoads = doPlots, mainTitle = "Scen 1: SPDF-LCP")
#'
#' ### using: scenario 1 / SpatRaster landings / minimum spanning tree ("mst")
#' # demo scenario 1
#' scen <- demoScen[[1]]
#'
#' # the RasterLayer version of landing set 1 of scenario 1:
#' land.rLyr <- scen$landings.stack[[1]]
#'
#' prRes <- projectRoads(land.rLyr, scen$cost.rast, scen$road.line, "mst",
#'                          plotRoads = doPlots, mainTitle = "Scen 1: Raster-MST")
#'
#'
#' ### using: scenario 2 / matrix landings raster roads / snapping ("snap")
#' # demo scenario 2
#' scen <- demoScen[[2]]
#'
#' # landing set 5 of scenario 2, as matrix:
#' land.mat  <- scen$landings.points[scen$landings.points$set==5,] |>
#'   sf::st_coordinates()
#'
#' prRes <- projectRoads(land.mat, scen$cost.rast, scen$road.rast, "snap",
#'                       plotRoads = doPlots, mainTitle = "Scen 2: Matrix-Snap")
#'
#' ## using scenario 7 / Polygon landings raster / minimum spanning tree
#' # demo scenario 7
#' scen <- demoScen[[7]]
#' # rasterize polygonal landings of demo scenario 7:
#' land.polyR <- terra::rasterize(scen$landings.poly, scen$cost.rast)
#'
#' prRes <- projectRoads(land.polyR, scen$cost.rast, scen$road.rast, "mst",
#'                          plotRoads = doPlots, mainTitle = "Scen 7: PolyRast-MST")
#' }
#'
#' @export
#'
setGeneric('projectRoads', function(landings = NULL,
                                    weightRaster = NULL,
                                    roads = NULL,
                                    roadMethod = "ilcp",
                                    plotRoads = FALSE,
                                    mainTitle = "",
                                    neighbourhood = "octagon",
                                    weightFunction=function(x1,x2,...) (x1+x2)/2,
                                    sim = NULL,
                                    roadsOut = NULL,
                                    roadsInWeight = TRUE,
                                    ordering = "closest",
                                    ...)
  standardGeneric('projectRoads'))

#' @rdname projectRoads
setMethod(
  'projectRoads', signature(sim = "missing"),
  function(landings, weightRaster, roads, roadMethod, plotRoads, mainTitle,
           neighbourhood, weightFunction, sim, roadsOut, roadsInWeight, ordering,...) {

    # check required args
    missingNames = names(which(sapply(lst(roads, weightRaster, roadMethod, landings),
                                      is.null)))
    if(length(missingNames) > 0){
      stop("Argument(s): ", paste0(missingNames, collapse = ", "),
           " are required if sim is not supplied")
    }

    recognizedRoadMethods = c("mst", "lcp", "ilcp", "snap")

    if(roadMethod == "dlcp"){
      roadMethod <- "ilcp"
      message("roadMethod 'dlcp' has been renamed. Changing to 'ilcp' instead.")
    }

    if(!is.element(roadMethod,recognizedRoadMethods)){
      stop("Invalid road method ", roadMethod, ". Options are:",
           paste(recognizedRoadMethods, collapse=','))
    }

    # if method is not ilcp ignore ordering
    if(roadMethod != "ilcp"){
      ordering <- "none"
    }

    # If roads in are raster return as raster
    if((is(roads, "Raster") || is(roads, "SpatRaster")) && is.null(roadsOut) ){
      roadsOut <- "raster"
    } else if(is.null(roadsOut)) {
      roadsOut <- "sf"
    }

    # set up sim list
    sim <- buildSimList(roads = roads, weightRaster = weightRaster,
                        roadMethod = roadMethod,
                        landings = landings,
                        roadsInWeight = roadsInWeight)
# browser()
    sim$landingsIn <- sim$landings

    # make sure the name of the sf_column is "geometry"
    geoColInL <- attr(sim$landings, "sf_column")
    if(geoColInL != "geometry"){
      sim$landings <- rename(sim$landings, geometry = tidyselect::all_of(geoColInL))
    }

    geoColInR <- attr(sim$roads, "sf_column")
    if(geoColInR != "geometry"){
      sim$roads <- select(sim$roads, everything(), geometry = tidyselect::all_of(geoColInR))
    }

    sim$g <- getGraph(sim, neighbourhood,weightFunction=weightFunction,...)
    message("graph done")
    print(gc(full = TRUE))

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
                  ilcp ={
                    sim <- getClosestRoad(sim, ordering)
                    message("closest done")
                    print(gc(full = TRUE))

                    sim <- lcpList(sim)
                    
                    message("lcp done")
                    print(gc(full = TRUE))

                    # includes iterative update graph
                    sim <- iterativeShortestPaths(sim)
                    
                    message("short path done")
                    print(gc(full = TRUE))

                    sim <- outputRoads(sim, roadsOut)
                  },
                  mst ={
                    sim <- getClosestRoad(sim, ordering)
                    message("closest done")
                    print(gc(full = TRUE))

                    # will take more time than lcpList given the construction of
                    # a mst
                    sim <- mstList(sim)
                    message("mst done")
                    print(gc(full = TRUE))

                    # update graph is within the shortestPaths function
                    sim <- shortestPaths(sim)
                    message("short path done")
                    print(gc(full = TRUE))

                    sim <- outputRoads(sim, roadsOut)
                    message("out roads done")
                    print(gc(full = TRUE))
                    sim
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
    rm(landingsIn, envir = sim)

    if(plotRoads){
      plotRoads(sim, mainTitle)
    }

    return(as.list(sim))
  })

#' @rdname projectRoads
setMethod(
  'projectRoads', signature(sim = "list"),
  function(landings, weightRaster, roads, roadMethod, plotRoads, mainTitle,
           neighbourhood, weightFunction,sim, roadsOut, roadsInWeight, ordering,...) {

    # If roads in are raster return as raster
    if((is(sim$roads, "Raster") || is(sim$roads, "SpatRaster")) && is.null(roadsOut) ){
      roadsOut <- "raster"
    } else if(is.null(roadsOut)) {
      roadsOut <- "sf"
    }

    sim <- list2env(sim)

    # add landings to sim list. Should involve all the same checks as before
    sim <- buildSimList(sim$roads, sim$weightRaster, sim$roadMethod, landings,
                        roadsInWeight = TRUE, sim = sim)

    sim$landingsIn <- sim$landings

    # make sure the name of the sf_column is "geometry"
    geoColInL <- attr(sim$landings, "sf_column")
    if(geoColInL != "geometry"){
      sim$landings <- rename(sim$landings, geometry = tidyselect::all_of(geoColInL))
    }

    geoColInR <- attr(sim$roads, "sf_column")
    sim$roads <- select(sim$roads, everything(), geometry = tidyselect::all_of(geoColInR))

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
                  ilcp ={
                    sim <- getClosestRoad(sim, ordering)

                    sim <- lcpList(sim)

                    # includes iterative update graph
                    sim <- iterativeShortestPaths(sim)

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
    rm(landingsIn, envir = sim)

    if(plotRoads){
      plotRoads(sim, mainTitle)
    }

    return(as.list(sim))
  })

outputRoads <- function(sim, roadsOut){
  if(roadsOut == "raster"){
    sim$roads <- sim$weightRasterNew == 0
  } else {
    # make new roads
    new_roads <- pathsToLines(sim)
    # add new roads to existing
    sim$roads <- bind_rows(sim$roads, new_roads)
  }

  sim$weightRaster <- sim$weightRasterNew

  # remove no longer needed parts of list that aren't being used for update
  rm(list = c("weightRasterNew", "roads.close.XY", "paths.v", "paths.list"), 
     envir = sim)

  return(sim)
}
