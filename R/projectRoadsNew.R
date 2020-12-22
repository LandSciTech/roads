#' @include AAAClassDefinitions.R
NULL

#' Project roads
#'
#' Project road locations based on existing roads, planned landings, and a cost
#' surface that defines the cost of building roads
#' 
#' @export
#' 
setGeneric('projectRoadsNew', function(landings = NULL,
                                       cost = NULL,
                                       roads = NULL,
                                       roadMethod = "mst",
                                       plotRoads = F,
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
    
    sim <- getGraph(sim,neighbourhood)
    
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
      print({plot(sim$costSurface)
        plot(sf::st_geometry(sim$roads), add = TRUE)
        plot(sf::st_geometry(sim$landings), add = TRUE)
        title(main = mainTitle, sub = paste0("Method: ", sim$roadMethod))})
    }
    
    return(sim)
  })

