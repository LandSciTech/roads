setGeneric('projectRoadsNew', function(landings = NULL,
                                       cost = NULL,
                                       roads = NULL,
                                       roadMethod = "mst",
                                       plotRoads = T,
                                       neighbourhood = "octagon",
                                       sim = NULL)
  standardGeneric('projectRoadsNew'))

setMethod(
  'projectRoadsNew', signature(roadMethod = "mst", sim = "missing"),
  function(landings, cost, roads, roadMethod, plotRoads, 
           neighbourhood, sim) {
    
    # check required args
    missingNames = names(which(sapply(lst(roads,cost,roadMethod, landings), is.null)))
    if(length(missingNames)>0){
      stop("Argument(s): ", paste0(missingNames, collapse = ", "), " are required if sim is not supplied")
    }
    
    #set up sim list
    roads = roads > 0
    sim <- buildSimList(roads = roads, costSurface = cost, 
                        roadMethod = roadMethod, 
                        landings = landings)
    
    
  })

