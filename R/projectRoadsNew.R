setGeneric('projectRoadsNew', function(landings = NULL,
                                       cost = NULL,
                                       roads = NULL,
                                       roadMethod = "mst",
                                       plotRoads = T,
                                       neighbourhood = "octagon",
                                       sim = NULL)
  standardGeneric('projectRoadsNew'))

setMethod(
  'projectRoadsNew', signature(sim = "missing"),
  function(landings, cost, roads, roadMethod, plotRoads, 
           neighbourhood, sim) {
    
    # check required args
    missingNames = names(which(sapply(lst(roads,cost,roadMethod, landings), is.null)))
    if(length(missingNames)>0){
      stop("Argument(s): ", paste0(missingNames, collapse = ", "), " are required if sim is not supplied")
    }
    
    recognizedRoadMethods = c("mst","lcp","snap")
    
    if(!is.element(roadMethod,recognizedRoadMethods)){
      stop("Invalid road method ",roadMethod,". Options are:",paste(recognizedRoadMethods,collapse=','))
    }
    
    # set up sim list
    #roads = roads > 0
    sim <- buildSimList(roads = roads, cost = cost, 
                        roadMethod = roadMethod, 
                        landings = landings)
    
    sim <- roadCLUS.getGraph(sim,neighbourhood)
    
    sim <- buildRoads(sim)
    
    if(plotRoads){
      
      # convert paths.v roads into raster of roads (doesn't do anything for snap) 
      sim <- roadCLUS.analysis(sim)
      
    }
  })

