#' Build roads according to roadMethod


setGeneric("buildRoads", function(sim, roadMethod = "mst")
  standardGeneric("buildRoads"))

setMethod(
  "buildRoads", signature(roadMethod = "mst"),
  function(sim, roadMethod) {
    
    sim <- roadCLUS.getClosestRoad(sim)
    
    # will take more time than lcpList given the construction of a mst
    sim <- roadCLUS.mstList(sim)
    
    # update graph is within the shortestPaths function
    sim <- roadCLUS.shortestPaths(sim)
    
  }
)
    
setMethod(
  "buildRoads", signature(roadMethod = "lcp"),
  function(sim, roadMethod) {
    
    sim <- roadCLUS.getClosestRoad(sim)
    
    sim <- roadCLUS.lcpList(sim)
    
    # includes update graph
    sim <- roadCLUS.shortestPaths(sim)
    
  }
)

setMethod(
  "buildRoads", signature(roadMethod = "snap"),
  function(sim, roadMethod) {
    # TODO: update build snap roads to use st_nearest_point directly
    sim <- roadCLUS.buildSnapRoads(sim)
    
  }
)

