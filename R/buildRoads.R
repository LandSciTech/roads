#' Build roads according to roadMethod

buildRoads <- function(sim){
  switch(sim$roadMethod,
         snap= {
           sim <- buildSnapRoads(sim)
         } ,
         lcp ={
           sim <- getClosestRoad(sim)
           
           sim <- roadCLUS.lcpList(sim)
           
           # includes update graph
           sim <- roadCLUS.shortestPaths(sim)
         },
         mst ={
           sim <- getClosestRoad(sim)
           
           # will take more time than lcpList given the construction of a mst
           sim <- mstList(sim)
           
           # update graph is within the shortestPaths function
           sim <- roadCLUS.shortestPaths(sim)
           }
  )
}
