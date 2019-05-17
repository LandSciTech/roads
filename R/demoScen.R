#' Demonstration set of 10 input scenarios
#'
#' A demonstration set of scenarios that can be used as input to \code{\link{projectRoads}} method.
#'
#' @docType data
#'
#' @usage data(demoScen)
#'
#' @format
#' A list of sub-lists, with each sub-list representing an input scenario. The scenarios (sub-lists) each contain the following components:
#' \describe{
#'   \item{scen.number}{An integer value representing the scenario number (generated scenarios are numbered incrementally from 1).}
#'   \item{road.rast}{A logical RasterLayer representing existing roads.  TRUE is existing road. FALSE is not existing road.}
#'   \item{road.line}{A SpatialLines object representing existing roads.}
#'   \item{cost.rast}{A RasterLayer representing the cost of developing new roads on a given cell.}
#'   \item{landings.points}{A SpatialPointsDataFrame representing landings sets and landing locations within each set. The data frame includes
#'   a field named 'set' which contains integer values representing the landings set that each point belongs to}
#'   \item{landings.stack}{A RasterStack representing the landings and landings sets. Each logical RasterLayer in the RasterStack represents
#'   one landings set. Values of TRUE are a landing in the given set. Values of FALSE are not.}
#'   \item{landings.poly}{A SpatialPolygonsDataFrame representing a single set of polygonal landings.}
#' }
#'
#' @examples
#'
#' library(raster) # load the raster package
#'
#' scen <- roads::demoScen[[1]] # demo scenario 1
#'
#' # landing points, set 1
#' landings <- subset(scen$landings.points,subset=scen$landings.points$set==1)
#' visualize(scen$cost.rast,scenlandings)
#'
#'
#'
#'
#' ##### roads to point landings
#' landings.1 <- scen$landings.points[scen$landings.points$set==1,] # landings set 1 of demo scenario 1
#' projRoadsResults <- roads::projectRoads(landings = landings.1,
#'                                         cost = scen$cost.rast,
#'                                         roads = scen$road.rast,
#'                                         roadMethod = "lcp") # project roads using least-cost path method
#' newRoads <- projRoadsResults$roads > 0 # get projected roads as a logical RasterLayer
#' # visualize
#' newRoads[!newRoads] <- NA
#' newRoads[scen$cost.rast==0] <- NA
#' plot(scen$cost.rast,col=c("black",colorRamps::matlab.like(255+50+50)[50:(50+255)]))
#' plot(newRoads,col="grey50",add=T,legend=F)
#' points(landings.1,pch=21,cex=2,bg="white")
#'
#' ##### roads to polygonal landings
#' landings.poly <- scen$landings.poly
#' projRoadsResults2 <- roads::projectRoads(landings = landings.poly,
#'                                          cost = scen$cost.rast,
#'                                          roads = scen$road.rast,
#'                                          roadMethod = "lcp") # project roads using least-cost path method
#' newRoads2 <- projRoadsResults2$roads > 0 # get projected roads as a logical RasterLayer
#' # visualize roads to landing points
#' newRoads2[!newRoads2] <- NA
#' newRoads2[scen$cost.rast==0] <- NA
#' plot(scen$cost.rast,col=c("black",colorRamps::matlab.like(255+50+50)[50:(50+255)]))
#' plot(newRoads2,col="grey50",add=T,legend=F)
#' plot(landings.poly,density=20,add=TRUE)
#'
"demoScen"