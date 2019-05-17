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
#' ####### Use of demoScen (scenario 1) in projectRoads method
#' ## using landing points, set 4
#' landings <- subset(demoScen[[1]]$landings.points,subset=demoScen[[1]]$landings.points$set==4)
#' pr <- projectRoads(landings,demoScen[[1]]$cost.rast,demoScen[[1]]$cost.rast==0,roadMethod='mst')
#' ## using landing points, sets 3, 5, and 10
#' landings <- subset(demoScen[[1]]$landings.points,subset=demoScen[[1]]$landings.points$set%in%c(3,5,10))
#' pr <- projectRoads(landings,demoScen[[1]]$cost.rast,demoScen[[1]]$cost.rast==0,roadMethod='mst')
#' visualize(demoScen[[1]]$cost.rast,landings,pr)
#' ## using landing polygons
#' pr <- projectRoads(scen01$landings.poly,scen01$cost.rast,scen01$cost.rast==0,roadMethod='mst')
#' visualize(scen01$cost.rast,scen01$landings.poly,pr)
#' ## using landings RasterStack, sets 1 to 5  
#' pr <- projectRoads(scen01$landings.stack[[1:5]],scen01$cost.rast,scen01$cost.rast==0,roadMethod='mst')
#' visualize(scen01$cost.rast,scen01$landings.stack[[1:5]],pr)
#' ## using complete landings RasterStack
#' pr <- projectRoads(scen01$landings.stack,scen01$cost.rast,scen01$cost.rast==0,roadMethod='mst')
#' visualize(scen01$cost.rast,scen01$landings.stack,pr)
#' 
#' ####### Scenario 1
#' visualize(demoScen[[1]]$cost.rast,demoScen[[1]]$landings.points,main='demoScen[[1]], point sets')
#' visualize(demoScen[[1]]$cost.rast,demoScen[[1]]$landings.poly,main='demoScen[[1]], polygons')
#' ####### Scenario 2
#' visualize(demoScen[[2]]$cost.rast,demoScen[[2]]$landings.points,main='demoScen[[2]], point sets')
#' visualize(demoScen[[2]]$cost.rast,demoScen[[2]]$landings.poly,main='demoScen[[2]], polygons')
#' ####### Scenario 3
#' visualize(demoScen[[3]]$cost.rast,demoScen[[3]]$landings.points,main='demoScen[[3]], point sets')
#' visualize(demoScen[[3]]$cost.rast,demoScen[[3]]$landings.poly,main='demoScen[[3]], polygons')
#' ####### Scenario 4
#' visualize(demoScen[[4]]$cost.rast,demoScen[[4]]$landings.points,main='demoScen[[4]], point sets')
#' visualize(demoScen[[4]]$cost.rast,demoScen[[4]]$landings.poly,main='demoScen[[4]], polygons')
#' ####### Scenario 5
#' visualize(demoScen[[5]]$cost.rast,demoScen[[5]]$landings.points,main='demoScen[[5]], point sets')
#' visualize(demoScen[[5]]$cost.rast,demoScen[[5]]$landings.poly,main='demoScen[[5]], polygons')
#' ####### Scenario 6
#' visualize(demoScen[[6]]$cost.rast,demoScen[[6]]$landings.points,main='demoScen[[6]], point sets')
#' visualize(demoScen[[6]]$cost.rast,demoScen[[6]]$landings.poly,main='demoScen[[6]], polygons')
#' ####### Scenario 7
#' visualize(demoScen[[7]]$cost.rast,demoScen[[7]]$landings.points,main='demoScen[[7]], point sets')
#' visualize(demoScen[[7]]$cost.rast,demoScen[[7]]$landings.poly,main='demoScen[[7]], polygons')
#' ####### Scenario 8
#' visualize(demoScen[[8]]$cost.rast,demoScen[[8]]$landings.points,main='demoScen[[8]], point sets')
#' visualize(demoScen[[8]]$cost.rast,demoScen[[8]]$landings.poly,main='demoScen[[8]], polygons')
#' ####### Scenario 9
#' visualize(demoScen[[9]]$cost.rast,demoScen[[9]]$landings.points,main='demoScen[[9]], point sets')
#' visualize(demoScen[[9]]$cost.rast,demoScen[[9]]$landings.poly,main='demoScen[[9]], polygons')
#' ####### Scenario 10
#' visualize(demoScen[[10]]$cost.rast,demoScen[[10]]$landings.points,main='demoScen[[10]], point sets')
#' visualize(demoScen[[10]]$cost.rast,demoScen[[10]]$landings.poly,main='demoScen[[10]], polygons')
#' 
"demoScen"
