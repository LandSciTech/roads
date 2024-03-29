% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projectRoads.R
\name{projectRoads}
\alias{projectRoads}
\alias{projectRoads,ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,missing-method}
\alias{projectRoads,ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,list-method}
\title{Project road network}
\usage{
projectRoads(
  landings = NULL,
  weightRaster = NULL,
  roads = NULL,
  roadMethod = "ilcp",
  plotRoads = FALSE,
  mainTitle = "",
  neighbourhood = "octagon",
  weightFunction = function(x1, x2, ...) (x1 + x2)/2,
  sim = NULL,
  roadsOut = NULL,
  roadsInWeight = TRUE,
  ordering = "closest",
  ...
)

\S4method{projectRoads}{ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,missing}(
  landings = NULL,
  weightRaster = NULL,
  roads = NULL,
  roadMethod = "ilcp",
  plotRoads = FALSE,
  mainTitle = "",
  neighbourhood = "octagon",
  weightFunction = function(x1, x2, ...) (x1 + x2)/2,
  sim = NULL,
  roadsOut = NULL,
  roadsInWeight = TRUE,
  ordering = "closest",
  ...
)

\S4method{projectRoads}{ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,list}(
  landings = NULL,
  weightRaster = NULL,
  roads = NULL,
  roadMethod = "ilcp",
  plotRoads = FALSE,
  mainTitle = "",
  neighbourhood = "octagon",
  weightFunction = function(x1, x2, ...) (x1 + x2)/2,
  sim = NULL,
  roadsOut = NULL,
  roadsInWeight = TRUE,
  ordering = "closest",
  ...
)
}
\arguments{
\item{landings}{sf polygons or points, RasterLayer, SpatialPolygons*,
SpatialPoints*, matrix, containing features to be connected
to the road network. Matrix should contain columns x, y with coordinates,
all other columns will be ignored.}

\item{weightRaster}{SpatRaster or RasterLayer. weights Raster where existing
roads must be the only cells with a weight of 0. If existing roads do not
have 0 weight set \code{roadsInWeight = FALSE} and they will be burned in.}

\item{roads}{sf lines, SpatialLines*, RasterLayer, SpatRaster. Existing road network.}

\item{roadMethod}{Character. Options are "ilcp", "mst", "lcp", "snap".}

\item{plotRoads}{Boolean. Should the resulting road network be plotted.
Default FALSE.}

\item{mainTitle}{Character. A title for the plot}

\item{neighbourhood}{Character. 'rook','queen', or 'octagon'. The cells that
should be considered adjacent. 'octagon' option is a modified version of
the queen's 8 cell neighbourhood in which diagonals weights are 2^0.5x
higher than horizontal/vertical weights.}

\item{weightFunction}{function. Method for calculating the weight of an edge
between two nodes from the value of the weights raster at each of those nodes
(x1 and x2). Default is the mean. Functions should be symmetric, meaning
that the value returned does not depend on the ordering of x1 and x2. All
functions must include the arguments \code{x1}, \code{x2} and \code{...}.}

\item{sim}{list. Returned from a previous iteration of \code{projectRoads}.
weightRaster, roads, and \code{roadMethod} are ignored if a \code{sim} list is provided.}

\item{roadsOut}{Character. Either "raster", "sf" or NULL. If "raster" roads
are returned as a raster in the \code{sim} list. If "sf" the roads are returned as
an sf object which will contain lines if the roads input was sf lines but a
geometry collection of lines and points if the roads input was a raster.
The points in the geometry collection represent the existing roads while
new roads are created as lines. If NULL (default) then the returned roads
are sf if the input is sf or Spatial* and raster if the input was a raster.}

\item{roadsInWeight}{Logical. The default is TRUE which means the
\code{weightRaster} is assumed to include existing roads as 0. If FALSE then the
roads will be "burned in" to the \code{weightRaster} with a weight of 0.}

\item{ordering}{character. The order in which roads should be built to
landings when \code{roadMethod = "ilcp"}. Options are "closest" (default) where
landings closest to existing roads are accessed first, or "none" where
landings are accessed in the order they are provided in.}

\item{...}{Optional additional arguments to weightFunction}
}
\value{
a list with components:
\itemize{
\item roads: the projected road network, including new and input roads.
\item weightRaster: the weights raster, updated to have 0 for new roads that
were added.
\item roadMethod: the road simulation method used.
\item landings: the landings used in the simulation.
\item g: the graph that describes the cost of paths between each cell in the
\code{weightRaster}. This is updated based on the new roads so that vertices
that were connected by new roads now have a weight of 0. This can be used to
avoid recomputing the graph in a simulation with multiple time steps.
}
}
\description{
Project road locations based on existing roads, planned landings, and a weights
raster that in conjunction with a weighting function determines the cost to
build a road between two cells in the raster.
}
\details{
Four different methods for projecting road networks have been implemented:
\itemize{
\item "snap": Connects each landing directly to the closest road without
reference to the weights or other landings.
\item "lcp": Least Cost Path connects each landing to the closest point
on the road by determining the least cost path based on the weights raster
provided, it does not consider other landings.
\item "ilcp": Iterative Least Cost Path, same as "lcp" but it builds each
path sequentially so that later roads will use earlier roads. The sequence
of landings is determined by \code{ordering} and is "closest" by default, the
other option is "none" which will use the order that landings are supplied
in.
\item "mst": Minimum Spanning Tree connects all landings to the road by
determining the least cost path to the road or other landings based on the
weight raster.
}
}
\examples{
CLUSexample <- prepExData(CLUSexample)
doPlots <- interactive()

projectRoads(CLUSexample$landings, CLUSexample$cost, CLUSexample$roads,
             "lcp", plotRoads = doPlots, mainTitle = "CLUSexample")


# More realistic examples that take longer to run
\donttest{

demoScen <- prepExData(demoScen)

### using:  scenario 1 / sf landings / iterative least-cost path ("ilcp")
# demo scenario 1
scen <- demoScen[[1]]

# landing set 1 of scenario 1:
land.pnts <- scen$landings.points[scen$landings.points$set==1,]

prRes <- projectRoads(land.pnts, scen$cost.rast, scen$road.line, "ilcp",
                         plotRoads = doPlots, mainTitle = "Scen 1: SPDF-LCP")

### using: scenario 1 / SpatRaster landings / minimum spanning tree ("mst")
# demo scenario 1
scen <- demoScen[[1]]

# the RasterLayer version of landing set 1 of scenario 1:
land.rLyr <- scen$landings.stack[[1]]

prRes <- projectRoads(land.rLyr, scen$cost.rast, scen$road.line, "mst",
                         plotRoads = doPlots, mainTitle = "Scen 1: Raster-MST")


### using: scenario 2 / matrix landings raster roads / snapping ("snap")
# demo scenario 2
scen <- demoScen[[2]]

# landing set 5 of scenario 2, as matrix:
land.mat  <- scen$landings.points[scen$landings.points$set==5,] |>
  sf::st_coordinates()

prRes <- projectRoads(land.mat, scen$cost.rast, scen$road.rast, "snap",
                      plotRoads = doPlots, mainTitle = "Scen 2: Matrix-Snap")

## using scenario 7 / Polygon landings raster / minimum spanning tree
# demo scenario 7
scen <- demoScen[[7]]
# rasterize polygonal landings of demo scenario 7:
land.polyR <- terra::rasterize(scen$landings.poly, scen$cost.rast)

prRes <- projectRoads(land.polyR, scen$cost.rast, scen$road.rast, "mst",
                         plotRoads = doPlots, mainTitle = "Scen 7: PolyRast-MST")
}

}
