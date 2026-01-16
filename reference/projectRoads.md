# Project road network

Project a road network that links target landings to existing roads. For
all methods except `"snap"`, a `weightRaster` and `weightFunction`
together determine the cost to build a road between two adjacent raster
cells.

## Usage

``` r
projectRoads(
  landings = NULL,
  weightRaster = NULL,
  roads = NULL,
  roadMethod = "ilcp",
  plotRoads = FALSE,
  mainTitle = "",
  neighbourhood = "octagon",
  weightFunction = simpleCostFn,
  sim = NULL,
  roadsOut = NULL,
  roadsInWeight = TRUE,
  ordering = "closest",
  roadsConnected = FALSE,
  ...
)

# S4 method for class 'ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,missing'
projectRoads(
  landings = NULL,
  weightRaster = NULL,
  roads = NULL,
  roadMethod = "ilcp",
  plotRoads = FALSE,
  mainTitle = "",
  neighbourhood = "octagon",
  weightFunction = simpleCostFn,
  sim = NULL,
  roadsOut = NULL,
  roadsInWeight = TRUE,
  ordering = "closest",
  roadsConnected = FALSE,
  ...
)

# S4 method for class 'ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,list'
projectRoads(
  landings = NULL,
  weightRaster = NULL,
  roads = NULL,
  roadMethod = "ilcp",
  plotRoads = FALSE,
  mainTitle = "",
  neighbourhood = "octagon",
  weightFunction = simpleCostFn,
  sim = NULL,
  roadsOut = NULL,
  roadsInWeight = TRUE,
  ordering = "closest",
  roadsConnected = FALSE,
  ...
)
```

## Arguments

- landings:

  sf polygons or points, `RasterLayer`, `SpatialPolygons*`,
  `SpatialPoints*`, or matrix. Contains features to be connected to the
  road network. Matrix should contain columns x, y with coordinates, all
  other columns will be ignored. Polygon and raster inputs will be
  processed by
  [`getLandingsFromTarget()`](https://landscitech.github.io/roads/reference/getLandingsFromTarget.md)
  to get the centroid of harvest blocks.

- weightRaster:

  `SpatRaster` or `RasterLayer`. A `weightRaster` and `weightFunction`
  together determine the cost to build a road between two adjacent
  raster cells. For the default `weightFunction = simpleCostFn`, the
  `weightRaster` should specify the cost of construction across each
  raster cell. The value of cells that contain existing roads should be
  set to 0; if not set `roadsInWeight = FALSE` to adjust the cost of
  existing roads. To use the alternative grade penalty method, set
  `weightFunction = gradePenaltyFn`, and provide a `weightRaster` in
  which:

  - NA indicates a road cannot be built

  - Negative values are costs for crossing streams or other barriers
    that are crossable but expensive.

  - Zero values are existing roads.

  - All other values are interpreted as elevation in the units of the
    raster map (so that a difference between two cells equal to the map
    resolution can be interpreted as 100% grade).

- roads:

  sf lines, `SpatialLines*`, `RasterLayer`, `SpatRaster`. The existing
  road network.

- roadMethod:

  Character. Options are `"ilcp"`, `"mst"`, `"lcp"`, and `"snap"`. See
  Details below.

- plotRoads:

  Boolean. Should the resulting road network be plotted. Default
  `FALSE`.

- mainTitle:

  Character. A title for the plot.

- neighbourhood:

  Character. `"rook"`, `"queen"`, or `"octagon"`. Determines which cells
  are considered adjacent. The default `"octagon"` option is a modified
  version of the queen's 8 cell neighbourhood in which diagonal weights
  are multiplied by 2^0.5.

- weightFunction:

  function. Method for calculating the weight of an edge between two
  nodes from the value of the `weightRaster` at each of those nodes
  (`x1` and `x2`). The default `simpleCostFn` is the mean. The
  alternative, `gradePenaltyFn`, sets edge weights as a function of the
  difference between adjacent `weightRaster` cells to penalize steep
  grades. Users supplying their own `weightFunction` should note that it
  must be symmetric, meaning that the value returned should not depend
  on the ordering of `x1` and `x2`. The `weightFunction` must include
  arguments `x1`, `x2` and `...`.

- sim:

  list. Returned from a previous iteration of `projectRoads`.
  `weightRaster`, `roads`, and `roadMethod` are ignored if a `sim` list
  is provided.

- roadsOut:

  Character. Either `"raster"`, `"sf"` or `NULL`. If `"raster"` roads
  are returned as a `SpatRaster` in the `sim` list. If `"sf"` the roads
  are returned as an sf object which will contain lines if the roads
  input was sf lines but a geometry collection of lines and points if
  the roads input was a raster. The points in the geometry collection
  represent the existing roads while new roads are created as lines. If
  `NULL` (default) then the returned roads are `sf` if the input is `sf`
  or `Spatial*` and `SpatRaster` if the input was a raster.

- roadsInWeight:

  Logical. If `TRUE` (default) the value of existing roads in the
  `weightRaster` is assumed to be 0. If `FALSE` cells in the
  `weightRaster` that contain existing roads will be set to 0.

- ordering:

  character. The order in which landings are processed when
  `roadMethod = "ilcp"`. Options are `"closest"` (default) where
  landings closest to existing roads are accessed first, or `"none"`
  where landings are accessed in the order they are provided in.

- roadsConnected:

  Logical. Are all roads fully connected? If `TRUE` and
  `roadMethod = "mst"` the MST graph can be simplified and the
  projection should be faster. Default is `FALSE`.

- ...:

  Optional additional arguments to `weightFunction`.

## Value

a list with components:

- roads: the projected road network, including new and input roads.

- weightRaster: the updated `weightRaster` in which new and old roads
  have value 0.

- roadMethod: the road simulation method used.

- landings: the landings used in the simulation.

- g: the graph that describes the cost of paths between each cell in the
  updated `weightRaster`. Edges between vertices connected by new roads
  have weight 0. `g` can be used to avoid the cost of rebuilding the
  graph in a simulation with multiple time steps.

## Details

Four road network projection methods are:

- `"lcp"`: The Least Cost Path method connects each landing to the
  closest road with a least cost path, without reference to other
  landings.

- `"ilcp"`: The Iterative Least Cost Path method iteratively connects
  each landing to the closest road with a least cost path, so that the
  path to each successive landing can include roads constructed to
  access previous landings. The sequence of landings is determined by
  `ordering` and is "closest" by default. The alternative "none" option
  processes landings in the order supplied by the user.

- `"mst"`: The Minimum Spanning Tree method connects landings to the
  existing road with a minimum spanning tree that does not require users
  to specify the order in which landings are processed.

- `"snap"`: Connects each landing to the closest (by Euclidean distance)
  road without, reference to the weights or other landings.

## Examples

``` r
CLUSexample <- prepExData(CLUSexample)
doPlots <- interactive()

projectRoads(CLUSexample$landings, CLUSexample$cost, CLUSexample$roads,
             "lcp", plotRoads = doPlots, mainTitle = "CLUSexample")
#> 0s detected in weightRaster raster, these will be considered as existing roads
#> $landings
#> Simple feature collection with 4 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.5 ymin: 0.5 xmax: 4.5 ymax: 2.5
#> Projected CRS: NAD83 / Statistics Canada Lambert
#>          geometry
#> 1 POINT (0.5 2.5)
#> 2 POINT (2.5 2.5)
#> 3 POINT (1.5 0.5)
#> 4 POINT (4.5 0.5)
#> 
#> $roads
#> class       : SpatRaster 
#> size        : 5, 5, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 5, 0, 5  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Statistics Canada Lambert (EPSG:3347) 
#> source(s)   : memory
#> name        : lyr.1 
#> min value   : FALSE 
#> max value   :  TRUE 
#> 
#> $g
#> IGRAPH bc0fe07 U-W- 25 72 -- 
#> + attr: weight (e/n)
#> + edges from bc0fe07:
#>  [1]  1-- 2  1-- 6  1-- 7  2-- 3  2-- 6  2-- 7  2-- 8  3-- 4  3-- 7  3-- 8
#> [11]  3-- 9  4-- 5  4-- 8  4-- 9  4--10  5-- 9  5--10  6-- 7  6--11  6--12
#> [21]  7-- 8  7--11  7--12  7--13  8-- 9  8--12  8--13  8--14  9--10  9--13
#> [31]  9--14  9--15 10--14 10--15 11--12 11--16 11--17 12--13 12--16 12--17
#> [41] 12--18 13--14 13--17 13--18 13--19 14--15 14--18 14--19 14--20 15--19
#> [51] 15--20 16--17 16--21 16--22 17--18 17--21 17--22 17--23 18--19 18--22
#> [61] 18--23 18--24 19--20 19--23 19--24 19--25 20--24 20--25 21--22 22--23
#> [71] 23--24 24--25
#> 
#> $weightRaster
#> class       : SpatRaster 
#> size        : 5, 5, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 5, 0, 5  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Statistics Canada Lambert (EPSG:3347) 
#> source(s)   : memory
#> name        :    lyr.1 
#> min value   :  0.00000 
#> max value   : 19.84622 
#> 
#> $roadMethod
#> [1] "lcp"
#> 


# More realistic examples that take longer to run
# \donttest{

demoScen <- prepExData(demoScen)

### using:  scenario 1 / sf landings / iterative least-cost path ("ilcp")
# demo scenario 1
scen <- demoScen[[1]]

# landing set 1 of scenario 1:
land.pnts <- scen$landings.points[scen$landings.points$set==1,]

prRes <- projectRoads(land.pnts, scen$cost.rast, scen$road.line, "ilcp",
                         plotRoads = doPlots, mainTitle = "Scen 1: SPDF-LCP")
#> 0s detected in weightRaster raster, these will be considered as existing roads

### using: scenario 1 / `SpatRaster` landings / minimum spanning tree ("mst")
# demo scenario 1
scen <- demoScen[[1]]

# the RasterLayer version of landing set 1 of scenario 1:
land.rLyr <- scen$landings.stack[[1]]

prRes <- projectRoads(land.rLyr, scen$cost.rast, scen$road.line, "mst",
                         plotRoads = doPlots, mainTitle = "Scen 1: Raster-MST")
#> 0s detected in weightRaster raster, these will be considered as existing roads
#> harvest raster values are all in 0,1. Using patches as landing areas


### using: scenario 2 / matrix landings raster roads / snapping ("snap")
# demo scenario 2
scen <- demoScen[[2]]

# landing set 5 of scenario 2, as matrix:
land.mat  <- sf::st_coordinates(scen$landings.points[scen$landings.points$set==5,])

prRes <- projectRoads(land.mat, scen$cost.rast, scen$road.rast, "snap",
                      plotRoads = doPlots, mainTitle = "Scen 2: Matrix-Snap")
#> 0s detected in weightRaster raster, these will be considered as existing roads
#> CRS of landings supplied as a matrix is assumed to match the weightRaster

## using scenario 7 / Polygon landings raster / minimum spanning tree
# demo scenario 7
scen <- demoScen[[7]]
# rasterize polygonal landings of demo scenario 7:
land.polyR <- terra::rasterize(scen$landings.poly, scen$cost.rast)

prRes <- projectRoads(land.polyR, scen$cost.rast, scen$road.rast, "mst",
                         plotRoads = doPlots, mainTitle = "Scen 7: PolyRast-MST")
#> 0s detected in weightRaster raster, these will be considered as existing roads
#> harvest raster values are all in 0,1. Using patches as landing areas
# }
```
