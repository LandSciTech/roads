# roads: Road Network Projection

Iterative least cost path and minimum spanning tree methods for
projecting forest road networks. The methods connect a set of target
points to an existing road network using 'igraph' <https://igraph.org>
to identify least cost routes. The cost of constructing a road segment
between adjacent pixels is determined by a user supplied weight raster
and a weight function; options include the average of adjacent weight
raster values, and a function of the elevation differences between
adjacent cells that penalizes steep grades. These road network
projection methods are intended for integration into R workflows and
modelling frameworks used for forecasting forest change, and can be
applied over multiple time-steps without rebuilding a graph at each
time-step.

## See also

Useful links:

- <https://github.com/LandSciTech/roads>

- <https://landscitech.github.io/roads/>

- Report bugs at <https://github.com/LandSciTech/roads/issues>

## Author

**Maintainer**: Josie Hughes <josie.hughes@ec.gc.ca>

Authors:

- Sarah Endicott <sarah.endicott@ec.gc.ca>
  ([ORCID](https://orcid.org/0000-0001-9644-5343))

- Kyle Lochhead <Kyle.Lochhead@gov.bc.ca>

- Patrick Kirby

Other contributors:

- Her Majesty the Queen in Right of Canada as represented by the
  Minister of the Environment (Copyright holder for included functions
  buildSimList, getLandingsFromTarget, pathsToLines, plotRoads,
  projectRoads, rasterizeLine, rasterToLineSegments) \[copyright
  holder\]

- Province of British Columbia (Copyright holder for included functions
  getGraph, lcpList, mstList, shortestPaths, getClosestRoad,
  buildSnapRoads) \[copyright holder\]
