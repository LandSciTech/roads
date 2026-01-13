# Demonstration set of 10 input scenarios

A demonstration set of scenarios that can be used as input to
[`projectRoads()`](https://landscitech.github.io/roads/reference/projectRoads.md).
The data contains `SpatRaster` objects that must be wrapped to be
stored. To unwrap them use
[`prepExData()`](https://landscitech.github.io/roads/reference/prepExData.md)

## Usage

``` r
data(demoScen)
```

## Format

A list of sub-lists, with each sub-list representing an input scenario.
The scenarios (sub-lists) each contain the following components:

- scen.number: An integer value representing the scenario number
  (generated scenarios are numbered incrementally from 1).

- road.rast: A logical `PackedSpatRaster` representing existing roads.
  TRUE is existing road. FALSE is not existing road.

- road.line: A sf object representing existing roads.

- cost.rast: A `PackedSpatRaster` representing the cost of developing
  new roads on a given cell.

- landings.points: A sf object representing landings sets and landing
  locations within each set. The data frame includes a field named 'set'
  which contains integer values representing the landings set that each
  point belongs to

- landings.stack: A `PackedSpatRaster` with multiple layers representing
  the landings and landings sets. Each logical layer represents one
  landings set. Values of TRUE are a landing in the given set. Values of
  FALSE are not.

- landings.poly: A sf object representing a single set of polygonal
  landings.

## See also

`projectRoads`

## Examples

``` r
demoScen[[1]]
#> $scen.number
#> [1] 1
#> 
#> $road.rast
#> [1] "This is a PackedSpatRaster object. Use 'terra::unwrap()' to unpack it"
#> 
#> $road.line
#> Simple feature collection with 4 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 0.5 ymin: 0.5 xmax: 99.5 ymax: 99.5
#> Projected CRS: NAD83 / Conus Albers
#>   ID                       geometry
#> 1  1 LINESTRING (0.5 59.5, 99.5 ...
#> 2  2 LINESTRING (0.5 12.5, 99.5 ...
#> 3  3 LINESTRING (78.5 99.5, 14.5...
#> 4  4 LINESTRING (0.5 8.5, 99.5 1...
#> 
#> $cost.rast
#> [1] "This is a PackedSpatRaster object. Use 'terra::unwrap()' to unpack it"
#> 
#> $landings.points
#> Simple feature collection with 57 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4.5 ymin: 4.5 xmax: 96.5 ymax: 99.5
#> Projected CRS: NAD83 / Conus Albers
#> First 10 features:
#>        lyr.1 set ID          geometry
#> 1   9.854914   1  1 POINT (96.5 29.5)
#> 2  10.736080   1  2 POINT (67.5 73.5)
#> 3   8.799249   2  1 POINT (55.5 97.5)
#> 4  10.610051   2  2 POINT (48.5 94.5)
#> 5  10.067412   2  3 POINT (37.5 70.5)
#> 6  11.688921   2  4 POINT (93.5 90.5)
#> 7   9.823934   2  5 POINT (50.5 44.5)
#> 8  12.055090   2  6 POINT (51.5 66.5)
#> 9  10.630008   2  7 POINT (34.5 78.5)
#> 10 11.007803   2  8   POINT (4.5 4.5)
#> 
#> $landings.stack
#> [1] "This is a PackedSpatRaster object. Use 'terra::unwrap()' to unpack it"
#> 
#> $landings.poly
#> Simple feature collection with 6 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 4.5 ymin: 29.5 xmax: 86.5 ymax: 93.5
#> Projected CRS: NAD83 / Conus Albers
#>   ID                       geometry
#> 1  1 POLYGON ((82.5 67.5, 82.5 6...
#> 2  2 POLYGON ((48.5 93.5, 48.5 8...
#> 3  3 POLYGON ((34.5 81.5, 34.5 7...
#> 4  4 POLYGON ((4.5 47.5, 4.5 29....
#> 5  5 POLYGON ((66.5 42.5, 66.5 3...
#> 6  6 POLYGON ((10.5 52.5, 10.5 4...
#> 
demoScen <- prepExData(demoScen)
demoScen[[1]]
#> $scen.number
#> [1] 1
#> 
#> $road.rast
#> class       : SpatRaster 
#> size        : 100, 100, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Conus Albers (EPSG:5070) 
#> source(s)   : memory
#> name        : lyr.1 
#> min value   : FALSE 
#> max value   :  TRUE 
#> 
#> $road.line
#> Simple feature collection with 4 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 0.5 ymin: 0.5 xmax: 99.5 ymax: 99.5
#> Projected CRS: NAD83 / Conus Albers
#>   ID                       geometry
#> 1  1 LINESTRING (0.5 59.5, 99.5 ...
#> 2  2 LINESTRING (0.5 12.5, 99.5 ...
#> 3  3 LINESTRING (78.5 99.5, 14.5...
#> 4  4 LINESTRING (0.5 8.5, 99.5 1...
#> 
#> $cost.rast
#> class       : SpatRaster 
#> size        : 100, 100, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Conus Albers (EPSG:5070) 
#> source(s)   : memory
#> name        :    lyr.1 
#> min value   :  0.00000 
#> max value   : 14.97291 
#> 
#> $landings.points
#> Simple feature collection with 57 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4.5 ymin: 4.5 xmax: 96.5 ymax: 99.5
#> Projected CRS: NAD83 / Conus Albers
#> First 10 features:
#>        lyr.1 set ID          geometry
#> 1   9.854914   1  1 POINT (96.5 29.5)
#> 2  10.736080   1  2 POINT (67.5 73.5)
#> 3   8.799249   2  1 POINT (55.5 97.5)
#> 4  10.610051   2  2 POINT (48.5 94.5)
#> 5  10.067412   2  3 POINT (37.5 70.5)
#> 6  11.688921   2  4 POINT (93.5 90.5)
#> 7   9.823934   2  5 POINT (50.5 44.5)
#> 8  12.055090   2  6 POINT (51.5 66.5)
#> 9  10.630008   2  7 POINT (34.5 78.5)
#> 10 11.007803   2  8   POINT (4.5 4.5)
#> 
#> $landings.stack
#> class       : SpatRaster 
#> size        : 100, 100, 10  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Conus Albers (EPSG:5070) 
#> source(s)   : memory
#> names       : lyr.1, lyr.1, lyr.1, lyr.1, lyr.1, lyr.1, ... 
#> min values  : FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, ... 
#> max values  :  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, ... 
#> 
#> $landings.poly
#> Simple feature collection with 6 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 4.5 ymin: 29.5 xmax: 86.5 ymax: 93.5
#> Projected CRS: NAD83 / Conus Albers
#>   ID                       geometry
#> 1  1 POLYGON ((82.5 67.5, 82.5 6...
#> 2  2 POLYGON ((48.5 93.5, 48.5 8...
#> 3  3 POLYGON ((34.5 81.5, 34.5 7...
#> 4  4 POLYGON ((4.5 47.5, 4.5 29....
#> 5  5 POLYGON ((66.5 42.5, 66.5 3...
#> 6  6 POLYGON ((10.5 52.5, 10.5 4...
#> 
```
