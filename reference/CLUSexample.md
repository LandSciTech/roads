# Data from the CLUS example

From Kyle Lochhead and Tyler Muhly's CLUS road simulation example.
`SpatRaster` files created with the terra package must be saved with
[`terra::wrap()`](https://rspatial.github.io/terra/reference/wrap.html)
and need to be unwrapped before they are used.
[`prepExData()`](https://landscitech.github.io/roads/reference/prepExData.md)
does this.

## Usage

``` r
data(CLUSexample)
```

## Format

A named list with components:

- cost: a `PackedSpatRaster` representing road building cost.

- landings: an sf dataframe of points representing landing locations.

- roads: a `PackedSpatRaster` representing existing roads.

## Examples

``` r
CLUSexample
#> $cost
#> [1] "This is a PackedSpatRaster object. Use 'terra::unwrap()' to unpack it"
#> 
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
#> [1] "This is a PackedSpatRaster object. Use 'terra::unwrap()' to unpack it"
#> 
prepExData(CLUSexample)
#> $cost
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
```
