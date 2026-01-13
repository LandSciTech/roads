# Moving window approach to get distance from source

This function is deprecated please use
[`terra::distance()`](https://rspatial.github.io/terra/reference/distance.html).
Note that you need to set `target = 0` to get distances from cells that
are zero to cells that are non-zero.

## Usage

``` r
getDistFromSource(src, maxDist, kwidth = 3, method = "terra", override = FALSE)
```

## Arguments

- src:

  `SpatRaster` or RasterLayer, where all values \> 0 are treated as
  source locations. NA values are treated as 0s.

- maxDist:

  Numeric, maximum distance that should be calculated in units of the
  CRS.

- kwidth:

  Integer, for the "pfocal" and "terra" methods the width of the moving
  window. For the "pfocal2" method the aggregation factor.

- method:

  Character, the method to use, currently only "terra" supported with
  the CRAN version, while "pfocal" or "pfocal2" are available with the
  development version. See below for details.

- override:

  Logical, if TRUE will use the old deprecated function.

## Value

A SpatRaster

## Details

This function provides three different methods for calculating the
distance of all points on a landscape from "source" locations. This is a
computationally intensive process so the function arguments can be used
to balance the tradeoffs between speed and accuracy. Note the pfocal
versions are only available in the development version of the package.

The "terra" and "pfocal" methods use an iterative moving window approach
and assign each cell a distance based on the number of times the moving
window is repeated before it is included. This means that the moving
window function is run many times but for a small window relative to the
size of the raster. The `maxDist` argument determines the maximum
distance calculated and affects the number of iterations of the moving
window that are needed. `kwidth` is the radius of the moving window in
number of cells, with larger values reducing the number of iterations
needed but also reducing the granularity of the distances produced. The
resulting distances will be in increments of `kwidth`

- the resolution of the raster. The total number of iterations is
  `maxDist`/ `kwidth` \* resolution. The only difference in these
  methods is the underlying package used to do the moving window. The
  `terra` package has methods for handling large rasters by writing them
  to disk, while the `pfocal` package requires that the raster can be
  held in memory as a matrix.

The third method "pfocal2" uses a global moving window to calculate the
distance to the source. This means that the moving window only needs to
be applied once but the window size can be very large. In this case
`maxDist` determines the total size of the window. `kwidth` can be used
to reduce the number of cells included in the moving window by
aggregating the source raster by a factor of `kwidth`. This will
increase the speed of computation but will produce results with
artefacts of the larger grid and which may be less accurate since the
output raster is disaggregated using bilinear interpolation.

## Examples

``` r
CLUSexample <-  prepExData(CLUSexample)
# Deprecated
# getDistFromSource(CLUSexample$roads, 5, 2)

# Use terra::distance instead
terra::distance(CLUSexample$roads, target = 0)
#> class       : SpatRaster 
#> size        : 5, 5, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 5, 0, 5  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Statistics Canada Lambert (EPSG:3347) 
#> source(s)   : memory
#> name        : lyr.1 
#> min value   :     0 
#> max value   :     4 

# \donttest{
 library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
 library(terra)
#> terra 1.8.93

#make example roads from scratch
rds <- data.frame(x = 1:1000/100, y = cos(1:1000/100)) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_union() %>%
  st_cast("LINESTRING")

rds_rast <- rasterize(vect(rds),
                      rast(nrows = 50, ncols = 50,
                           xmin = 0, xmax = 10,
                           ymin = -5, ymax = 5),
                      touches = TRUE)

terra::distance(rds_rast)
#> class       : SpatRaster 
#> size        : 50, 50, 1  (nrow, ncol, nlyr)
#> resolution  : 0.2, 0.2  (x, y)
#> extent      : 0, 10, -5, 5  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84) 
#> source(s)   : memory
#> name        :    layer 
#> min value   :      0.0 
#> max value   : 549661.6 

# or straight from the line
terra::distance(rds_rast, terra::vect(rds %>% st_set_crs(st_crs(rds_rast))))
#> class       : SpatRaster 
#> size        : 50, 50, 1  (nrow, ncol, nlyr)
#> resolution  : 0.2, 0.2  (x, y)
#> extent      : 0, 10, -5, 5  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84) 
#> source(s)   : memory
#> name        :       layer 
#> min value   :    407.8533 
#> max value   : 562906.4665 
# }
```
