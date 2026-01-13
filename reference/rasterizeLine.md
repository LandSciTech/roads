# Faster rasterize for lines

Rasterize a line using `stars` because `fasterize` doesn't work on lines
and rasterize is slow. Deprecated use
[`terra::rasterize`](https://rspatial.github.io/terra/reference/rasterize.html)

## Usage

``` r
rasterizeLine(sfLine, rast, value)
```

## Arguments

- sfLine:

  an sf object to be rasterized

- rast:

  a raster to use as template for the output raster

- value:

  a number value to give the background ie 0 or NA

## Value

a RasterLayer where the value of cells that touch the line will be the
row index of the line in the sf

## Examples

``` r
CLUSexample <- prepExData(CLUSexample)
roadsLine <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(
matrix(c(0.5, 4.5, 4.5, 4.51),
       ncol = 2, byrow = TRUE) 
)))

# Deprecated rasterizeLine(roadsLine, CLUSexample$cost, 0)   
# Use terra::rasterize
terra::rasterize(roadsLine, CLUSexample$cost, background = 0)
#> class       : SpatRaster 
#> size        : 5, 5, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 5, 0, 5  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Statistics Canada Lambert (EPSG:3347) 
#> source(s)   : memory
#> name        : layer 
#> min value   :     0 
#> max value   :     1 
```
