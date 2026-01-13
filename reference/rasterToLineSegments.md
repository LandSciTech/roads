# Convert raster to lines

Converts rasters that represent lines into an sf object.

## Usage

``` r
rasterToLineSegments(rast, method = "mst")
```

## Arguments

- rast:

  `SpatRaster`. Raster representing lines all values \> 0 are assumed to
  be lines

- method:

  character. Method of building lines. Options are `"mst"` (default) or
  `"nearest"`. See Details below.

## Value

an sf simple feature collection

## Details

For `method = "nearest"` raster is first converted to points and then
lines are drawn between the nearest points. If there are two different
ways to connect the points that have the same distance both are kept
which can cause doubled lines. USE WITH CAUTION. `method = "mst"`
converts the raster to points, reclassifies the raster so roads are 0
and other cells are 1 and then uses `projectRoads` to connect all the
points with a minimum spanning tree. This will always connect all raster
cells and is slower but will not double lines as often. Neither method
is likely to work for very large rasters

## Examples

``` r
CLUSexample <- prepExData(CLUSexample)
# works well for very simple roads
roadLine1 <- rasterToLineSegments(CLUSexample$roads)
#> Warning: No 0s detected in weightRaster. If existing roads have not been included in the weightRaster set roadsInWeight = FALSE to have them burned in

# longer running more realistic examples
# \donttest{
demoScen <- prepExData(demoScen)
# mst method works well in this case
roadLine2 <- rasterToLineSegments(demoScen[[1]]$road.rast)
#> Warning: No 0s detected in weightRaster. If existing roads have not been included in the weightRaster set roadsInWeight = FALSE to have them burned in

# nearest method has doubled line where the two roads meet
roadLine3 <- rasterToLineSegments(demoScen[[1]]$road.rast, method = "nearest")

# The mst method can also produce odd results in some cases
roadLine4 <- rasterToLineSegments(demoScen[[4]]$road.rast)
#> Warning: No 0s detected in weightRaster. If existing roads have not been included in the weightRaster set roadsInWeight = FALSE to have them burned in

# }
```
