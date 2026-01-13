# Get landing target points within harvest blocks

Generate landing points inside polygons representing harvested area.
There are three different sampling types available: `"centroid"`
(default) returns the centroid or a point inside the polygon if the
centroid is not (see
[`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html));
`"random"` returns a random sample given `landingDens` see
([`sf::st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.html));
`"regular"` returns points on a regular grid with cell size
`sqrt(1/landingDens)` that intersect the polygon, or centroid if no grid
points fall within the polygon.

## Usage

``` r
getLandingsFromTarget(harvest, landingDens = NULL, sampleType = "centroid")
```

## Arguments

- harvest:

  `sf`, `SpatialPolygons`, `SpatRaster` or `RasterLayer` object with
  harvested areas. If it is a raster with values outside 0,1, values are
  assumed to be harvest block IDs. If raster values are in 0,1 they are
  assumed to be a binary raster and
  [terra::patches](https://rspatial.github.io/terra/reference/patches.html)
  is used to identify harvest blocks.

- landingDens:

  number of landings per unit area. This should be in the same units as
  the CRS of the harvest. Note that 0.001 points per m2 is \> 1000
  points per km2 so this number is usually very small for projected CRS.

- sampleType:

  character. `"centroid"` (default), `"regular"` or `"random"`.
  `"centroid"` returns one landing per harvest block, which is
  guaranteed to be in the harvest block for sf objects but not for
  rasters. `"regular"` returns points from a grid with density
  `landingDens` that overlap the harvested areas. `"random"` returns a
  random set of points from each polygon determined by the area of the
  polygon and `landingDens`. If `harvest` is a raster set of landings
  always includes the centroid to ensure at least one landing for each
  harvest block.

## Value

an sf simple feature collection with an `ID` column and `POINT` geometry

## Details

Note that the `landingDens` is points per unit area where the unit of
area is determined by the CRS. For projected CRS this should likely be a
very small number i.e. \< 0.001.

## Examples

``` r
doPlots <- interactive()
demoScen <- prepExData(demoScen)

polys <- demoScen[[1]]$landings.poly[1:2,]

# Get centroid
outCent <- getLandingsFromTarget(polys)
#> Warning: st_point_on_surface assumes attributes are constant over geometries

if(doPlots){
  plot(sf::st_geometry(polys))
  plot(outCent, col = "red", add = TRUE)
}

# Get random sample with density 0.1 points per unit area
outRand <- getLandingsFromTarget(polys, 0.1, sampleType = "random")
#> you have asked for > 0.001 pts per m2 which is > 1000 pts per km2 and may take a long time

if(doPlots){
  plot(sf::st_geometry(polys))
  plot(outRand, col = "red", add = TRUE)
}

# Get regular sample with density 0.1 points per unit area
outReg <- getLandingsFromTarget(polys, 0.1, sampleType = "regular")
#> you have asked for > 0.001 pts per m2 which is > 1000 pts per km2 and may take a long time

if(doPlots){
  plot(sf::st_geometry(polys))
  plot(outReg, col = "red", add = TRUE)
}
```
