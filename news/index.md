# Changelog

## roads (development version)

## roads 1.2.0

CRAN release: 2024-06-27

- Change dynamic least cost paths (DLCP) to iterative least cost paths
  (ILCP) throughout
- Change default `roadMethod` to `"ilcp"` in `projectRoads`
- Add ability to use a custom `weightFunction` and add a
  `weightFunction` `gradePenaltyFn` that determines the grade between
  two cells
- Add a vignette and a demo data set `dem_example` to show how
  `gradePenaltyFn` can be used.
- Change argument name from `cost` to `weightRaster` since it no longer
  represents a cost surface and can now be inputs to the
  `weightFunction`. Also change `roadsInCost` to `roadsInWeight`.
- returned roads are no longer unioned together.
- Deprecate `getDistFromSource` and use
  [`terra::distance`](https://rspatial.github.io/terra/reference/distance.html)
  instead.
- Fix bug in `getLandingsFromTarget` and change so that patches are used
  for 0,1 rasters and ids are used otherwise using terra::as.polygons to
  make it faster.

## roads 1.1.1

CRAN release: 2023-06-24

- Fix an issue where updates to `terra` were causing roads to break
- In the process removed `raster` and `sp` from dependencies and
  converted example data to `terra` and `sf` formats. This requires a
  new function `prepExData` to unwrap the `terra` objects that needed to
  be wrapped for storage

## roads 1.1.0

CRAN release: 2023-02-02

- converted to using `terra` throughout the package. `raster` objects
  are still accepted but will be converted to `terra` formats.
- Added `getDistFromSource` function to use moving windows to quickly
  get a raster of the distance from the nearest source (e.g road).
- remove `SpaDES.tools` dependency because it was archived on CRAN and
  there is an equivalent method in `terra` now.
- The returned cost raster now includes the projected roads as having a
  cost of 0. This makes it easier to loop additional road building over
  time but is a change from the previous version where the input cost
  surface was returned.

## roads 1.0.0

CRAN release: 2022-06-22

- Added a `NEWS.md` file to track changes to the package.
