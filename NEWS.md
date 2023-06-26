# roads (development version)

# roads 1.1.1
* Fix an issue where updates to `terra` were causing roads to break
* In the process removed `raster` and `sp` from dependencies and converted example data to `terra` and `sf` formats. This requires a new function `prepExData` to unwrap the `terra` objects that needed to be wrapped for storage

# roads 1.1.0
* converted to using `terra` throughout the package. `raster` objects are still accepted but will be converted to `terra` formats.
* Added `getDistFromSource` function to use moving windows to quickly get a raster of the distance from the nearest source (e.g road).
* remove `SpaDES.tools` dependency because it was archived on CRAN and there is an equivalent method in `terra` now.
* The returned cost raster now includes the projected roads as having a cost of 0. This makes it easier to loop additional road building over time but is a change from the previous version where the input cost surface was returned. 

# roads 1.0.0

* Added a `NEWS.md` file to track changes to the package.
