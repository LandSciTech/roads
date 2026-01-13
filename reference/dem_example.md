# Grade penalty example data

A list containing two rasters covering an area near Revelstoke, British
Columbia, Canada. `ex_elev` is elevation data and `ex_wat` is the
proportion of the cell that contains water. Both are subsets of data
downloaded with the geodata package at 30 arc seconds
resolution.`SpatRaster` files created with the terra package must be
saved with
[`terra::wrap()`](https://rspatial.github.io/terra/reference/wrap.html)
and need to be unwrapped before they are used.
[`prepExData()`](https://landscitech.github.io/roads/reference/prepExData.md)
does this.

## Usage

``` r
data(dem_example)
```

## Format

A named list with components:

- ex_elev: a `PackedSpatRaster` of elevation.

- ex_wat: a `PackedSpatRaster` of proportion water.

## Details

Elevation data are primarily from Shuttle Radar Topography Mission
(SRTM), specifically the hole-filled CGIAR-SRTM (90 m resolution) from
https://srtm.csi.cgiar.org/.

Water data are derived from the ESA WorldCover data set at 0.3-seconds
resolution. (License CC BY 4.0). See https://esa-worldcover.org/en for
more information.

## References

Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., Souverijns, N.,
Brockmann, C., Quast, R., Wevers, J., Grosu, A., Paccini, A., Vergnaud,
S., Cartus, O., Santoro, M., Fritz, S., Georgieva, I., Lesiv, M.,
Carter, S., Herold, M., Li, Linlin, Tsendbazar, N.E., Ramoino, F.,
Arino, O., 2021. ESA WorldCover 10 m 2020 v100.
doi:10.5281/zenodo.5571936.

## Examples

``` r
dem_example
#> $ex_elev
#> [1] "This is a PackedSpatRaster object. Use 'terra::unwrap()' to unpack it"
#> 
#> $ex_wat
#> [1] "This is a PackedSpatRaster object. Use 'terra::unwrap()' to unpack it"
#> 
prepExData(dem_example)
#> $ex_elev
#> class       : SpatRaster 
#> size        : 173, 305, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : -118.4917, -115.95, 49.40833, 50.85  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : CAN_elv_msk 
#> min value   :         349 
#> max value   :        3371 
#> 
#> $ex_wat
#> class       : SpatRaster 
#> size        : 173, 305, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : -118.4917, -115.95, 49.40833, 50.85  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : water 
#> min value   :     0 
#> max value   :     1 
#> 
```
