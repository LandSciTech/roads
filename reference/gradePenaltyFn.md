# Grade penalty edge weight function

Method for calculating the weight of an edge between two nodes from the
value of the input raster at each of those nodes (`x1` and `x2`),
designed for a single DEM input. The method assumes an input
`weightRaster` in which:

- `NA` indicates a road cannot be built

- Negative values are costs for crossing streams or other barriers that
  are crossable but expensive. Edges that link to barrier penalty
  (negative value) nodes are assigned the largest barrier penalty
  weight.

- Zero values are assumed to be existing roads.

- All other values are interpreted as elevation in the units of the
  raster map (so that a difference between two cells equal to the map
  resolution can be interpreted as 100% grade) This is a simplified
  version of the grade penalty approach taken by Anderson and Nelson
  (2004): The approach does not distinguish between adverse and
  favourable grades. Default construction cost values are from the BC
  interior appraisal manual. The approach ignores (unknown) grade
  penalties beside roads and barriers in order to avoid increased memory
  and computational burden associated with multiple input rasters.

## Usage

``` r
gradePenaltyFn(
  x1,
  x2,
  hdistance,
  baseCost = 16178,
  limit = 20,
  penalty = 504,
  limitWeight = NA
)
```

## Arguments

- x1, x2:

  Number. Value of the input raster at two nodes.

- hdistance:

  Number. Horizontal distance between nodes. `hdistance`, `x1`, and `x2`
  should have the same units.

- baseCost:

  Number. Construction cost of 0% grade road per km.

- limit:

  Number. Maximum grade (%) on which roads can be built.

- penalty:

  Number. Cost increase (per km) associated with each additional %
  increase in road grade.

- limitWeight:

  Number. Value assigned to edges that exceed the grade limit. Try
  setting to a high (not `NA`) value if encountering problems with
  disconnected graphs.

## References

Anderson AE, Nelson J (2004) Projecting vector-based road networks with
a shortest path algorithm. Canadian Journal of Forest Research
34:1444–1457. https://doi.org/10.1139/x04-030

## Examples

``` r
gradePenaltyFn(0.5,0.51,1)
#> [1] 16682
gradePenaltyFn(0.5,0.65,1)
#> [1] 23738
# grade > 20% so NA
gradePenaltyFn(0.5,0.75,1)
#> [1] NA
```
