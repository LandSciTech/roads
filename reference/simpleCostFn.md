# Simple cost edge weight function

Calculates the weight of an edge between two nodes as the mean value of
an input cost raster at each of those nodes (`x1` and `x2`).

## Usage

``` r
simpleCostFn(x1, x2, hdistance)
```

## Arguments

- x1, x2:

  Number. Value of the input cost raster at two nodes.

- hdistance:

  Number. Horizontal distance between the nodes - for penalizing longer
  diagonal edges.

## Examples

``` r
simpleCostFn(0.5,0.7,1)
#> [1] 0.6
```
