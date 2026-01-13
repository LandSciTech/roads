# Plot projected roads

Plot the results of
[`projectRoads()`](https://landscitech.github.io/roads/reference/projectRoads.md)

## Usage

``` r
plotRoads(sim, mainTitle, subTitle = paste0("Method: ", sim$roadMethod), ...)
```

## Arguments

- sim:

  `sim` list result from `projectRoads`

- mainTitle:

  character. A title for the plot

- subTitle:

  character. A sub title for the plot, by default the `roadMethod` is
  used

- ...:

  Other arguments passed to raster plot call for the `weightRaster`

## Value

Creates a plot using base graphics

## Examples

``` r
CLUSexample <- prepExData(CLUSexample)
prRes <- projectRoads(CLUSexample$landings, CLUSexample$cost, CLUSexample$roads)
#> 0s detected in weightRaster raster, these will be considered as existing roads
if(interactive()){
  plotRoads(prRes, "Title")
}

```
