#' Prepare example data
#'
#' Prepare example data included in the package that contain wrapped terra
#' objects. This applies [terra::unwrap()] recursively to the list provided so
#' that all `PackedSpatRasters` are converted to `SpatRasters`.
#'
#' @param x list. Contains elements some of which are packed `SpatRasters`.
#'
#' @return The same list but with unwrapped `SpatRasters`
#' @export
#'
#' @examples
#' CLUSexample
#' prepExData(CLUSexample)
prepExData <- function(x){
  rapply(x, f = terra::unwrap, classes = "PackedSpatRaster", how = "replace")
}
