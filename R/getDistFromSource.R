#'Moving window approach to get distance from source
#'
#'This function is deprecated please use [terra::distance()]. Note that you need
#'to set `target = 0` to get distances from cells that are zero to cells that
#'are non-zero. 
#'
#'This function provides three different methods for calculating the distance of
#'all points on a landscape from "source" locations. This is a computationally
#'intensive process so the function arguments can be used to balance the
#'tradeoffs between speed and accuracy. Note the pfocal versions are only
#'available in the development version of the package.
#'
#'The "terra" and "pfocal" methods use an iterative moving window approach and
#'assign each cell a distance based on the number of times the moving window is
#'repeated before it is included. This means that the moving window function is
#'run many times but for a small window relative to the size of the raster. The
#'`maxDist` argument determines the maximum distance calculated and affects the
#'number of iterations of the moving window that are needed. `kwidth` is the
#'radius of the moving window in number of cells, with larger values reducing
#'the number of iterations needed but also reducing the granularity of the
#'distances produced. The resulting distances will be in increments of `kwidth`
#' * the resolution of the raster. The total number of iterations is `maxDist`/
#'`kwidth` * resolution. The only difference in these methods is the underlying
#'package used to do the moving window. The `terra` package has methods for
#'handling large rasters by writing them to disk, while the `pfocal` package
#'requires that the raster can be held in memory as a matrix.
#'
#'The third method "pfocal2" uses a global moving window to calculate the
#'distance to the source. This means that the moving window only needs to be
#'applied once but the window size can be very large. In this case `maxDist`
#'determines the total size of the window. `kwidth` can be used to reduce the
#'number of cells included in the moving window by aggregating the source raster
#'by a factor of `kwidth`. This will increase the speed of computation but will
#'produce results with artefacts of the larger grid and which may be less
#'accurate since the output raster is disaggregated using bilinear
#'interpolation.
#'
#'@param src SpatRaster or RasterLayer, where all values > 0 are treated as
#'  source locations. NA values are treated as 0s.
#'@param maxDist Numeric, maximum distance that should be calculated in units of
#'  the CRS.
#'@param kwidth Integer, for the "pfocal" and "terra" methods the width of the
#'  moving window. For the "pfocal2" method the aggregation factor.
#'@param method Character, the method to use, currently only "terra" supported
#'  with the CRAN version, while "pfocal" or "pfocal2" are available with the
#'  development version. See below for details.
#'@param override Logical, if TRUE will use the old deprecated function.
#'
#'@return A SpatRaster
#'@export
#'
#' @examples
#'
#' CLUSexample <-  prepExData(CLUSexample)
#' # Deprecated
#' # getDistFromSource(CLUSexample$roads, 5, 2)
#' 
#' # Use terra::distance instead
#' terra::distance(CLUSexample$roads, target = 0)
#'
#'\donttest{
#'  library(sf)
#'  library(terra)
#'
#' #make example roads from scratch
#' rds <- data.frame(x = 1:1000/100, y = cos(1:1000/100)) %>%
#'   st_as_sf(coords = c("x", "y")) %>%
#'   st_union() %>%
#'   st_cast("LINESTRING")
#'
#' rds_rast <- rasterize(vect(rds),
#'                       rast(nrows = 50, ncols = 50,
#'                            xmin = 0, xmax = 10,
#'                            ymin = -5, ymax = 5),
#'                       touches = TRUE)
#'
#' terra::distance(rds_rast)
#' 
#' # or straight from the line
#' terra::distance(rds_rast, terra::vect(rds %>% st_set_crs(st_crs(rds_rast))))
#'}

getDistFromSource <- function(src, maxDist, kwidth = 3, method = "terra", override = FALSE) {
  if(!override){
    .Deprecated("terra::distance")
    return(terra::distance(src, target = 0))
  }
  # Leaving this here for now
  
  # Not currently using this parameter but could in the future
  dissag = F
  
  if(is(src, "Raster")){
    src <- terra::rast(src)
  }
  if(method == "pfocal2"){
    stop("method pfocal2 is only available with the development version of roads.",
         call. = FALSE)
    # if(!rlang::is_installed("pfocal")){
    #   stop("The pfocal2 method requires the pfocal package. ",
    #        "It can be installed with remotes::install_github('LandSciTech/pfocal')",
    #        call. = FALSE)
    # }
    # src <- src > 0
    # 
    # # aggregate based on kwidth in a way that matches the moving window version
    # if(kwidth > 1){
    #   src <- terra::aggregate(src, kwidth, fun = "max", na.rm = TRUE)
    # }
    # # convert maxDist to ncells
    # maxDist <- maxDist/terra::res(src)[1]
    # 
    # # make kernal for distance from center
    # kn <- pfocal::euclidean_distance_kernel(maxDist)
    # kn <- kn*terra::res(src)[1]
    # 
    # 
    # src2 <- terra::mask(src, src, maskvalues = 0, updatevalue = NA)
    # dist_src <- pfocal::pfocal(terra::as.matrix(src2, wide = TRUE), kernel = kn, edge_value = NA, na.rm = TRUE,
    #                            reduce_function = "min")
    # dist_src <- terra::rast(src, vals = dist_src)
    # # min when all NA is Inf so need to convert to NA
    # dist_src <- terra::mask(dist_src, dist_src > (maxDist * terra::res(src2)[1]),
    #                         maskvalues = 1, updatevalue = NA)
    # 
    # if(kwidth > 1){
    #   dist_src <- terra::disagg(dist_src, fact = kwidth, method = "bilinear")
    # }
    # 
    # return(dist_src)
  }
  src <- src > 0
  if (dissag) {
    mwidth <- terra::res(src)[1]
    dd <- terra::disagg(src, fact = kwidth)
  } else {
    mwidth <- terra::res(src)[1] * kwidth
    dd <- src
  }
  mm <- uniformKernel(kwidth, useAveDist = F)
  nSteps <- ceiling(maxDist / mwidth)
  cPop <- dd
  cPop <- terra::mask(cPop, cPop, maskvalue = NA, updatevalue = 0)
  dd <- 1 - dd
  dd <- terra::mask(dd, dd, maskvalue = 0, updatevalue = NA, inverse = TRUE)
  for (s in 1:nSteps) {
    if(method == "terra"){
      ssD2 <- terra::focal(cPop, w = mm, fun = "sum", na.rm = TRUE)
      # ssD2 <- terra::init(cPop, fun = ssO2)
    } else if(method == "pfocal"){
      stop("method pfocal is only available with the development version of roads.",
           call. = FALSE)
      # if(!rlang::is_installed("pfocal")){
      #   stop("The pfocal method requires the pfocal package. ",
      #        "It can be installed with remotes::install_github('LandSciTech/pfocal')",
      #        call. = FALSE)
      # }
      # ssO2 <- pfocal::pfocal(as.matrix(cPop, wide = TRUE), mm, reduce_function = "SUM",
      #                        transform_function = "MULTIPLY")
      # ssD2 <- cPop
      # terra::values(ssD2) <- ssO2
    }
    # Add back 0s for existing roads
    ssD2 <- terra::mask(ssD2, dd, inverse = TRUE, updatevalue = 0)
    # Convert all values in window to mwidth * step number
    dd <- terra::mask(dd, ssD2 > 0, maskvalue = 1, updatevalue = s * mwidth)
    # Change to 1s for next step
    cPop <- terra::mask(cPop, ssD2 > 0, maskvalue = 1, updatevalue = 1)
  }
  if (dissag) {
    dd <- terra::aggregate(dd, fact = kwidth)
  }
  #dd <- terra::mask(dd, src)
  return(dd)
}

getDistKernelFromMax <- function(kdim) {
  # kdim=2
  kdim <- ceiling(kdim)
  wDim <- kdim * 2 + 1
  locSeq <- seq(-kdim, kdim)
  y <- matrix(locSeq, wDim, wDim)
  xx <- t(y)
  d <- (xx^2 + y^2)^0.5
  return(d)
}

uniformKernel <- function(dmax, cellDim = 1, useAveDist = F) {
  if (useAveDist) {
    # https://math.stackexchange.com/questions/3019165/average-distance-from-center-of-circle
    dmax <- 3 * dmax / 2
  }
  hdim <- ceiling(dmax / cellDim)
  weights <- getDistKernelFromMax(hdim)
  weights <- weights <= dmax / cellDim
  weights <- weights / sum(weights)
  return(weights)
}
