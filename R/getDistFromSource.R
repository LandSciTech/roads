
#Using fast parallel moving window approach to get distance from roads Note that
#pfocal package requires R version 4 or higher. Will consider with Sarah putting
#this function in the roads package. For now, here it is.

#The function uses pfocal, which is a fast parallel moving window function we
#have been developing for another project. In order to install the pfocal
#package, you will need to make sure you have a current version of R installed.
#If you run into problems installing (and you may) let us know. Now is a good
#time for us to find and fix problems with that package.


# 


#' Moving window approach to get distance from roads
#' 
#' There is a tradeoff between speed and accuracy in this method, controlled by
#' the parameters kwidth and dissag. kwidth is the radius of the moving window in
#' number of cells. For very small values (e.g. 1,2) we get grid artefacts in the
#' results. Larger moving windows are more circular which reduces the grid
#' artefacts. If dissag is false, the resulting distance map will have values of
#' 0, 1kwidth, 2kwidth, etc. If dissag is true the algorithm disaggregates the
#' raster to a finer resolution, so the resulting distance map will have values of
#' 0,1,2, etc, but there is considerable computational cost associated with the
#' higher accuracy.
#'
#' @param src 
#' @param maxDist 
#' @param kwidth 
#' @param dissag 
#'
#' @return
#' @export
#'
#' @examples
getDistFromSource <- function(src, maxDist, kwidth = 3, dissag = F) {
  if(is(src, "Raster")){
    src <- terra::rast(src)
  }
  src[src > 0] <- 1
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
  cPop[is.na(cPop)] <- 0
  dd <- 1 - dd
  dd[dd != 0] <- NA
  for (s in 1:nSteps) {
    ssO2 <- pfocal::pfocal(as.matrix(cPop, wide = TRUE), mm, reduce_function = "SUM",
                           transform_function = "MULTIPLY")
    ssD2 <- cPop
    terra::values(ssD2) <- ssO2
    dd[is.na(dd) & (ssD2 > 0)] <- s * mwidth
    cPop <- ssD2
    cPop[cPop > 0] <- 1
  }
  if (dissag) {
    dd <- terra::aggregate(dd, fact = kwidth)
  }
  dd[is.na(src)] <- NA
  return(dd)
}

getDistFromSource2 <- function(src, maxDist, kwidth = 3, dissag = F) {
  if(is(src, "Raster")){
    src <- terra::rast(src)
  }
  src[src > 0] <- 1
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
    ssD2 <- terra::focal(cPop, w = mm, fun = "sum", na.rm = TRUE)
    # ssD2 <- terra::init(cPop, fun = ssO2)
    
    ssD2 <- terra::mask(ssD2, dd, inverse = TRUE, updatevalue = 0)
    dd <- terra::mask(dd, ssD2 > 0, maskvalue = 1, updatevalue = s * mwidth)
    cPop <- ssD2
    cPop <- terra::mask(cPop, cPop > 0, maskvalue = 1, updatevalue = 1)
  }
  if (dissag) {
    dd <- terra::aggregate(dd, fact = kwidth)
  }
  dd <- terra::mask(dd, src)
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

