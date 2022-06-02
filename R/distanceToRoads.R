#Using fast parallel moving window approach to get distance from roads Note that
#pfocal package requires R version 4 or higher. Will consider with Sarah putting
#this function in the roads package. For now, here it is.

#The function uses pfocal, which is a fast parallel moving window function we
#have been developing for another project. In order to install the pfocal
#package, you will need to make sure you have a current version of R installed.
#If you run into problems installing (and you may) let us know. Now is a good
#time for us to find and fix problems with that package.

#There is a tradeoff between speed and accuracy in this method, controlled by
#the parameters kwidth and dissag. kwidth is the radius of the moving window in
#number of cells. For very small values (e.g. 1,2) we get grid artefacts in the
#results. Larger moving windows are more circular which reduces the grid
#artefacts. If dissag is false, the resulting distance map will have values of
#0, 1kwidth, 2kwidth, etc. If dissag is true the algorithm disaggregates the
#raster to a finer resolution, so the resulting distance map will have values of
#0,1,2, etc, but there is considerable computational cost associated with the
#higher accuracy.
# 
#devtools::install_github("LandSciTech/pfocal")
library(raster)
library(pfocal)
dataDir = "../RoadPaper/analysis/data/raw_data/"
maxDist=10000 #in units of res
src = raster(paste0(dataDir,"/subsetBinary.tif"))
src=trim(src)

src <- roadsExist_rast

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

getDistFromSource <- function(src,maxDist,kwidth=3,dissag=F){
  if(dissag){
    mwidth=res(src)[1]
    src=disaggregate(src,fact=kwidth)
  }else{
    mwidth=res(src)[1]*kwidth
  }
  src[src>0]=1
  mm = uniformKernel(kwidth,useAveDist=F)
  nSteps=ceiling(maxDist/mwidth)
  dd = src;dd=1-dd;dd[dd!=0]=NA
  cPop = src;cPop[is.na(cPop)]=0
  for(s in 1:nSteps){
    ssO2 = pfocal(as.matrix(cPop),mm,reduce_function="SUM", transform_function="MULTIPLY")
    ssD2 = cPop
    values(ssD2)=ssO2
    dd[is.na(dd)&(ssD2>0)]=s*mwidth
    cPop=ssD2;cPop[cPop>0]=1
  }
  if(dissag){
    dd=aggregate(dd,fact=kwidth)
  }
  dd[is.na(src)]=NA
  return(dd)
}

fastRough=getDistFromSource(src,maxDist,kwidth=3,dissag=F)
slowFine=getDistFromSource(as(src,"Raster"),maxDist,kwidth=3,dissag=T)

square=getDistFromSource(src,maxDist,kwidth=1,dissag=F)
smootherCircle=getDistFromSource(src,maxDist,kwidth=5,dissag=F)

res = c(fastRough,terra::rast(slowFine),square,smootherCircle)
names(res)=c("fastRough","slowFine","square","smootherCircle")
plot(res)

srcBig = raster(paste0(dataDir,"/revelstokeTSA_Binary_nonAggregated.tif"))
srcBig=trim(srcBig)

ptm <- proc.time()
big=getDistFromSource(srcBig,maxDist,kwidth=3,dissag=F)
proc.time() - ptm
plot(stack(srcBig,big))