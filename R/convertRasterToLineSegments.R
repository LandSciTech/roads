#' @import spdep
#' @export
convertRasterToLineSegments<-function(cMap){
  #warning("Converting linear rasters to line segments. This will work for output from roads::projectRoads(), and raster::rasterize(SpatialLines), but not for rasters in general.")
  
  cPts = raster::rasterToPoints(cMap,fun=function(x){!is.na(x)})
  cPts=subset(cPts,select=c(x,y))
  
  #get nearest neighbour lists
  cNN = spdep::dnearneigh(cPts,d1=1,d2=(res(cMap)[[1]]^2*2)^0.5)
  cNN=lapply(cNN,function(x){data.frame(B=x)})
  
  cNNs = dplyr::bind_rows(cNN, .id = "A")
  cNNs$A = as.numeric(cNNs$A)
  cNNs=subset(cNNs,B>0)
  
  cNNs$Atemp=cNNs$A
  cNNs$A[cNNs$B<cNNs$Atemp]=cNNs$B[cNNs$B<cNNs$Atemp]
  cNNs$B[cNNs$B<cNNs$Atemp]=cNNs$Atemp[cNNs$B<cNNs$Atemp]
  cNNs$Atemp=NULL
  
  cNNs=unique(cNNs)
  #this is the set of unique line segments.
  cPtsDF = data.frame(cPts)
  cPtsDF$A = seq(1:nrow(cPtsDF))
  names(cPtsDF)=c("startX","startY","A")
  cNNs=merge(cNNs,cPtsDF,all.x=T)
  names(cPtsDF)=c("endX","endY","B")
  cNNs=merge(cNNs,cPtsDF,all.x=T)
  
  begin.coord=subset(cNNs,select=c(startX,startY));names(begin.coord)=c("x","y")
  end.coord=subset(cNNs,select=c(endX,endY));names(end.coord)=c("x","y")
  ## raw list to store Lines objects
  l <- vector("list", nrow(begin.coord))
  for (i in seq_along(l)) {
    #i=17806
    l[[i]] <- sp::Lines(list(sp::Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
  }
  
  cLines=sp::SpatialLines(l,proj4string =raster::crs(cMap))
  return(cLines)
}
