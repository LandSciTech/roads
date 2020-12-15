#' @import spdep
#' @import dplyr
#' @import sp
#' @importFrom raster res rasterToPoints
#' @export

convertRasterToLineSegments <- function(rast){
  pts <- sf::st_as_sf(raster::rasterToPoints(rast,
                                             fun = function(x){x > 0},
                                             spatial = TRUE))
  
  nearLn <- sf::st_nearest_points(pts, pts) %>% 
    sf::st_as_sf() %>%  
    mutate(len = sf::st_length(x), ID = 1:n())
  
  # speeds things up because filtering is slow on sf (as is [])
  nearLn2 <- nearLn %>% sf::st_drop_geometry() %>% 
    filter(len > 0) %>% 
    filter(len == min(len))
  
  nearLn <- semi_join(nearLn, nearLn2, by = "ID")
  
  coords <- sf::st_coordinates(nearLn) %>% 
    as.data.frame() %>% 
    group_by(L1) %>% 
    summarise(X =  first(X), Y = first(Y), )
  
  nearLn2 <- nearLn %>% sf::st_drop_geometry() %>% 
    mutate(coordX = pull(coords, X),
           coordY = pull(coords, Y)) %>% 
    group_by(.data$coordX, .data$coordY) %>% 
    summarise(ID = first(.data$ID)) 
  
  nearLn <- semi_join(nearLn, nearLn2, by = "ID") %>% 
    sf::st_geometry() %>% 
    sf::st_union() %>% 
    sf::st_as_sf()
  
  return(nearLn)
  
}

# Old version pretty slow doesn't work for demoScen[[1]]$road.rast
convertRasterToLineSegments_old <- function(cMap){
  #warning("Converting linear rasters to line segments. This will work for output from roads::projectRoads(), and raster::rasterize(SpatialLines), but not for rasters in general.")

  cPts = raster::rasterToPoints(cMap,fun=function(x){!is.na(x)})
  cPts=subset(cPts,select=c('x','y'))

  #get nearest neighbour lists
  cNN = spdep::dnearneigh(cPts,d1=1,d2=(raster::res(cMap)[[1]]^2*2)^0.5)
  cNN=lapply(cNN,function(x){data.frame(B=x)})

  cNNs = dplyr::bind_rows(cNN, .id = 'A')
  cNNs$A = as.numeric(cNNs$A)
  cNNs=subset(cNNs,cNNs$B>0)

  cNNs$Atemp=cNNs$A
  cNNs$A[cNNs$B<cNNs$Atemp]=cNNs$B[cNNs$B<cNNs$Atemp]
  cNNs$B[cNNs$B<cNNs$Atemp]=cNNs$Atemp[cNNs$B<cNNs$Atemp]
  cNNs$Atemp=NULL

  cNNs=unique(cNNs)
  #this is the set of unique line segments.
  cPtsDF = data.frame(cPts)
  cPtsDF$A = seq(1:nrow(cPtsDF))
  names(cPtsDF)=c('startX','startY','A')
  cNNs=merge(cNNs,cPtsDF,all.x=T)
  names(cPtsDF)=c('endX','endY','B')
  cNNs=merge(cNNs,cPtsDF,all.x=T)

  begin.coord=subset(cNNs,select=c('startX','startY'));names(begin.coord)=c('x','y')
  end.coord=subset(cNNs,select=c('endX','endY'));names(end.coord)=c('x','y')
  ## raw list to store Lines objects
  l <- vector('list', nrow(begin.coord))
  for (i in seq_along(l)) {
    #i=17806
    l[[i]] <- sp::Lines(list(sp::Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
  }

  cLines=sp::SpatialLines(l,proj4string =raster::crs(cMap))
  return(cLines)
}
