# Copyright © Her Majesty the Queen in Right of Canada as represented by the
# Minister of the Environment 2021/© Sa Majesté la Reine du chef du Canada
# représentée par le ministre de l'Environnement 2021.
# 
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
# 
#       http://www.apache.org/licenses/LICENSE-2.0
# 
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.


#' Convert raster to lines
#'
#' Converts rasters that represent lines into an sf object.
#'
#' For \code{method = "nearest"} raster is first converted to points and then
#' lines are drawn between the nearest points. If there are two different ways
#' to connect the points that have the same distance both are kept which can
#' cause doubled lines. USE WITH CAUTION. \code{method = "mst"} converts the
#' raster to points, reclassifies the raster so roads are 0 and other cells are
#' 1 and then uses \code{projectRoads} to connect all the points with a minimum
#' spanning tree. This will always connect all raster cells and is slower but
#' will not double lines as often. Neither method is likely to work for very
#' large rasters
#'
#' @param rast raster representing lines all values > 0 are assumed to be lines
#' @param method method of building lines. See Details
#'
#' @return an sf simple feature collection
#'
#' @examples
#' roadRast <- demoScen[[1]]$road.rast
#' # Note this is imperfect because the line is doubled where the two roads
#' # intersect
#' roadLine <- rasterToLineSegments(roadRast)
#'
#' @export

rasterToLineSegments <- function(rast, method = "mst"){
  if(method == "mst"){
    lnds <- raster::rasterToPoints(rast, fun = function(x){x > 0},
                                   spatial = TRUE) %>%
      sf::st_as_sf() %>% sf::st_set_agr("constant")
    
    cst <- rast == 0
    
    cst <- raster::reclassify(cst, matrix(c(0, 0.001, 1, 1), ncol = 2,
                                          byrow = TRUE), right = NA)
    
    prRes <- projectRoads(landings = lnds, cst, roads = lnds[1,])
    lines <- prRes$roads %>% sf::st_collection_extract("LINESTRING")
    return(lines)
    
  } else if(method == "nearest"){
    pts <- sf::st_as_sf(raster::rasterToPoints(rast,
                                               fun = function(x){x > 0},
                                               spatial = TRUE))
    # finds line between all points and keep shortest
    nearLn <- sf::st_nearest_points(pts, pts) %>%
      sf::st_as_sf() %>%
      mutate(len = sf::st_length(.data$x), ID = 1:n()) %>%
      sf::st_set_agr("constant")
    
    # speeds things up because filtering is slow on sf (as is [])
    nearLn2 <- nearLn %>% sf::st_drop_geometry() %>%
      filter(.data$len > 0) %>%
      filter(.data$len == min(.data$len))
    
    nearLn <- semi_join(nearLn, nearLn2, by = "ID")
    
    # remove duplicate lines
    coords <- sf::st_coordinates(nearLn) %>%
      as.data.frame() %>%
      group_by(.data$L1) %>%
      slice(1)
    
    nearLn2 <- nearLn %>% sf::st_drop_geometry() %>%
      mutate(coordX = pull(coords, .data$X),
             coordY = pull(coords, .data$Y)) %>%
      group_by(.data$coordX, .data$coordY) %>%
      slice(1:2)
    
    nearLn <- semi_join(nearLn, nearLn2, by = "ID") %>%
      sf::st_geometry() %>%
      sf::st_union()
    
    nearLn <- sf::st_sf(geometry = nearLn)
    
    return(nearLn)
  }
}

# #' @import spdep
# #' @import dplyr
# #' @import sp
# #' @importFrom raster res rasterToPoints
# Old version pretty slow doesn't work for demoScen[[1]]$road.rast
# convertRasterToLineSegments_old <- function(cMap){
#   #warning("Converting linear rasters to line segments. This will work for output from roads::projectRoads(), and raster::rasterize(SpatialLines), but not for rasters in general.")
# 
#   cPts = raster::rasterToPoints(cMap,fun=function(x){!is.na(x)})
#   cPts=subset(cPts,select=c('x','y'))
# 
#   #get nearest neighbour lists
#   cNN = spdep::dnearneigh(cPts,d1=1,d2=(raster::res(cMap)[[1]]^2*2)^0.5)
#   cNN=lapply(cNN,function(x){data.frame(B=x)})
# 
#   cNNs = dplyr::bind_rows(cNN, .id = 'A')
#   cNNs$A = as.numeric(cNNs$A)
#   cNNs=subset(cNNs,cNNs$B>0)
# 
#   cNNs$Atemp=cNNs$A
#   cNNs$A[cNNs$B<cNNs$Atemp]=cNNs$B[cNNs$B<cNNs$Atemp]
#   cNNs$B[cNNs$B<cNNs$Atemp]=cNNs$Atemp[cNNs$B<cNNs$Atemp]
#   cNNs$Atemp=NULL
# 
#   cNNs=unique(cNNs)
#   #this is the set of unique line segments.
#   cPtsDF = data.frame(cPts)
#   cPtsDF$A = seq(1:nrow(cPtsDF))
#   names(cPtsDF)=c('startX','startY','A')
#   cNNs=merge(cNNs,cPtsDF,all.x=T)
#   names(cPtsDF)=c('endX','endY','B')
#   cNNs=merge(cNNs,cPtsDF,all.x=T)
# 
#   begin.coord=subset(cNNs,select=c('startX','startY'));names(begin.coord)=c('x','y')
#   end.coord=subset(cNNs,select=c('endX','endY'));names(end.coord)=c('x','y')
#   ## raw list to store Lines objects
#   l <- vector('list', nrow(begin.coord))
#   for (i in seq_along(l)) {
#     #i=17806
#     l[[i]] <- sp::Lines(list(sp::Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
#   }
# 
#   cLines=sp::SpatialLines(l,proj4string =raster::crs(cMap))
#   return(cLines)
# }


