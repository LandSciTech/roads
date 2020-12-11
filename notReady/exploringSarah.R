# Sarah experimenting
library(roads)
library(dplyr)

CLUSexample
# Note call to gc that was in function caused 3 sec of wasted time!! when rest
# of funciton took 20ms

roadCLUS.getClosestRoad <- function(sim){
  roads.pts <- raster::rasterToPoints(sim$roads, fun=function(x){x > 0})
  closest.roads.pts <- apply(rgeos::gDistance(sp::SpatialPoints(roads.pts),sp::SpatialPoints(sim$landings), byid=TRUE), 1, which.min)
  sim$roads.close.XY <- as.matrix(roads.pts[closest.roads.pts, 1:2,drop=F]) #this function returns a matrix of x, y coordinates corresponding to the closest road
  #The drop =F is needed for a single landing - during the subset of a matrix it will become a column vector because as it converts a vector to a matrix, r will assume you have one column
  rm(roads.pts, closest.roads.pts)
  return(invisible(sim))
}

roadCLUS.getClosestRoad.sf <- function(sim){
  # convert roads to sp points dataframe
  roads.pts <- raster::rasterToPoints(sim$roads, fun=function(x){x > 0}, spatial = TRUE)
  
  # union roads to one feature
  roads.pts <- sf::st_union(sf::st_as_sf(roads.pts))
  
  # find nearest point between road feature and landings, returns a line between
  # the points
  closest.roads.pts <- sf::st_nearest_points(sim$landings, roads.pts)
  
  # convert lines to points
  closest.roads.pts <- sf::st_cast(closest.roads.pts, "POINT")
  
  # get every second point which is the ones on the road
  closest.roads.pts <- closest.roads.pts[seq(2, length(closest.roads.pts), 2)]
  
  # assign coord matrix
  sim$roads.close.XY <-  sf::st_coordinates(closest.roads.pts)
  colnames(sim$roads.close.XY) <- c("x", "y")
  
  rm(roads.pts, closest.roads.pts)
  
  return(invisible(sim))
}

roadCLUS.getClosestRoad.sf2 <- function(sim){
  # union roads to one feature
  roads.pts <- sf::st_union(sf::st_as_sf(sim$roads))
  
  # find nearest point between road feature and landings, returns a line between
  # the points
  closest.roads.pts <- sf::st_nearest_points(sim$landings, roads.pts)
  
  # convert lines to points
  closest.roads.pts <- sf::st_cast(closest.roads.pts, "POINT")
  
  # get every second point which is the ones on the road
  closest.roads.pts <- closest.roads.pts[seq(2, length(closest.roads.pts), 2)]
  
  # assign coord matrix
  sim$roads.close.XY <-  sf::st_coordinates(closest.roads.pts)
  colnames(sim$roads.close.XY) <- c("x", "y")
  
  rm(roads.pts, closest.roads.pts)
  
  return(invisible(sim))
}

simList <- list(roads = demoScen[[1]]$road.rast, 
                costSurface = demoScen[[1]]$cost.rast,
                landings = rbind(demoScen[[1]]$landings.points, 
                                 demoScen[[1]]$landings.points))

simListSF <- list(roads = demoScen[[1]]$road.rast, 
                  costSurface = demoScen[[1]]$cost.rast,
                  landings = rbind(demoScen[[1]]$landings.points, 
                                   demoScen[[1]]$landings.points) %>%
                    sf::st_as_sf())

simListSF2 <- list(roads = demoScen[[1]]$road.line, 
                costSurface = demoScen[[1]]$cost.rast,
                landings = rbind(demoScen[[1]]$landings.points, 
                                 demoScen[[1]]$landings.points) %>%
                  sf::st_as_sf())

landingsSF <- sf::st_as_sf(demoScen[[1]]$landings.points)
roadsSF <- sf::st_as_sf(raster::rasterToPoints(demoScen[[1]]$road.rast, 
                                               fun=function(x){x > 0}, 
                                               spatial = TRUE)) %>% 
  sf::st_union()

roadsSF2 <- sf::st_as_sf(demoScen[[1]]$road.line) %>% 
  sf::st_union()

nearPts <- sf::st_nearest_points(landingsSF, roadsSF) 
nearPts2 <- sf::st_cast(nearPts, "POINT") %>% .[seq(2, length(.), 2)]

plot(sf::st_geometry(landingsSF))
plot(sf::st_geometry(roadsSF2), add = TRUE, col = "blue")
plot(nearPts2, add= TRUE, col = "red")
plot(nearPts, add= TRUE, col = "green")

spVersion <- roadCLUS.getClosestRoad(simList)

sfVersion <- roadCLUS.getClosestRoad.sf(simListSF)

sfVersion2 <- roadCLUS.getClosestRoad.sf2(simListSF2)

mb <- microbenchmark::microbenchmark(roadCLUS.getClosestRoad(simList),
                               roadCLUS.getClosestRoad.sf(simListSF), 
                               roadCLUS.getClosestRoad.sf2(simListSF2))
ggplot2::autoplot(mb)

#compare results
mismatchSfSp <- bind_cols(as.data.frame(sfVersion$roads.close.XY), 
                      as.data.frame(spVersion$roads.close.XY)) %>% 
  mutate(ind = 1:n()) %>% 
  filter(X != x | Y != y)

mismatchSf2Sp <- bind_cols(as.data.frame(sfVersion2$roads.close.XY), 
                          as.data.frame(spVersion$roads.close.XY)) %>% 
  mutate(ind = 1:n()) %>% 
  filter(X != x | Y != y)

# make points into sf dataframe
closest.roads.pts.sp2 <- as.data.frame(spVersion$roads.close.XY) %>% 
  mutate(ind = 1:n()) %>% 
  group_by(ind) %>% 
  mutate(geometry = sf::st_point(x = c(x, y)) %>% list() %>% sf::st_as_sfc()) %>% 
  sf::st_as_sf()

plot(landingsSF %>% sf::st_geometry())
plot(roadsSF %>% sf::st_geometry(), add = TRUE)
plot(closest.roads.pts.sf[mismatchSfSp$ind], add = TRUE, col = "red")
plot(closest.roads.pts.sp2[mismatchSfSp$ind,] %>% sf::st_geometry(),
     add = TRUE, col = "green")

# seems like a tie is broken differently for the two methods but otherwise the
# same when using raster as road input

plot(landingsSF %>% sf::st_geometry())
plot(roadsSF2 %>% sf::st_geometry(), add = TRUE)
plot(closest.roads.pts.sp2[mismatchSf2Sp$ind,] %>% sf::st_geometry(),
     add = TRUE, col = "green")
plot(closest.roads.pts.sf2, add = TRUE, col = "red", pch = 4)
