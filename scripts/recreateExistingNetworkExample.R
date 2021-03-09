library(raster)
#I think I should be able to recreate existing road network from a point on it, setting cost to 0 where roads exist.
#But I can't.

cMap = CLUSexample$roads

rds = raster::rasterToPoints(cMap, fun = function(x){x > 0},
                             spatial = TRUE)
rds = spsample(rds,n=1,type="random")
plot(cMap)
plot(rds,add=T)

# make landings into points because the raster is interpreted as one large
# harvest block and only the centroid is used as a landing
lnds <-  raster::rasterToPoints(cMap, 
                                fun = function(x){x > 0},
                                spatial = TRUE)

cost <- 1-cMap

# need to set cost to slightly more than 0 because the algorithm uses cost 0
# cells as existing roads (I will make this clear in the documentation)
cost[cost == 0] <- 0.001
nw = roads::projectRoadsNew(landings = lnds,
                            cost = cost,
                            roads = rds,roadsOut="raster")
plot(nw$roads)
plot(nw$landings, add = T)
