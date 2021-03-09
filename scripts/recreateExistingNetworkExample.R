library(raster)
#I think I should be able to recreate existing road network from a point on it, setting cost to 0 where roads exist.
#But I can't.
#DO: set cost low but not 0
#DO: convert landings to points.
#If this works reasonably quickly, consider replacing existing rasterToLineSegments function.

cMap = roads::CLUSexample$roads

rds = raster::rasterToPoints(cMap, fun = function(x){x > 0},
                             spatial = TRUE)
rds=spsample(rds,n=1,type="random")
plot(cMap);plot(rds,add=T)


cost <- 1-cMap

nw = roads::projectRoadsNew(landings = cMap,
                            cost = cost,
                            roads = rds,roadsOut="raster")
plot(nw$roads)
