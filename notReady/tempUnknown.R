library(raster) # load the raster package

#install.packages("colorRamps")
# function for visualizing output
visualize <- function(projRoadsResults,landings,scen){
  plot(scen$cost.rast,col=c("black",colorRamps::matlab.like(255+50+50)[50:(50+255)]))
  if (class(projRoadsResults)!="RasterBrick"){
    newRoads <- projRoadsResults$roads > 0
    newRoads[!newRoads] <- NA
    newRoads[scen$cost.rast==0] <- NA
    plot(newRoads,col="grey50",add=T,legend=F)
    if (class(landings)=="SpatialPointsDataFrame"){
      points(landings,pch=21,cex=2,bg="white")
    }else if (class(landings)=="SpatialPolygonsDataFrame"){
      plot(landings,density=20,add=T)
    }
  }else{
    newRoads <- raster::raster(ext=raster::extent(scen$cost.rast),res=1)
    nlayers <- raster::nlayers(projRoadsResults)
    for (i in nlayers:1){newRoads[projRoadsResults[[i]]]<-i}
    plot(newRoads,col=c("black",grey.colors(nlayers-1)),add=T,legend=F)
    points(landings,pch=21,cex=2.5,bg="white")
    text(landings@coords[,1],landings@coords[,2],landings@data$set,cex=0.8)
  }
}

### project roads: using scenario 1 / landings as a SpatialPointsDataFrame / the least-cost path ("lcp") approach
scen <- roads::demoScen[[1]] # demo scenario 1
landings.points <- scen$landings.points[scen$landings.points$set==1,] # use landing set 1 of demo scenario 1
projRoadsResults <- roads::projectRoads(landings=landings.points,cost=scen$cost.rast,roads=scen$road.rast,roadMethod="lcp")
visualize(projRoadsResults,landings.points,scen)

### project roads: using scenario 1 / landings as a RasterLayer / the minimum spanning tree ("mst") approach
scen <- roads::demoScen[[1]] # demo scenario 1
landings.points <- scen$landings.points[scen$landings.points$set==1,] # use landing set 1 of demo scenario
landings.rastLayer   <- scen$landings.stack[[1]] # the RasterLayer version of landing set 1
projRoadsResults <- roads::projectRoads(landings=landings.rastLayer,cost=scen$cost.rast,roads=scen$road.rast,roadMethod="mst")
visualize(projRoadsResults,landings.points,scen)

### project roads: using scenario 2 / landings as a matrix / the snapping ("snap") approach
scen <- roads::demoScen[[2]] # demo scenario 2
landings.points <- scen$landings.points[scen$landings.points$set==5,] # use landing set 5 of demo scenario 2
landings.matrix <- landings.points@coords  # landings as a matrix
projRoadsResults <- roads::projectRoads(landings=landings.matrix,cost=scen$cost.rast,roads=scen$road.rast,roadMethod="snap")
visualize(projRoadsResults,landings.points,scen)

### project roads: using scenario 3 / landings as a RasterStack / the minimum spanning tree ("mst") approach
scen <- roads::demoScen[[3]] # demo scenario 3
landings.points <- scen$landings.points[scen$landings.points$set%in%1:4,] # use landing sets 1 to 4 of demo scenario 3
landings.rastStack <- scen$landings.stack[[1:4]]  # use landing sets 1 to 4 as a RasterStack
projRoadsResults <- roads::projectRoads(landings=landings.rastStack,cost=scen$cost.rast,roads=scen$road.rast,roadMethod="mst")
visualize(projRoadsResults,landings.points,scen)

### project roads: using scenario 7 / landings as a SpatialPolygonsDataFrame / the minimum spanning tree ("mst") approach
scen <- roads::demoScen[[7]] # demo scenario 7
landings.poly <- scen$landings.poly # use polygonal landings of demo scenario 7
projRoadsResults <- roads::projectRoads(landings=landings.poly,cost=scen$cost.rast,roads=scen$road.rast,roadMethod="mst")
visualize(projRoadsResults,landings.poly,scen)
