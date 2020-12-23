# Sarah experimenting
library(dplyr)
devtools::load_all(".")

# Small example data
CLUSexample
roads <- CLUSexample$roads
cost <- CLUSexample$cost
landings <- CLUSexample$landings
roadMethod = "mst"
neighbourhood = "octagon"

# check that all roadMethods return same list format
outs <- purrr::map(list(snap = "snap", lcp = "lcp", mst = "mst"), 
                   ~projectRoadsNew(landings = landings, cost = cost, 
                                    roads = roads, roadMethod = .x))

purrr::map(outs, names) %>% unlist() %>% unique()
purrr::map(outs, length)


# compare old method and new #==================================================
## Inputs raster and sp points should give advatage to old method
roads <-  demoScen[[1]]$road.rast
costSurface <- demoScen[[1]]$cost.rast

#double the landings to make it easier to compare speed
landings <- rbind(demoScen[[1]]$landings.points, 
                 demoScen[[1]]$landings.points)

# snap
profvis::profvis({
  newSnap <- projectRoadsNew(landings, costSurface, roads, "snap")
  
  oldSnap <- projectRoads(landings, costSurface, roads, "snap")
})
# new is 7x faster 

# LCP
profvis::profvis({
  newLCP <- projectRoadsNew(landings, costSurface, roads, "lcp")
  
  oldLCP <- projectRoads(landings, costSurface, roads, "lcp")
})
# old is 3 times faster after removing gc mostly bc of rasterToLineSegments

# MST
profvis::profvis({
  newMST <- projectRoadsNew(landings, costSurface, roads, "mst")
  
  oldMST <- projectRoads(landings, costSurface, roads, "mst")
})
# old is 3 times faster after removing gc mostly bc of rasterToLineSegments

# Compare results
newSnap$roads %>% plot()
oldSnap$newRoads.lines %>% plot()

newLCP$roads %>% plot()
plot(oldLCP$roads > 0)

newMST$roads %>% plot()
(oldMST$roads > 0) %>% plot()

# all seem to match well


## Inputs line and sp points should be even faster
roads <-  demoScen[[1]]$road.line
costSurface <- demoScen[[1]]$cost.rast

# double the landings to make it easier to compare speed
landings <- rbind(demoScen[[1]]$landings.points, 
                  demoScen[[1]]$landings.points)

# snap
profvis::profvis({
  newSnap <- projectRoadsNew(landings, costSurface, roads, "snap")
  
  oldSnap <- projectRoads(landings, costSurface, 
                          raster::rasterize(roads, costSurface),
                          "snap")
})
# new is > 80x faster 

# LCP
profvis::profvis({
  newLCP <- projectRoadsNew(landings, costSurface, roads, "lcp")
  
  oldLCP <- projectRoads(landings, costSurface, 
                         raster::rasterize(roads, costSurface), 
                         "lcp")
})
# new is 2.9x faster with rasterize and 1.5x faster without. 

# MST
profvis::profvis({
  newMST <- projectRoadsNew(landings, costSurface, roads, "mst")
  
  oldMST <- projectRoads(landings, costSurface, 
                         raster::rasterize(roads, costSurface),
                         "mst")
})
# new is 1.8x faster with rasterize and even without. 

# Compare results
newSnap$roads %>% plot()
oldSnap$newRoads.lines %>% plot()
# results are a bit different because roads go to nearest point on road not
# limited to raster cell centre

newLCP$roads %>% plot()
plot(oldLCP$roads > 0)
# mostly match because still follow cost raster

newMST$roads %>% plot()
(oldMST$roads > 0) %>% plot()
# mostly match because still follow cost raster

# overall similar but snap roads are slightly shifted

## Inputs sf line and sf points. Check that landings being not cell centers still
## works
roadsSF <-  demoScen[[1]]$road.line %>% sf::st_as_sf()
costSurface <- demoScen[[1]]$cost.rast

# old method won't work with sf or line
roads <- demoScen[[1]]$road.rast

# randomly generate 126 points for landings
landings <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(roads))) %>% 
  {sf::st_sf(geometry = sf::st_sample(., 126))}

# snap
profvis::profvis({
  newSnap <- projectRoadsNew(landings, costSurface, roadsSF, "snap")
  
  oldSnap <- projectRoads(landings %>% mutate(ID = 1:n()) %>% sf::as_Spatial(),
                          costSurface, 
                          roads, 
                          "snap")
})
# new is > 80x faster 

# LCP
profvis::profvis({
  newLCP <- projectRoadsNew(landings, costSurface, roadsSF, "lcp")
  
  oldLCP <- projectRoads(landings %>% mutate(ID = 1:n()) %>% sf::as_Spatial(),
                         costSurface, 
                         roads,
                         "lcp")
})
# new is 2.9x But most of the difference is due to gc

# MST
profvis::profvis({
  newMST <- projectRoadsNew(landings, costSurface, roadsSF, "mst")
  
  oldMST <- projectRoads(landings %>% mutate(ID = 1:n()) %>% sf::as_Spatial(),
                         costSurface, 
                         roads,
                         "mst")
})
# new is 1.6x faster. But most of the difference is due to gc and
# newRoadsToLine seems faster than pathsToLine

# Compare results
newSnap$roads %>% plot()
oldSnap$newRoads.lines %>% plot()
# results are a bit different because roads go to nearest point on road not
# limited to raster cell centre

newLCP$roads %>% plot()
plot(oldLCP$roads > 0)
# mostly match because still follow cost raster

newMST$roads %>% plot(col = "red", add = T)
(oldMST$roads > 0) %>% plot()
plot(landings %>% st_geometry(), pch = 4)
# mostly match because still follow cost raster. Roads end at the closest raster
# center rather than the actual landing point, could be an issue if res of cost
# raster was course

# overall similar but snap roads are slightly 

# Benchmark compare speeds #====================================================

# different inputs
roadsR <- demoScen[[1]]$road.rast
roadsSP <-  demoScen[[1]]$road.line
roadsSF <-  demoScen[[1]]$road.line %>% sf::st_as_sf()

landingsSP <- rbind(demoScen[[1]]$landings.points, 
                    demoScen[[1]]$landings.points)

landingsSF <- rbind(demoScen[[1]]$landings.points, 
                    demoScen[[1]]$landings.points) %>% 
  sf::st_as_sf()
  
costSurface <- demoScen[[1]]$cost.rast

# plot differences in speed
mb <- microbenchmark::microbenchmark(
  oldSnap = projectRoads(landingsSP, costSurface, roadsR, "snap"),
  oldLCP = projectRoads(landingsSP, costSurface, roadsR, "lcp"),
  oldMST = projectRoads(landingsSP, costSurface, roadsR, "mst"),
  
  newSnap_sp_rast = projectRoadsNew(landingsSP, costSurface, roadsR, "snap"),
  newLCP_sp_rast = projectRoadsNew(landingsSP, costSurface, roadsR, "lcp"),
  newMST_sp_rast = projectRoadsNew(landingsSP, costSurface, roadsR, "mst"),
  
  newSnap_sp_sp = projectRoadsNew(landingsSP, costSurface, roadsSP, "snap"),
  newLCP_sp_sp = projectRoadsNew(landingsSP, costSurface, roadsSP, "lcp"),
  newMST_sp_sp = projectRoadsNew(landingsSP, costSurface, roadsSP, "mst"),
  
  newSnap_sf_sf = projectRoadsNew(landingsSF, costSurface, roadsSF, "snap"),
  newLCP_sf_sf = projectRoadsNew(landingsSF, costSurface, roadsSF, "lcp"),
  newMST_sf_sf = projectRoadsNew(landingsSF, costSurface, roadsSF, "mst"),
  
  times = 10
)

mb %>% 
  mutate(version = ifelse(stringr::str_detect(expr, "old"), "old", "new"),
         roadMethod = stringr::str_extract(expr, "MST|LCP|Snap"),
         name = paste0(roadMethod,"-" ,version, stringr::str_extract(expr, "_.*"))) %>% 
  ggplot2::ggplot(ggplot2::aes(x = name, y = time, fill = version))+
  ggplot2::geom_violin(col = "grey", scale = "width")+
  ggplot2::coord_flip()+
  ggplot2::theme_bw()
ggplot2::ggsave("notReady/compareOldVsNewSpeedRemoveGC.png")  

# compare getGraph on a big raster 
bigrast <- raster::raster("wc10/tmin1.bil")
medrast <- raster::aggregate(bigrast,4)
profvis::profvis({
  roadCLUS.getGraph(list(costSurface = medrast), "octagon")
  getGraph(list(costSurface = medrast), "octagon")
})

microbenchmark::microbenchmark(
  roadCLUS.getGraph(list(costSurface = bigrast), "octagon"),
  getGraph(list(costSurface = bigrast), "octagon"), 
  times = 10
) %>% ggplot2::autoplot()

# with the big rast there are a lot of GC calls that make it hard to tell so
# profile with medrast. Overall new seems slightly faster


# Check with different sf col name #============================================
roadsSF <-  demoScen[[1]]$road.line %>% sf::st_as_sf() %>% rename(g = geometry)

landingsSF <- rbind(demoScen[[1]]$landings.points, 
                    demoScen[[1]]$landings.points) %>% 
  sf::st_as_sf() %>%
  rename(g = geometry)

costSurface <- demoScen[[1]]$cost.rast

projectRoadsNew(landingsSF, costSurface, roadsSF)

# explore polygon landings process
landings <- demoScen[[1]]$landings.stack$layer.1.1.1

# projectRoads does for RasterLayer 
landingsCent = getCentroids(landings, withIDs=T)

landingsPts = raster::rasterToPoints(landings,fun=function(landings){landings>0})

# getCentroids finds clumps in raster and then gets average coords at res of
# raster. This seems to shift the landings when there is no clump but just one
# raster cell per landing. 

plot(landings, col = c("white", "black"))
plot(landingsCent, add = T)

# check if there are any clumps of more than 1 cell
clumps <- raster::clump(landings, gaps = F)
raster::freq(clumps) %>% as.data.frame() %>% filter(!is.na(value)) %>% 
  pull(count) %>% max() > 1

# in this case make more sense to just take rasterToPoints to get centoid

# make example with clumps
landClumps <- raster::raster(ncols=100, nrows=100, crs = NA)
landClumps[c(1:5,101:105)] <- 1
landClumps[c(450:455,550:555)] <- 1
landClumps[700] <- 1
rc <- raster::clump(landClumps, gaps = F) 
raster::freq(rc)
plot(rc)
raster::freq(rc, useNA = "no")[,2] > 1

# this makes them all one multipolygon
polys1 <- raster::rasterToPolygons(landClumps, dissolve = TRUE)

# this makes each clump a separate poly
polys2 <- raster::rasterToPolygons(rc, dissolve = TRUE) %>% sf::st_as_sf()

# can get center of poly as landing approximation
ptClumpCent <- polys2 %>% sf::st_centroid()

# or could get nearest point on poly border to another feature
ptOnClumpEdge <-  sf::st_nearest_points(polys2, sf::st_point(c(15, 50))) %>% 
  sf::st_cast("POINT") %>% .[seq(1, length(.), 2)]

plot(polys2 %>% sf::st_geometry())
plot(ptOnClumpEdge, add = TRUE, col = "red")
plot(sf::st_point(c(15, 50)), add = TRUE)

plot(ptClumpCent, add = TRUE, col = "green")

# or multiple points has problems for very small polygons, size is not exact
ptsInClump <- sf::st_sample(polys2$geometry, type = "regular", size = 3, 
                            offset = c(raster::extent(landClumps)@xmin,
                                       raster::extent(landClumps)@ymin))
plot(ptsInClump, add = TRUE, col = "blue")

# Raster to lines with Voronoi Not working... #=================================
rast <- demoScen[[1]]$road.rast

rast.poly <- raster::rasterToContour(rast, levels = 1) %>% st_as_sf()

rast.line <- sf::st_buffer(rast.poly, dist = 0.01, singleSide = TRUE) %>% 
  {sf::st_difference(rast.poly, .)} %>% st_cast("LINESTRING")

rast.pts <- raster::rasterToPoints(rast, fun = function(x)x>0, 
                                   spatial = TRUE) %>% 
  sf::st_as_sf() %>% sf::st_union()

env <- sf::st_buffer(rast.pts, dist = 1)

vor.pts <- env %>% 
  sf::st_cast("MULTIPOINT") %>%  
  sf::st_union() %>% 
  sf::st_make_valid()

rast.vor <- sf::st_voronoi(vor.pts, sf::st_buffer(env, dist = 1)) %>% 
  unlist(recursive = FALSE) %>% sf::st_as_sfc() %>% sf::st_make_valid()

rast.vor2 <- sf::st_intersection(rast.vor, rast.pts)
# Miscellaneous exploring #=====================================================
# Note call to gc that was in function caused 3 sec of wasted time!! when rest
# of funciton took 20ms



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


# lcpList

roadCLUS.lcpList<- function(sim){
  ##Get a list of cell indexs for to and from points
  paths.matrix<-cbind(raster::cellFromXY(sim$costSurface,sim$landings ), raster::cellFromXY(sim$costSurface,sim$roads.close.XY ))
  sim$paths.list<-split(paths.matrix, 1:nrow(paths.matrix))
  rm(paths.matrix)
  gc()
  return(invisible(sim))
}

roadCLUS.lcpListNoGC<- function(sim){
  ##Get a list of cell indexs for to and from points
  paths.matrix<-cbind(raster::cellFromXY(sim$costSurface,sim$landings ), raster::cellFromXY(sim$costSurface,sim$roads.close.XY ))
  sim$paths.list<-split(paths.matrix, 1:nrow(paths.matrix))
  rm(paths.matrix)
  # gc()
  return(invisible(sim))
}
simLCPList <- roadCLUS.lcpList(spVersion)
microbenchmark::microbenchmark(roadCLUS.lcpList(spVersion),
                               roadCLUS.lcpListNoGC(spVersion))
# gc() HUGELY increases time!!
# Unit: microseconds
#                            expr        min          lq        mean      median
# roadCLUS.lcpList(spVersion)     181667.253 185912.3195 189554.0898 188157.7215
# roadCLUS.lcpListNoGC(spVersion)    141.503    190.9205    341.5714    212.7935
#          uq        max neval cld
# 191038.5345 239738.253   100   b
#    332.4225   8583.569   100  a 

roadCLUS.shortestPaths<- function(sim){
  #print('shortestPaths')
  #------finds the least cost paths between a list of two points
  if(!length(sim$paths.list)==0){
    #print(sim$paths.list)
    
    #create a list of shortest paths
    paths <- unlist(lapply(sim$paths.list, 
                         function(x) {
                           igraph::get.shortest.paths(sim$g, x[1], x[2], 
                                                      output = "both")})) 
    
    # save the verticies for mapping
    sim$paths.v <- rbind(data.table::data.table(paths[grepl("vpath", names(paths))]),
                         sim$paths.v) 
    
    paths.e <- paths[grepl("epath", names(paths))]
    
    # changes the cost(weight) associated with the edge that became a path (or
    # road)
    igraph::edge_attr(sim$g, 
                      index = igraph::E(sim$g)[igraph::E(sim$g) %in% paths.e], 
                      name= 'weight') <- 0.00001 
    
    # reset landings and roads close to them
    sim$landings<-NULL
    sim$roads.close.XY<-NULL
    sim$newRoads.lines<-newRoadsToLines(sim)
    rm(paths.e)
    gc()
  }
  return(invisible(sim))
}

simLCPList <- roadCLUS.getGraph(simLCPList, "octagon")

shortPathsList <- roadCLUS.shortestPaths(simLCPList)

# Only uses the cost surface to make graph
roadCLUS.getGraph<- function(sim,neighbourhood){
  ###Set the grpah which determines least cost paths
  #Creates a graph (sim$g) in inititation phase which can be updated and solved for paths
  sim$paths.v<-NULL
  #------prepare the cost surface raster
  ras.matrix<-raster::as.matrix(sim$costSurface)#get the cost surface as a matrix using the raster package
  
  weight<-c(t(ras.matrix)) #transpose then vectorize which matches the same order as adj
  weight<-data.table::data.table(weight) # convert to a data.table - faster for large objects than data.frame
  #weight<-data.table(getValues(sim$costSurface)) #Try
  weight[, id := seq_len(.N)] # get the id for ther verticies which is used to merge with the edge list from adj
  
  #------get the adjacency using SpaDES function adj
  #rooks case
  if(!is.element(neighbourhood,c("rook","octagon","queen"))){
    stop("neighbourhood type not recognized")
  }
  
  edges<-SpaDES.tools::adj(returnDT= TRUE, numCol = ncol(ras.matrix), numCell=ncol(ras.matrix)*nrow(ras.matrix), directions =4, cells = 1:as.integer(ncol(ras.matrix)*nrow(ras.matrix)))
  edges<-data.table::data.table(edges)
  #edges[from < to, c("from", "to") := .(to, from)]
  edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]
  edges<-unique(edges)
  edges.w1<-merge(x=edges, y=weight, by.x= "from", by.y ="id") #merge in the weights from a cost surface
  data.table::setnames(edges.w1, c("from", "to", "w1")) #reformat
  edges.w2<-data.table::setDT(merge(x=edges.w1, y=weight, by.x= "to", by.y ="id"))#merge in the weights to a cost surface
  data.table::setnames(edges.w2, c("from", "to", "w1", "w2")) #reformat
  edges.w2$weight <-(edges.w2$w1 + edges.w2$w2)/2 #take the average cost between the two pixels
  
  if(neighbourhood=="rook"){
    edges.weight =edges.w2
  }else{
    #bishop's case - multiply weights by 2^0.5
    if(neighbourhood=="octagon"){
      # based on length of diagonal of a square
      mW = 2^0.5
    }else{mW=1}
    weight$weight = weight$weight*mW
    edges<-SpaDES.tools::adj(returnDT= TRUE, numCol = ncol(ras.matrix), numCell=ncol(ras.matrix)*nrow(ras.matrix), directions ="bishop", cells = 1:as.integer(ncol(ras.matrix)*nrow(ras.matrix)))
    edges<-data.table::data.table(edges)
    
    #edges[from < to, c("from", "to") := .(to, from)]
    edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]
    edges<-unique(edges)
    
    edges.w1<-merge(x=edges, y=weight, by.x= "from", by.y ="id") #merge in the weights from a cost surface
    data.table::setnames(edges.w1, c("from", "to", "w1")) #reformat
    
    edges.w3<-data.table::setDT(merge(x=edges.w1, y=weight, by.x= "to", by.y ="id"))#merge in the weights to a cost surface
    data.table::setnames(edges.w3, c("from", "to", "w1", "w2")) #reformat
    edges.w3$weight<-(edges.w3$w1 + edges.w3$w2)/2 #take the average cost between the two pixels
    
    #------get the edges list
    edges.weight =rbind(edges.w2,edges.w3)
  }
  
  edges.weight<-edges.weight[stats::complete.cases(edges.weight), c(1:2, 5)] #get rid of NAs caused by barriers. Drop the w1 and w2 costs.
  edges.weight[, id := seq_len(.N)] #set the ids of the edge list. Faster than using as.integer(row.names())
  
  #------make the graph
  sim$g<-igraph::graph.edgelist(as.matrix(edges.weight)[,1:2], dir = FALSE) #create the graph using to and from columns. Requires a matrix input
  igraph::E(sim$g)$weight<-as.matrix(edges.weight)[,3]#assign weights to the graph. Requires a matrix input
  
  #------clean up
  rm(edges.w1,edges.w2,edges.w3, edges, weight, ras.matrix)#remove unused objects
  #gc() #garbage collection
  return(invisible(sim))
}

# try buildSimList

simList <- buildSimList(roads = demoScen[[1]]$road.rast, 
                        cost = demoScen[[1]]$cost.rast,
                        landings = demoScen[[1]]$landings.points, 
                        roadMethod = "mst")

simListSF <- buildSimList(roads = demoScen[[1]]$road.rast, 
                          cost = demoScen[[1]]$cost.rast,
                          landings = demoScen[[1]]$landings.points %>%
                            sf::st_as_sf(), 
                          roadMethod = "snap")

simListSF2 <- buildSimList(roads = demoScen[[1]]$road.line, 
                           cost = demoScen[[1]]$cost.rast,
                           landings = demoScen[[1]]$landings.points %>%
                             sf::st_as_sf(), 
                           roadMethod = "snap")
out <- getClosestRoad(simList)

# raster to lines using polygon and - buffer
rdRast <- demoScen[[1]]$road.rast

rdCont <- raster::rasterToContour(rdRast, nlevels = 1) %>% 
  sf::st_as_sf()

rdLine <- sf::st_simplify(rdCont, dTolerance = 4)

