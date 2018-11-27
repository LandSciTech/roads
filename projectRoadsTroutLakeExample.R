library(rsyncrosim)
library(raster)
#library(roads)

#############
# Paths and parameters
roadPath = "C:/Users/hughesjo/Documents/InitialWork/RPackage/v1Misc/Roads/troutLakeRoadRaster.tif"
libPath = "C:/Users/hughesjo/Documents/InitialWork/RPackage/v1Misc/Libraries/Trout Lake Forest"
scnId = 12746
harvestGroupName="Harvest";iteration=1;timestep=1:3

########
# Get handle on syncrosim simulation results, as an example
myLibrary = ssimLibrary(name=libPath) #open library
myProject = project(myLibrary,project="2009 Plan")
myResult = scenario(myProject,scenario=scnId,forceElements=T)

###########
#Get layers for road network projection
initialRoads=raster(roadPath)
myStratum = datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="StratumFileName")
newBlocks = datasheetRaster(myResult[[1]],datasheet="STSim_OutputSpatialTransition",timestep=timestep,iteration=iteration,subset=expression(TransitionGroupID==harvestGroupName))
tag = paste(c(strsplit(names(newBlocks)[[1]],".",fixed=T)[[1]][1:2],"ts"),collapse=".")
newBlocks = adjustBrickNames(newBlocks,ctag=tag)

water = myStratum-1
water[is.na(water)]=1000
cost = simpleCost(initialRoads,newBlocks[[1]],water)

#############
#Project road network
roadMethod="snap"
outRoads = projectRoads(landings=newBlocks,cost=cost,roads=initialRoads,roadMethod=roadMethod)
pdf("roadNetworkGrowthSnap.pdf")
plot(outRoads,col="black")
dev.off()

roadMethod="lcp"
outRoads = projectRoads(landings=newBlocks,cost=cost,roads=initialRoads,roadMethod=roadMethod)
pdf("roadNetworkGrowthLcp.pdf")
plot(outRoads,col="black")
dev.off()

roadMethod="mst"
outRoads = projectRoads(landings=newBlocks,cost=cost,roads=initialRoads,roadMethod=roadMethod)
pdf("roadNetworkGrowthLcp.pdf")
plot(outRoads,col="black")
dev.off()
