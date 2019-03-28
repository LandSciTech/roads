#simple example test case - compare to results from kylesCLUSExample.Rmd
library(igraph)
library(SpaDES)
library(raster)
library(data.table)
library(rgeos)
library(sf)
library(dplyr)
#Empty raster0
x.size = 5
y.size = 5
ras = raster(extent(0, x.size, 0, y.size),res =1, vals =1)
set.seed(1)
ras[]<-runif(as.integer(x.size*x.size), 1,20)
ras[1:5]<-0
plot(ras)
title('Cost Surface')
startCells <- xyFromCell(ras, as.integer(sample(11:ncell(ras), 4)), spatial=FALSE)
sC<-SpatialPoints(startCells)
sC$ID<-paste(1:4, sep = "")
plot(sC, col ='red', add=TRUE)
segments(0,5,5,5, lwd =2)
text(sC, labels=sC$ID, pos=2)
lines(c(0,5),c(4.5,4.5), lwd =2)

cost=ras
sC$ID = as.numeric(sC$ID)
landings=sC
roads = ras==0
plot(roads)

devtools::install_github("LandSciTech/roads")
library(roads)
roadMethod="snap"
outRoads = projectRoads(landings=landings,cost=cost,roads=roads,roadMethod=roadMethod,plotRoads=T)

pdf("roadNetworkGrowthSnap.pdf")
plot(outRoads,col="black")
dev.off()

#TASKS
# Get Scott to help setup git/github/local copy of repository. Workflow is [pull, do stuff, commit, push] each time you work on the project.
# Formally (return TRUE/FALSE) compare to Kyle's output - confirm methods are returning the same thing
# Repeat for "mst" and "lcp" roadMethods
# Set up as formal test suite: http://r-pkgs.had.co.nz/tests.html
# Add examples to projectRoads documentation - Josie will explain how to when you get to this step
