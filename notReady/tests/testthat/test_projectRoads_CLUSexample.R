context("simple example test case - compare to results from kylesCLUSExample.Rmd")
library(sp)
library(raster)
library(roads)
library(testthat)
###########################################
# basic test case from kylesCLUSExample.Rmd
#   - 5 by 5 raster representing cost, populated with uniform random numbers based on seed value 1
#   - first/top row (cost[1:5]) identified as existing roads (cost is 0 for these cells)
#   - 4 landings are cells: 11, 13, 22, 25
##############################################
# hard code expected results from CLUS example
CLUS.snap.roads <- raster::raster(raster::extent(0, 5, 0, 5),res=1,vals=0)
CLUS.snap.roads[c(1:25)[-c(9,14,16,18,19,21,23,24)]] <- 1    # expected results from 'snap' method
CLUS.lcp.roads <- raster::raster(raster::extent(0, 5, 0, 5),res=1,vals=0)
CLUS.lcp.roads[c(1:25)[-c(6,7,9,15,17,18,20,21,23,24)]] <- 1 # expected results from 'lcp' method
CLUS.mst.roads <- raster::raster(raster::extent(0, 5, 0, 5),res=1,vals=0)
CLUS.mst.roads[c(1:25)[-c(6:9,15,17,18,20,21,23,24)]] <- 1   # expected results from 'mst' method
###############################################
# generate the results from roads::projectRoads
landings <- roads::CLUSexample$landings
cost     <- roads::CLUSexample$cost
roads    <- roads::CLUSexample$roads

pR.snap.roads <- roads::projectRoads(landings=landings, cost=cost, roads=roads, roadMethod="snap", plotRoads=T, sim=list())$roads
pR.snap.roads[pR.snap.roads>0] <- 1
pR.lcp.roads  <- roads::projectRoads(landings=landings, cost=cost, roads=roads, roadMethod="lcp",  plotRoads=T, sim=list())$roads
pR.lcp.roads[pR.lcp.roads>0]   <- 1
pR.mst.roads  <- roads::projectRoads(landings=landings, cost=cost, roads=roads, roadMethod="mst",  plotRoads=T, sim=list())$roads
pR.mst.roads[pR.mst.roads>0]   <- 1
###############################################
# perform tests
#TO DO: fix snap
#testthat::test_that("Projected roads results match CLUS example results for the 'snap' method",{
#  testthat::expect_true(raster::all.equal(CLUS.snap.roads,pR.snap.roads,showwarning=FALSE))
#})
testthat::test_that("Projected roads results match CLUS example results for the 'lcp' method",{
  testthat::expect_true(raster::all.equal(CLUS.lcp.roads,pR.lcp.roads,showwarning=FALSE))
})
testthat::test_that("Projected roads results match CLUS example results for the 'mst' method",{
  testthat::expect_true(raster::all.equal(CLUS.mst.roads,pR.mst.roads,showwarning=FALSE))
})
###############################################
# end of tests
