context("simple example test case - compare to results from kylesCLUSExample.Rmd")
library(sp)
library(raster)
library(roads)
library(testthat)
###########################################
# basic test case from kylesCLUSExample.Rmd
#   - 5 by 5 raster representing cost, populated with uniform random numbers based on seed value 1
#   - first/top row (ras[1:5]) identified as existing roads (cost is 0 for these cells)
#   - 4 startCells (sC), a.k.a. landings, are cells: 11, 13, 22, 25
ras        <- raster:::raster(raster:::extent(0, 5, 0, 5),res=1,vals =1) # define cost raster
# hard code the random cost values to avoid having random seed being modified in testing
#   - values are the same as running:  set.seed(1);runif(25,1,20)
ras[]      <- c( 6.0446645996998996, 8.0703540930990130,11.8842139036860317,18.2559480099007487, 4.8319566897116601,
                18.0694040143862367,18.9488301035016775,13.5551580572500825,12.9531668340787292, 2.1739391388837248,
                 4.9135169230867177, 4.3545782980509102,14.0534340864978731, 8.2979706460610032,15.6269869799725711,
                10.4562855996191502,14.6347516570240259,19.8462158017791808, 8.2206684092525393,15.7714592050760984,
                18.7593993910122663, 5.0307079043705016,13.3818015556316823, 3.3855468232650310, 6.0771927058231086 )
ras[1:5]   <- 0   # cells 1 to 5 (first/top row) are existing roads, so set the cost of these cells to zero
startCells <- raster:::xyFromCell(ras, as.integer(c(11,13,22,25)), spatial=FALSE) # define start cells/landings as cells 11,13,22,25
sC         <- sp:::SpatialPoints(startCells)  # coerce startCells/landings to SpatialPoints, sC
##############################################
# hard code expected results from CLUS example
CLUS.snap.roads <- raster:::raster(raster:::extent(0, 5, 0, 5),res=1,vals=0)
CLUS.snap.roads[c(1:25)[-c(9,14,16,18,19,21,23,24)]] <- 1    # expected results from 'snap' method
CLUS.lcp.roads <- raster:::raster(raster:::extent(0, 5, 0, 5),res=1,vals=0)
CLUS.lcp.roads[c(1:25)[-c(6,7,9,15,17,18,20,21,23,24)]] <- 1 # expected results from 'lcp' method
CLUS.mst.roads <- raster:::raster(raster:::extent(0, 5, 0, 5),res=1,vals=0)
CLUS.mst.roads[c(1:25)[-c(6:9,15,17,18,20,21,23,24)]] <- 1   # expected results from 'mst' method
###############################################
# generate the results from roads::projectRoads, each with both options (TRUE/FALSE) for plotRoads
pR.snap.roads <- roads:::projectRoads(landings=sC, cost=ras, roads=(ras==0),roadMethod="snap", plotRoads=T, sim=list())$roads
pR.snap.roads[pR.snap.roads>0] <- 1
pR.lcp.roads  <- roads:::projectRoads(landings=sC, cost=ras, roads=(ras==0),roadMethod="lcp",  plotRoads=T, sim=list())$roads
pR.lcp.roads[pR.lcp.roads>0]   <- 1
pR.mst.roads  <- roads:::projectRoads(landings=sC, cost=ras, roads=(ras==0),roadMethod="mst",  plotRoads=T, sim=list())$roads
pR.mst.roads[pR.mst.roads>0]   <- 1
###############################################
# perform tests
testthat:::test_that("Projected roads results match CLUS example results for the 'snap' method",{
  testthat:::expect_true(raster:::all.equal(CLUS.snap.roads,pR.snap.roads,showwarning=FALSE))
})
testthat:::test_that("Projected roads results match CLUS example results for the 'lcp' method",{
  testthat:::expect_true(raster:::all.equal(CLUS.lcp.roads,pR.lcp.roads,showwarning=FALSE))
})
testthat:::test_that("Projected roads results match CLUS example results for the 'mst' method",{
  testthat:::expect_true(raster:::all.equal(CLUS.mst.roads,pR.mst.roads,showwarning=FALSE))
})
###############################################
# end of tests
