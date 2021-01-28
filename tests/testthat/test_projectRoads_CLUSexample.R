context("simple example test case - compare to results from kylesCLUSExample.Rmd")

###########################################
# basic test case from kylesCLUSExample.Rmd
#   - 5 by 5 raster representing cost, populated with uniform random numbers based on seed value 1
#   - first/top row (cost[1:5]) identified as existing roads (cost is 0 for these cells)
#   - 4 landings are cells: 11, 13, 22, 25
##############################################
# hard code expected results from CLUS example

CLUS.snap.roads <- c(1:25)[-c(9,14,16,18,19,21,23,24)]  

CLUS.lcp.roads <- c(1:25)[-c(6,7,9,15,17,18,20,21,23,24)] 

CLUS.mst.roads <- c(1:25)[-c(6:9,15,17,18,20,21,23,24)] 
###############################################
# generate the results from roads::projectRoadsNew
landingsC <- CLUSexample$landings
costC     <- CLUSexample$cost
roadsC    <- CLUSexample$roads

pR_snap <- projectRoadsNew(landings = landingsC, 
                           cost = costC,
                           roads = roadsC,
                           roadMethod = "snap")

pR_lcp <- projectRoadsNew(landings = landingsC,
                          cost = costC,
                          roads = roadsC,
                          roadMethod = "lcp")

pR_mst <- projectRoadsNew(landings = landingsC,
                          cost = costC,
                          roads = roadsC,
                          roadMethod="mst")

getRoadCells <- function(rast, roads){
  raster::cellFromLine(rast, 
                       sf::as_Spatial(roads) %>%
                         sp::as.SpatialLines.SLDF()) %>%
    unlist() %>% unique() %>% sort()
}
###############################################
# perform tests
#TO DO: fix snap
testthat::test_that("Projected roads results match CLUS example results for the 'snap' method",{
  testthat::expect_equal(getRoadCells(costC, pR_snap$roads), CLUS.snap.roads)
})
testthat::test_that("Projected roads results match CLUS example results for the 'lcp' method",{
  testthat::expect_equal(getRoadCells(costC, pR_lcp$roads), CLUS.lcp.roads)
})
testthat::test_that("Projected roads results match CLUS example results for the 'mst' method",{
  testthat::expect_equal(getRoadCells(costC, pR_mst$roads), CLUS.mst.roads)
})
###############################################
# end of tests

# get total cost from CLUS example vs ours
sum(costC[CLUS.lcp.roads])
sum(costC[getRoadCells(costC, pR_lcp$roads)])

sum(costC[CLUS.mst.roads])
sum(costC[getRoadCells(costC, pR_mst$roads)])
