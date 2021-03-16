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
                           roadMethod = "snap", roadsOut = "sf")

pR_lcp <- projectRoadsNew(landings = landingsC,
                          cost = costC,
                          roads = roadsC,
                          roadMethod = "lcp", 
                          neighbourhood = "queen", roadsOut = "sf")

pR_mst <- projectRoadsNew(landings = landingsC,
                          cost = costC,
                          roads = roadsC,
                          roadMethod="mst", 
                          neighbourhood = "queen", roadsOut = "sf")

getRoadCells <- function(rast, roads, method){
  if(method == "snap"){
    raster::extract(rast, 
                    roads %>% 
                      sf::st_segmentize(dfMaxLength = raster::xres(costC)) %>% 
                      sf::st_cast("MULTIPOINT") %>%
                      sf::st_cast("POINT"), 
                    cellnumbers = T) %>% 
      .[,1] %>% unique() %>% sort()
  } else {
    raster::extract(rast, 
                    roads %>% 
                      sf::st_cast("MULTIPOINT") %>%
                      sf::st_cast("POINT"), 
                    cellnumbers = T) %>% 
      .[,1] %>% unique() %>% sort()
  }
  
}
###############################################
# perform tests
testthat::test_that("Projected roads results match CLUS example results for the 'snap' method",{
  testthat::expect_equal(getRoadCells(costC, pR_snap$roads, "snap"), CLUS.snap.roads)
})
testthat::test_that("Projected roads results match CLUS example results for the 'lcp' method",{
  testthat::expect_equal(getRoadCells(costC, pR_lcp$roads, "lcp"), CLUS.lcp.roads)
})
testthat::test_that("Projected roads results match CLUS example results for the 'mst' method",{
  testthat::expect_equal(getRoadCells(costC, pR_mst$roads, "mst"), CLUS.mst.roads)
})
###############################################
# end of tests

