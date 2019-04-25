library(sp)
library(raster)
###########################################
# basic test case from kylesCLUSExample.Rmd
#   - 5 by 5 raster representing cost, populated with uniform random numbers based on seed value 1
#   - first/top row (cost[1:5]) identified as existing roads (cost is 0 for these cells)
#   - 4 landings are cells: 11, 13, 22, 25
cost       <- raster::raster(raster::extent(0, 5, 0, 5),res=1,vals =1) # define cost raster
# hard code the random cost values to avoid having random seed being modified
#   - values are the same as running:  set.seed(1);runif(25,1,20)
cost[]     <- c( 6.0446645996998996, 8.0703540930990130,11.8842139036860317,18.2559480099007487, 4.8319566897116601,
                 18.0694040143862367,18.9488301035016775,13.5551580572500825,12.9531668340787292, 2.1739391388837248,
                 4.9135169230867177, 4.3545782980509102,14.0534340864978731, 8.2979706460610032,15.6269869799725711,
                 10.4562855996191502,14.6347516570240259,19.8462158017791808, 8.2206684092525393,15.7714592050760984,
                 18.7593993910122663, 5.0307079043705016,13.3818015556316823, 3.3855468232650310, 6.0771927058231086 )
cost[1:5]  <- 0   # cells 1 to 5 (first/top row) are existing roads, so set the cost of these cells to zero
startCells <- raster::xyFromCell(cost, as.integer(c(11,13,22,25)), spatial=FALSE) # define start cells/landings as cells 11,13,22,25
landings   <- sp::SpatialPoints(startCells)  # coerce startCells/landings to SpatialPoints, sC
##############################################
CLUSexample <- list(cost=cost,landings=landings,roads=(cost==0))
##############################################
save(CLUSexample,file="data/CLUSexample.rda")
