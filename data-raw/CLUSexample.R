# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#NOTICE: This code has been modified from https://github.com/bcgov/clus/blob/master/R/SpaDES-modules/roadCLUS/roadCLUS.R


###########################################
# basic test case from kylesCLUSExample.Rmd
#   - 5 by 5 raster representing cost, populated with uniform random numbers based on seed value 1
#   - first/top row (cost[1:5]) identified as existing roads (cost is 0 for these cells)
#   - 4 landings are cells: 11, 13, 22, 25
cost       <- terra::rast(terra::ext(0, 5, 0, 5),res=1,vals =1, crs="EPSG:3347") # define cost raster
# hard code the random cost values to avoid having random seed being modified
#   - values are the same as running:  set.seed(1);runif(25,1,20)
cost[]     <- c( 6.0446645996998996, 8.0703540930990130,11.8842139036860317,18.2559480099007487, 4.8319566897116601,
                 18.0694040143862367,18.9488301035016775,13.5551580572500825,12.9531668340787292, 2.1739391388837248,
                 4.9135169230867177, 4.3545782980509102,14.0534340864978731, 8.2979706460610032,15.6269869799725711,
                 10.4562855996191502,14.6347516570240259,19.8462158017791808, 8.2206684092525393,15.7714592050760984,
                 18.7593993910122663, 5.0307079043705016,13.3818015556316823, 3.3855468232650310, 6.0771927058231086 )
cost[1:5]  <- 0   # cells 1 to 5 (first/top row) are existing roads, so set the cost of these cells to zero
startCells <- terra::xyFromCell(cost, as.integer(c(11,13,22,25))) # define start cells/landings as cells 11,13,22,25
landings   <- sf::st_as_sf(as.data.frame(startCells), coords = c("x", "y"), 
                           crs = sf::st_crs(cost), agr = "constant") # coerce startCells/landings to SpatialPoints, sC
##############################################
# terra::writeRaster(cost, "inst/extData/CLUScost.tif")
# terra::writeRaster(cost == 0, "inst/extData/CLUSroads.tif")
CLUSexample <- list(cost=terra::wrap(cost),landings=landings,roads=terra::wrap(cost == 0))
##############################################
usethis::use_data(CLUSexample, overwrite = TRUE)
