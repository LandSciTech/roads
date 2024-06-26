## code to prepare `dem_example` dataset goes here
library(roads)
library(dplyr)
library(terra)

dir <- tempdir()
can_elev <- geodata::elevation_30s("CAN", dir)
wrld_lc <- geodata::landcover("water", dir)
plot(can_elev)

# ext <- terra::draw()
# dput(as.vector(ext))

ext <- c(xmin = -118.490016655244, xmax = -115.954077203458, ymin = 49.4109306683323, 
         ymax = 50.846908182386)

ex_elev <- crop(can_elev, ext)
ex_lc <- crop(wrld_lc, ext)

plot(ex_elev)
plot(ex_lc > 0.2)



tpi <- ex_elev %>% terra::terrain(v = "TPI")

# Try TPI over larger window
TPI <- focal(ex_elev, w=9, fun=\(x) x[41] - mean(x[-41]))

# thing is at this resolution crossing the river and following the river are
# going to both be in the higher cost area which isn't correct...

plot(TPI < -400 |ex_lc > 0.1)

dem_example <- lst(ex_elev, ex_wat = ex_lc)

dem_example <- lapply(dem_example, wrap)

usethis::use_data(dem_example, overwrite = TRUE)
