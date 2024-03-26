## code to prepare `hexlogo` goes here

library(tmap)
library(sf)
library(dplyr)

# temp file
temp_fn <- tempfile()

demoScen <- prepExData(demoScen)
scen <- demoScen[[2]]

ext <- raster::extent(x = 19.35725,
                      xmax = 83.22462,
                      ymin = 16.67346,
                      ymax = 90.89971 )

# landing set 1 of scenario 1:
land.pnts <- scen$landings.points %>% raster::crop(ext) %>% 
  st_as_sf() %>% filter(set %in% 4:7) %>% 
  st_set_crs(5070)

ex_roads <- scen$road.line %>% st_as_sf() %>% st_set_crs(5070)

scen$cost.rast <- raster::`crs<-`(scen$cost.rast, value = 5070)

prRes <- projectRoads(land.pnts, scen$cost.rast, ex_roads, "mst",
                         plotRoads = TRUE)

# 0s look ugly so smooth out
prRes$weightRaster[prRes$weightRaster == 0] <- NA

prRes$weightRaster <- raster::focal(prRes$weightRaster, w=matrix(1, nc=5, nr=5),
                                   fun = "mean",
                                   na.rm = TRUE,
                                   NAonly = TRUE)

mp <- tm_shape(prRes$weightRaster)+
  tm_raster(style = "cont", legend.show = FALSE)+
  tm_shape(prRes$roads)+
  tm_lines(lwd = 1)+
  tm_shape(prRes$landings)+
  tm_symbols(size = 0.1, shape = 1, col = "black")+
  tm_layout(frame = FALSE)

mp <- tmap_grob(mp)

s <- hexSticker::sticker(mp, package = "roads",
        p_size = 12, # This seems to behave very differently on a Mac vs PC
        p_y = 0.3, 
        p_color = "black", 
        s_x = 1, s_y = 1,
        s_width = 1.8, s_height = 2,
        h_fill = "white",
        h_color = "black",
        white_around_sticker = TRUE,
        filename = paste0(temp_fn, ".png"))

plot(s)

use_logo(paste0(temp_fn, ".png"))
