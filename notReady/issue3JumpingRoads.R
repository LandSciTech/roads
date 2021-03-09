# Explore disconnected existing roads #==================
# run churchillExample.R first 

harvCH2010 <- st_read("../ChurchillAnalysis/inputNV/ChurchillData/ChurchillRoadsHarv/harvPCIFRI.shp")
harvCH2010 <- harvCH2010 %>% filter(yrdep == 2010) %>% 
  st_point_on_surface()
harvCH2010 <- harvCH2010 %>%  st_transform(st_crs(cost_st))
#using data pulled from the top left of the churchill example
plot(roads %>% st_geometry())
#ext <- raster::drawExtent()

# class      : Extent 
# xmin       : 343502.5 
# xmax       : 441107.8 
# ymin       : 12708389  
# ymax       : 12766271 
ext <- raster::extent(c(343502.5, 441107.8, 12708389, 12766271))

roadsTest <- st_crop(roads, ext)

landingsTest <- st_crop(harvCH2010, ext) %>% mutate(ID = 1:n())

costTest <- raster::crop(cost_st, ext)

roadsProjTest <- projectRoadsNew(landings = landingsTest, 
                                 cost = costTest, 
                                 roads = roadsTest)

tmap::qtm(roadsProjTest$costSurface)+
  tmap::qtm(roadsProjTest$landings, dots.col = "red")+
  tmap::qtm(roadsProjTest$roads, lines.col = "red")+
  tmap::qtm(roadsTest)
#tmap::qtm(roads)+
#tmap::qtm(harvCH2 %>% filter(yrdep == 2010))+
#tmap::qtm(roads2020)

# checked and the issue is the same with input as raster

# points connected to weird chunk of road are 52:54, and 40:43 try with just those points
roadsProjTest2 <- projectRoadsNew(landings = landingsTest %>% slice(12:20, 52:54, 40:43), 
                                  cost = costTest, 
                                  roads = roadsTest)

tmap::qtm(roadsProjTest2$costSurface)+
  tmap::qtm(roadsProjTest2$landings, dots.col = "red")+
  tmap::qtm(roadsProjTest2$roads, lines.col = "red")+
  tmap::qtm(roadsTest)
# Does a long straight line crossing existing road to connect the two clumps.
# this is probably related to the conn and keep part of pathsToLine

# go into functions
landings = landingsTest %>% slice(12:20, 52:54, 40:43)
cost = costTest
roads = roadsTest

# Noticed that landings that are within 250m (ie raster res) are building long
# roads for no reason. These should not be included in but should 