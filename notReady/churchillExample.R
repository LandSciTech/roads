# Example with Churchill data
library(dplyr)
library(sf)
devtools::load_all(".")

# also need installed raster and stars packages

# Download the ChurchillData.zip and ChurchillRoadsHarv.zip from here:
# https://drive.google.com/drive/u/0/folders/155ZceLOw_-519NCHNOYQgeiQEIe_Qqca

# path to folder where you unzipped the Churchill data
data_path <- "../ChurchillAnalysis/inputNV/ChurchillData/"

# Note this road data was separated into 2010 and 2020 based on Year constructed
# in the MNRF data which is incomplete and not very accurate. Timing is VERY
# APPROXIMATE
roads <- st_read(paste0(data_path, "road_ORNMNRFCH2010.shp"))

roads2020 <- st_read(paste0(data_path, "road_ORNMNRFCH2020.shp"))

# provincial land cover data
plc <- raster::raster(paste0(data_path, "plc50.tif"))

# harvest polygons with year of harvest = yrdep
harvCH <- st_read(paste0(data_path, "ChurchillRoadsHarv/harvPCIFRI.shp"))

plc <- raster::aggregate(plc, fact = 5, fun = raster::modal)
# make cost from plc
# conversion table from plc class to cost
# TODO: These are made up costs, will need to define real ones with reasons.
plcToCost <- tribble(
  ~plcClass, ~cost,
  1:2, 0.9, # water
  3, 0, # infrastructure/settlement,
  4, 1, #  quarry/mine tailings
  5, 0.25, # bedrock
  6, 0.8, # mudflats
  7:8, 0.25, # cuts, burns
  9, 0.6, # regen depletion
  10, 0.3, # sparse forest
  11:13, 0.5, # dense forest
  15:23, 0.8, # marsh, swamp, fen, bog
  24:27, 0.25, # tundra, agriculture
  28:29, 0.5 # other, assume forest
) %>% tidyr::unnest(cols = c(plcClass)) %>% as.matrix()

cost <- raster::reclassify(plc, plcToCost)

# make roads 0 cost
# very slow!
# cost2 <- raster::mask(cost, roads, updatevalue = 0)

# use stars package instead
cost_st <- stars::st_as_stars(cost)

# rasterize roads to template
tmplt <- stars::st_as_stars(st_bbox(cost_st), nx = raster::ncol(cost), 
                            ny = raster::nrow(cost), values = 1)

roads_st <- stars::st_rasterize(roads %>% select(-YEAR_CO), template = tmplt, 
                                options = "ALL_TOUCHED=TRUE") == 1

# road raster is 1 where there are no roads and 0 where there are roads
cost_st <- cost_st * roads_st

# convert back to Raster from stars
cost_st <- as(cost_st, "Raster")

# pick landings as points on the surface
harvCH2010 <- harvCH %>% filter(yrdep == 2010) %>% 
  st_point_on_surface()

# transform to the same crs
roads <- roads %>%  st_transform(st_crs(cost_st))
harvCH2010 <- harvCH2010 %>%  st_transform(st_crs(cost_st))

# run projection of roads to connect landings to existing road network
# mst
roadsProjCH2010 <- projectRoadsNew(landings = harvCH2010, 
                                   cost = cost_st, 
                                   roads = roads)

# lcp
roadsProjCH2010lcp <- projectRoadsNew(landings = harvCH2010, 
                                   cost = cost_st, 
                                   roads = roads, roadMethod = "lcp")

# snap
roadsProjCH2010snap <- projectRoadsNew(landings = harvCH2010, 
                                   cost = cost_st, 
                                   roads = roads, roadMethod = "snap")

# Look at maps in interactive mode, change to "plot" for non-interactive
tmap::tmap_mode("view")

tmap::qtm(roadsProjCH2010$costSurface)+
  tmap::qtm(roadsProjCH2010$landings, dots.col = "red")+
  tmap::qtm(roadsProjCH2010$roads, lines.col = "red")+
  tmap::qtm(roads)
# you might want to add these layers to compare to the polygons or 2020 roads
  #tmap::qtm(harvCH2 %>% filter(yrdep == 2010))+
  #tmap::qtm(roads2020)

# lcp
tmap::qtm(roadsProjCH2010lcp$costSurface)+
  tmap::qtm(roadsProjCH2010lcp$landings, dots.col = "red")+
  tmap::qtm(roadsProjCH2010lcp$roads, lines.col = "red")+
  tmap::qtm(roads)

# snap
tmap::qtm(roadsProjCH2010lcp$costSurface)+
  tmap::qtm(roadsProjCH2010lcp$landings, dots.col = "red")+
  tmap::qtm(roadsProjCH2010lcp$roads, lines.col = "red")+
  tmap::qtm(roads)
