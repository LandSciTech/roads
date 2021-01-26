# Example with Churchill data
library(dplyr)
library(sf)
devtools::load_all(".")

data_path <- "../ChurchillAnalysis/inputNV/ChurchillData/"

roads <- st_read(paste0(data_path, "road_ORNMNRFCH2010.shp"))

plc <- raster::raster(paste0(data_path, "plc50.tif"))

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

tmplt <- stars::st_as_stars(st_bbox(cost_st), nx = raster::ncol(cost), 
                            ny = raster::nrow(cost), values = 1)

roads_st <- stars::st_rasterize(roads %>% select(-YEAR_CO), template = tmplt, 
                                options = "ALL_TOUCHED=TRUE") == 1

cost_st <- cost_st * roads_st

# convert back to Raster from stars
cost_st <- as(cost_st, "Raster")

# make landings based on fri
st_layers("../ChurchillAnalysis/inputNV/FRI/FRI_PCI_2020/fripci2020.gdb")

# trout lake = 120, Caribou = 175, Lac Seul 702
friTL <- st_read("../ChurchillAnalysis/inputNV/FRI/FRI_PCI_2020/fripci2020.gdb", "f120")
friCar <- st_read("../ChurchillAnalysis/inputNV/FRI/FRI_PCI_2020/fripci2020.gdb", "f175")
friLS <- st_read("../ChurchillAnalysis/inputNV/FRI/FRI_PCI_2020/fripci2020.gdb", "f702")

friCH <- rbind(friTL, friCar) %>% rbind(friLS)

harvCH <- filter(friCH, deptype == "HARVEST") %>% select(objid, yrdep)

harvCH2 <- harvCH %>% group_by(yrdep) %>% 
  summarise(objid = first(objid))

# pick landings as points on the surface
harvCH2010 <- harvCH2 %>% filter(yrdep == 2010) %>% st_cast("POLYGON") %>% 
  st_point_on_surface()

rm(plc, friCH, friLS, friTL, friCar, harvCH, cost, roads_st, tmplt, harvCH2)

harvCH2010 <-  harvCH2010 %>% st_transform(st_crs(cost_st))
roads <- roads %>%  st_transform(st_crs(cost_st))

roadsProjCH2010 <- projectRoadsNew(landings = harvCH2010, 
                                   cost = cost_st, 
                                   roads = roads)

# 1,2 ,4 don't work but 3 does, what is the difference?
tmap::tmap_mode("view")

tmap::qtm(cost_st)+
  tmap::qtm(roads)+
  tmap::qtm(harvCH2010 %>% slice(c(88, 89, 272)) %>% mutate(ID = 1:n()), col = "ID")
