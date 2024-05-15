# commenting to avoid R cmd warnings due to missing testing packages
# if(FALSE){
#   library(terra)
#   library(dplyr)
#   library(sf)
#   #library(roads)
#   library(igraph)
#   library(ggplot2)
#   devtools::document();devtools::load_all()
# 
#   ## colours for displaying weight raster
#   if(requireNamespace("viridis", quietly = TRUE)){
#     # Use colour blind friendly palette if available
#     rastColours <- c('grey50', viridis::viridis(20))
#   } else {
#     rastColours <- c('grey50', terrain.colors(20))
#   }
# 
#   CLUSexample <- prepExData(CLUSexample)
# 
#   weightRaster <- CLUSexample$cost
# 
#   roadsLine <- sf::st_sfc(geometry = sf::st_linestring(
#     matrix(c(0.5, 4.5, 4.5, 4.5),
#            ncol = 2, byrow = T)
#   )) %>%
#     sf::st_as_sf(crs = sf::st_crs(weightRaster))
# 
# 
#   landings <- roads::CLUSexample$landings
# 
#   ## plot example scenario
#   plot(weightRaster, col = rastColours, main = 'Example Scenario')
#   plot(roadsLine, add = TRUE)
#   plot(landings, add = TRUE, pch = 19)
#   points(x=5.6,y=4.5,pch=19,xpd=TRUE)
#   text(x=5.8,y=4.5,labels='landing',adj=c(0,0.4),xpd=TRUE)
#   lines(x=c(5.3,5.6),y=c(4.2,4.2),lwd=2,xpd=TRUE)
#   text(x=5.75,y=4.2,labels='roads',adj=c(0,0.3),xpd=TRUE)
# 
#   sim = list(weightRaster=weightRaster)
# 
#   g1$g = getGraph(sim,"queen")
#   gNew$g = getGraph(sim,"queen",method="gdistance")
#   igraph::identical_graphs(g1$g,gNew$g,attrs=F)
# 
#   edge_attr(g1$g,"weight")=round(edge_attr(g1$g,"weight"),5)
#   edge_attr(gNew$g,"weight")=round(edge_attr(gNew$g,"weight"),5)
# 
#   edge_attr(g1$g,"weight")==edge_attr(gNew$g,"weight")
#   igraph::identical_graphs(g1$g,gNew$g,attrs=T)
#   #queen and rook methods yield identical graphs, up to rounding errors.
# 
#   g1$g = getGraph(sim,"octagon")
#   gNew$g = getGraph(sim,"octagon",method="gdistance")
#   igraph::identical_graphs(g1$g,gNew$g,attrs=F)
# 
#   edge_attr(g1$g,"weight")=round(edge_attr(g1$g,"weight"),0)
#   edge_attr(gNew$g,"weight")=round(edge_attr(gNew$g,"weight"),0)
# 
#   edge_attr(g1$g,"weight")==edge_attr(gNew$g,"weight")
#   igraph::identical_graphs(g1$g,gNew$g,attrs=T)
#   #octagon methods yield similar but not identical graphs because gdistance::geoCorrection
#   #method accounts for lengths of diagonals and other geographic distortions on the grid.
# 
#   #speed/memory benchmarking
#   data_path_raw <- "~/Documents/gitprojects/RoadPaper/analysis/data/raw_data/"
#   out_path <- "~/Documents/gitprojects/RoadPaper/analysis/figures/"
# 
#   landscape <- rast(paste0(data_path_raw, "cost_surface_bc_ha.tif"))
# 
#   base_point <- c((1881188-159588)/2, (1748188-173788)/2)
#   the_res <- res(landscape)[1]
#   ext_100 <- ext(c(base_point, base_point+100*the_res)[c(1,3,2,4)])
#   landscape_100 <- list(weightRaster=crop(landscape, ext_100))
#   ext_500 <- ext(c(base_point, base_point+500*the_res)[c(1,3,2,4)])
#   landscape_500 <- list(weightRaster=crop(landscape, ext_500))
#   ext_1000 <- ext(c(base_point, base_point+1000*the_res)[c(1,3,2,4)])
#   landscape_1000 <- list(weightRaster=crop(landscape, ext_1000))
#   ext_2000 <- ext(c(base_point, base_point+200*the_res)[c(1,3,2,4)])
#   landscape_2000 <- list(weightRaster=crop(landscape, ext_2000))
# 
#   rr = getGraph(landscape_100,neighb,method="old",weightFunction=gradePenaltyFn,limitWeight=65000)
# 
#   neighb  ="octagon"
#   bm <- bench::mark(min_iterations = 1, check = FALSE,
#                     old_100 = getGraph(landscape_100,neighb,method="old"),
#                     dem_100 = getGraph(landscape_100,neighb,method="old",weightFunction = gradePenaltyFn),
#                     gdistance_100 = getGraph(landscape_100,neighb,method="gdistance"),
#                     old_500 = getGraph(landscape_500,neighb,method="old"),
#                     dem_500 = getGraph(landscape_500,neighb,method="old",weightFunction=gradePenaltyFn),
#                     gdistance_500 = getGraph(landscape_500,neighb,method="gdistance"),
#                     old_1000 = getGraph(landscape_1000,neighb,method="old"),
#                     dem_1000 = getGraph(landscape_1000,neighb,method="old",weightFunction=gradePenaltyFn),
#                     gdistance_1000 = getGraph(landscape_1000,neighb,method="gdistance"),
#                     old_2000 = getGraph(landscape_2000,neighb,method="old"),
#                     dem_2000 = getGraph(landscape_2000,neighb,method="old",weightFunction=gradePenaltyFn),
#                     gdistance_2000 = getGraph(landscape_2000,neighb,method="gdistance")
#   )
# 
#   bm
# 
#   # this one is supposed to tell us about max RAM used over the whole calculation
#   ram_use <- peakRAM::peakRAM(old_100 = getGraph(landscape_100,neighb,method="old"),
#                               gdistance_100 = getGraph(landscape_100,neighb,method="gdistance"),
#                               old_500 = getGraph(landscape_500,neighb,method="old"),
#                               gdistance_500 = getGraph(landscape_500,neighb,method="gdistance"),
#                               old_1000 = getGraph(landscape_1000,neighb,method="old"),
#                               gdistance_1000 = getGraph(landscape_1000,neighb,method="gdistance"),
#                               old_2000 = getGraph(landscape_2000,neighb,method="old"),
#                               gdistance_2000 = getGraph(landscape_2000,neighb,method="gdistance"))
# 
#   ram_use %>%
#     ggplot(aes(Function_Call, Peak_RAM_Used_MiB))+
#     geom_col()+
#     scale_x_discrete(limits =
#                        unique(ram_use$Function_Call,
#                               invert = TRUE, value = TRUE),
#                      labels = paste0(stringr::str_extract(ram_use$Function_Call,
#                                                           "(method=\")(.*)\"", group = 2),
#                                      "_",
#                                      stringr::str_extract(ram_use$Function_Call, "\\d\\d*")))+
#     coord_flip()
# 
#   plot(bm,type="boxplot")+
#     scale_x_discrete(limits =
#                        unique(names(bm$expression),
#                               invert = TRUE, value = TRUE))+
#     xlab("method & landscape width")+ylab("processing time")
# 
#   bm %>%
#     mutate(x = names(expression), mem = mem_alloc) %>%
#     ggplot(aes(x, mem_alloc))+
#     geom_col()+
#     #bench::scale_x_bench_expr()+
#     bench::scale_y_bench_bytes(base=NULL)+
#     coord_flip()+
#     scale_x_discrete(limits =
#                        unique(names(bm$expression),
#                               invert = TRUE, value = TRUE))+
#     labs(x = "method & landscape width",y="memory allocation")
# 
# }
# 

