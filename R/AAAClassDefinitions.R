# Currently commmented out because not in use but we may decide we want a class
# simlist might be a bad name though because I think it is used in spaDES
# SimList <- setClass(
#   "SimList",
#   slots = c(roads = "sf",
#             costSurface = "RasterLayer",
#             roadMethod = "character",
#             landings = "sf",
#             g = "igraph"),
#   prototype = list(
#     roads = sf::st_sf(col1 = NA,
#                       geometry = sf::st_sfc(sf::st_point(x = c(1,1)))),
#     costSurface  = raster::raster(matrix(NA)),
#     roadMethod  = NA_character_,
#     landings  = sf::st_sf(col1 = NA,
#                           geometry = sf::st_sfc(sf::st_point(x = c(1,1)))),
#     g = igraph::graph(c(1, 1))
#   ))
# 
# 
# #' @name SimList
# #' @rdname SimList-class
# setMethod(f = "initialize", signature = "SimList",
#           definition = function(.Object, sim){
# 
#             .Object@roads <- sim$roads
#             .Object@costSurface <- sim$costSurface
#             .Object@roadMethod <- sim$roadMethod
#             .Object@landings <- sim$landings
#             .Object@g <- sim$g
# 
#             return(.Object)
#           })