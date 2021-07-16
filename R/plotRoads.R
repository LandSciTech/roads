#' Plot projected roads
#' 
#' Plot the results of \code{\link{projectRoads}}
#'
#' @param sim sim list result from \code{projectRoads}
#' @param mainTitle A title for the plot
#' @param subTitle A sub title for the plot, by default the roadMethod is used
#' @param ... Other arguments passed to raster plot call for the costSurface
#' 
#' @return Creates a plot using base graphics
#' 
#' @export
#'
plotRoads <- function(sim, mainTitle, 
                      subTitle = paste0("Method: ", sim$roadMethod), 
                      ...){
  raster::plot(sim$costSurface, main = mainTitle, 
               sub = subTitle, ...)
  if(is(sim$roads, "Raster")){
    plot(raster::subs(sim$roads, data.frame(cur = c(0,1), 
                                            new = c(NA, 1))), 
         add = TRUE, col = "grey50", legend = FALSE)
  } else {
    
    plot(sf::st_geometry(sim$roads), add = TRUE)
  }
  plot(sf::st_geometry(sim$landings), add = TRUE)
  if(is(sim$landings, "SpatialPolygons")){
    sp::plot(sim$landings, add = TRUE)
  } else if(is(sim$landings, "sf") &&
            sf::st_geometry_type(sim$landings, by_geometry = FALSE) %in%
            c("POLYGON", "MULTIPOLYGON")){
    plot(sf::st_geometry(sim$landings), add = TRUE)
  }
}
