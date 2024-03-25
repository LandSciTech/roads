# Copyright © Her Majesty the Queen in Right of Canada as represented by the
# Minister of the Environment 2021/© Sa Majesté la Reine du chef du Canada
# représentée par le ministre de l'Environnement 2021.
# 
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
# 
#       http://www.apache.org/licenses/LICENSE-2.0
# 
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.

#' Plot projected roads
#' 
#' Plot the results of `projectRoads`
#'
#' @param sim sim list result from `projectRoads`
#' @param mainTitle A title for the plot
#' @param subTitle A sub title for the plot, by default the `roadMethod` is used
#' @param ... Other arguments passed to raster plot call for the `costSurface`
#' 
#' @return Creates a plot using base graphics
#' 
#' @examples 
#' CLUSexample <- prepExData(CLUSexample)
#' prRes <- projectRoads(CLUSexample$landings, CLUSexample$cost, CLUSexample$roads)
#' if(interactive()){
#'   plotRoads(prRes, "Title")
#' }
#' 
#' 
#' @export
#'
plotRoads <- function(sim, mainTitle, 
                      subTitle = paste0("Method: ", sim$roadMethod), 
                      ...){
  terra::plot(sim$costSurface, main = mainTitle, 
               sub = subTitle, ...)
  if(is(sim$roads, "SpatRaster")){
    terra::plot(terra::subst(sim$roads, from = c(0,1), to = c(NA, 1)), 
         add = TRUE, col = "grey50", legend = FALSE)
  } else {
    
    plot(sf::st_geometry(sim$roads), add = TRUE)
  }
  plot(sf::st_geometry(sim$landings), add = TRUE)
  if(is(sim$landings, "SpatialPolygons")){
    plot(sim$landings, add = TRUE)
  } else if(is(sim$landings, "sf") &&
            sf::st_geometry_type(sim$landings, by_geometry = FALSE) %in%
            c("POLYGON", "MULTIPOLYGON")){
    plot(sf::st_geometry(sim$landings), add = TRUE)
  }
}
