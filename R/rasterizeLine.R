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

#' Faster rasterize for lines
#'
#' Rasterize a line using \code{stars} because \code{fasterize} doesn't work on lines and
#' rasterize is slow
#'
#' @param sfLine an sf object to be rasterized
#' @param rast a raster to use as template for the output raster
#' @param value a number value to give the background ie 0 or NA
#'
#' @return a RasterLayer where the value of cells that touch the line will be
#'   the row index of the line in the sf
#' @export
#'
#' @examples
#' 
#' roadsLine <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(
#' matrix(c(0.5, 4.5, 4.5, 4.51),
#'        ncol = 2, byrow = TRUE) 
#' )))
#'
#' rasterizeLine(roadsLine, CLUSexample$cost, 0)   
#'
#'
rasterizeLine <- function(sfLine, rast, value){
  # rasterize roads to template
  tmplt <- stars::st_as_stars(sf::st_bbox(rast), nx = raster::ncol(rast),
                              ny = raster::nrow(rast), values = value)
  
  rastLine <- stars::st_rasterize(sfLine[attr(sfLine, "sf_geometry")],
                                   template = tmplt,
                                   options = "ALL_TOUCHED=TRUE") %>%
    as("Raster")
  
  return(rastLine)
}


