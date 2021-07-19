#' Faster rasterize for lines
#'
#' Rasterize a line using stars because fasterize doesn't work on lines and
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
#' roadsLine <- sf::st_sfc(geometry = sf::st_linestring(
#' matrix(c(0.5, 4.5, 4.5, 4.51),
#'        ncol = 2, byrow = T) 
#' )) %>%
#'   sf::st_as_sf()
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


