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


#' Get landing target points within harvest blocks
#'
#' Generate landing points inside polygons representing harvested area. There
#' are three different sampling types available: `"centroid"` (default) returns
#' the centroid or a point inside the polygon if the
#' centroid is not (see [sf::st_point_on_surface()]); `"random"` returns a
#' random sample given `landingDens` see
#' ([sf::st_sample()]); `"regular"` returns points on a regular grid with cell size `sqrt(1/landingDens)`
#' that intersect the polygon, or centroid if no grid points fall within the polygon.
#'
#' Note that the `landingDens` is points per unit area where the unit of
#' area is determined by the CRS. For projected CRS this should likely be a very
#' small number i.e. < 0.001.
#'
#' @param harvest `sf`, `SpatialPolygons`, `SpatRaster` or `RasterLayer` object with harvested
#'   areas. If it is a raster with values outside 0,1, values are assumed
#'   to be harvest block IDs. If raster values are in 0,1 they are assumed to be
#'   a binary raster and [terra::patches] is used to identify harvest
#'   blocks.
#' @param landingDens number of landings per unit area. This should be in the
#'   same units as the CRS of the harvest. Note that 0.001 points per m2 is > 1000
#'   points per km2 so this number is usually very small for projected CRS.
#' @param sampleType character. `"centroid"` (default), `"regular"` or `"random"`.
#'   `"centroid"` returns one landing per harvest block, which is guaranteed to be
#'   in the harvest block for sf objects but not for rasters. `"regular"` returns
#'   points from a grid with density `landingDens` that overlap the
#'   harvested areas. `"random"` returns a random set of points from each polygon
#'   determined by the area of the polygon and
#'   `landingDens`. If `harvest` is a raster set of landings always includes the centroid
#'   to ensure at least one landing for each harvest block.
#'
#' @return an sf simple feature collection with an `ID` column and `POINT` geometry
#'
#' @examples
#' doPlots <- interactive()
#' demoScen <- prepExData(demoScen)
#'
#' polys <- demoScen[[1]]$landings.poly[1:2,]
#'
#' # Get centroid
#' outCent <- getLandingsFromTarget(polys)
#'
#' if(doPlots){
#'   plot(sf::st_geometry(polys))
#'   plot(outCent, col = "red", add = TRUE)
#' }
#'
#' # Get random sample with density 0.1 points per unit area
#' outRand <- getLandingsFromTarget(polys, 0.1, sampleType = "random")
#'
#' if(doPlots){
#'   plot(sf::st_geometry(polys))
#'   plot(outRand, col = "red", add = TRUE)
#' }
#'
#' # Get regular sample with density 0.1 points per unit area
#' outReg <- getLandingsFromTarget(polys, 0.1, sampleType = "regular")
#'
#' if(doPlots){
#'   plot(sf::st_geometry(polys))
#'   plot(outReg, col = "red", add = TRUE)
#' }
#'
#' @export
getLandingsFromTarget <- function(harvest,
                                  landingDens = NULL,
                                  sampleType = "centroid"){
  if(!sampleType %in% c("regular", "centroid", "random")){
    stop("sampleType must be one of ", c("regular", "centroid", "random"))
  }

  if(sampleType != "centroid" && is.null(landingDens)){
    stop("landingDens must be supplied unless sampleType = 'centroid'")
  }
  if(sampleType == "centroid" && !is.null(landingDens)){
    warning("sampleType is 'centroid' so landingDens will be ignored")
  }

  if(!is.numeric(landingDens) && !is.null(landingDens)){
    stop("landingDens must be numeric not", class(landingDens))
  }
  if(!is.na(sf::st_crs(harvest))){
    if(!sf::st_is_longlat(harvest) &&
       !is.null(landingDens) &&
       landingDens > 0.001){
      message("you have asked for > 0.001 pts per m2",
              " which is > 1000 pts per km2 and may take a long time")
    }
  }

  if(!(is(harvest, "sf") || is(harvest, "sfc"))){
    if(is(harvest, "Spatial")){

      harvest <- sf::st_as_sf(harvest) %>% sf::st_set_agr("constant")

    } else if(is(harvest, "Raster") || is(harvest, "SpatRaster")){
      if(is(harvest, "Raster")){
        harvest <- terra::rast(harvest)
      }

      if(terra::nlyr(harvest) > 1){
        stop("landings should be a single layer SpatRaster")
      }

      #if harvest rast is binary use clumping else assume values are cutblock ids
      lnds_mnmx <- terra::minmax(harvest)[,1]

      if(all(lnds_mnmx %in% c(0,1))){
        message("harvest raster values are all in 0,1. Using patches as landing areas")
        # check if harvest are clumps of cells (ie polygons) or single cells
        # (ie points) and if clumps take centroid
        harvest <- terra::patches(harvest, allowGaps = TRUE, zeroAsNA = TRUE)

      } else {
        message("harvest raster values not in 0,1. Using values as landing ids")

      }

      clumps <- harvest %>% terra::freq()

      clumps <- clumps[,3] %>% max() > 1

      if(clumps){
        # zeros should be treated as NA
        harvest <- terra::subst(harvest, from = 0, to = NA)
        harvest <- sf::st_as_sf(terra::as.polygons(harvest,
                                                   aggregate = TRUE)) %>%
          sf::st_set_agr("constant")
      } else {
        landings <- terra::subst(harvest, from = 0, to = NA)%>%
          terra::as.points() %>%
          sf::st_as_sf() %>%
          sf::st_set_agr("constant")

        if(sampleType != "centroid"){
          warning("raster has only single cell havest blocks so",
                  " landingDens is ignored and cells > 0 converted to points",
                  call. = FALSE)
        }
        return(landings)
      }

    }
  }
  if(all(sf::st_geometry_type(harvest, by_geometry = TRUE) %in%
     c("POLYGON", "MULTIPOLYGON"))){
    if(sampleType == "centroid"){
      # Use point on surface not centroid to ensure point is inside irregular
      # polygons
      landings <- sf::st_point_on_surface(harvest)
    } else if (sampleType == "regular"){
      harvest <- mutate(harvest, ID = 1:n()) %>% sf::st_set_agr("constant")

      grd <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(harvest)),
                              cellsize = sqrt(1/landingDens), what = "corners")

      inter <- sf::st_intersection(harvest, grd)

      notInter <- anti_join(harvest, sf::st_drop_geometry(inter), by = "ID") %>%
        sf::st_set_agr("constant") %>%
        sf::st_point_on_surface()

      landings <- rbind(inter, notInter)

    } else if (sampleType == "random"){

      landings <- harvest %>% mutate(id = 1:n()) %>%
        mutate(size = round(landingDens *
                              sf::st_area(.data[[attr(harvest, "sf_column")]])) %>%
                 units::set_units(NULL)) %>%
        mutate(size = ifelse(.data$size == 0, 1, .data$size))

      landings1 <- filter(landings, .data$size == 1) %>%
        mutate(lands = sf::st_point_on_surface(.data[[attr(harvest, "sf_column")]]))

      if(nrow(landings1) < nrow(landings)){
        landings2Plus <- filter(landings, .data$size > 1)
        landings2Plus <- sf::st_sample(landings2Plus[[attr(landings2Plus, "sf_column")]], type = "random",
                                       size = landings2Plus$size)

        landings <- c(landings1$lands, landings2Plus)
      } else {

        landings <- landings1$lands
      }
      return(sf::st_sf(landings))
    }

  } else {
    stop("harvest contains geometries other than POLYGON and MULTIPOLYGON.",
         "\nPlease supply only polygons")
  }
}


#' Select random landing locations within patches.
#'
#' @param inputPatches A SpatRaster. Harvested patches should have values
#'   greater than 0
#' @param landingDens number of landings per unit area. This should be in the
#'   same units as the CRS of the harvest. Note that 0.001 points per m2 is > 1000
#'   points per km2 so this number is usually very small for projected CRS.
#' @param sampleType character. "centroid" (default), "regular" or "random".
#'   Centroid returns one landing per harvest block, which is not guaranteed to
#'   be in the harvest block. Regular returns points from a grid with density
#'   `landingDens` that overlap the harvested areas. Random returns a
#'   random set of points from each polygon where the number is determined by
#'   the area of the polygons and `landingDens`. The centroid is always
#'   returned as one of the landings to ensure all harvest areas get at least
#'   one landing.
#' @param omitCentroidsOutOfPolygons Logical. Default is FALSE in which case
#'   some points may be outside the borders of the harvested patch
#'
#' @noRd
getLandingsFromTargetRast<-function(inputPatches,
                                    landingDens,
                                    sampleType = "regular",
                                    omitCentroidsOutOfPolygons = F){
  # Function to select a specific number of landings withing patches. Landing set
  # will include centroids, and additional randomly selected sample points if
  # landingDens>numCentroids.

  # inputPatches=newBlocks[[cm]]

  if(length(landingDens) > 1){
    stop("landingDens should have length 1 not ", length(landingDens))
  }
  if(!sampleType %in% c("regular", "centroid", "random")){
    stop("sampleType must be one of ", c("regular", "centroid", "random"))
  }

  inputPatches[inputPatches == 0] <- NA

  clumpVals <- terra::unique(inputPatches, na.rm = TRUE)[[1]]
  landPts <- sf::st_sf(integer(),
                       geometry = sf::st_sfc(),
                       crs = sf::st_crs(inputPatches))

  # landPts = matrix(0,0,3)
  # colnames(landPts)=c("x","y","ID")
  # landPts <- sf::st_as_sf(as.data.frame(landPts), coords = c("x", "y"),
  #                         crs = sf::st_crs(inputPatches))

  for(i in seq_along(clumpVals)){

    nl <- ifelse(is.null(landingDens), 0, landingDens)

    ip <- inputPatches == clumpVals[i]

    ip[ip == 0] <- NA

    landC = getCentroids(ip, withIDs = T) #note centroids are not always in polygons
    if (omitCentroidsOutOfPolygons) {
      landC[is.na(ip)] <- NA
    }
    remL <- ip
    remL[landC > 0] <- NA

    landC <- terra::subst(landC, from = 0, to = NA)%>%
      terra::as.points() %>%
      sf::st_as_sf() %>%
      sf::st_set_agr("constant")

    if(sampleType == "centroid"){
      landPts <- rbind(landPts, landC)
      next
    }

    if(sampleType == "random"){
      #select additional points so total number is equal to small alternative
      #numSamples = nl - raster::cellStats(landC > 0, "sum")
      remL_sum <- terra::global(remL, "sum", na.rm = TRUE)[1,1]

      numSamples <- round(remL_sum *
                            landingDens *
                            prod(terra::res(remL)))
      if(is.na(numSamples) || numSamples < 1){
        # use just the centroids
        landPts <- rbind(landPts, landC)
        next
      }

      if(numSamples >= remL_sum){

        # If more samples than cells return 1 pt per cell and ignore centroid
        landingPts <- terra::subst(ip, from = 0, to = NA)%>%
          terra::as.points() %>%
          sf::st_as_sf() %>%
          sf::st_set_agr("constant")
        landPts <- rbind(landPts, landingPts)

      } else {
        remL <- terra::trim(remL, padding = 1)

        landingPts <- terra::spatSample(remL, size = numSamples,
                                        method = "random", na.rm = TRUE,
                                        as.points = TRUE, warn = FALSE)%>%
          sf::st_as_sf()
      }
    }
    if(sampleType == "regular"){
      extArea <- prod(terra::res(remL))*terra::ncell(remL)
      nPts <- landingDens * extArea

      landingPts <- terra::spatSample(remL, size = nPts, method = "regular",
                                       as.points = TRUE, warn = FALSE) %>%
        sf::st_as_sf()
      landingPts <- landingPts[!is.na(landingPts[[1]]), ]
    }

    #add centroids to ensure all patches are included
    landPts = rbind(landPts, landC, landingPts)
  }
  # make sf object
  landPts <- landPts %>%
    sf::st_set_crs(sf::st_crs(inputPatches))

  return(landPts)
}

#' Get centroids from raster landings
#'
#' @param newLandings raster landings
#'
#' @param withIDs logical
#' @noRd
getCentroids<-function(newLandings, withIDs = TRUE){
  cRes = terra::res(newLandings)

  p = terra::as.data.frame(terra::patches(newLandings, zeroAsNA = TRUE,
                                          allowGaps = FALSE), xy = TRUE)
  p = p[!is.na(p$patches), ]

  pointLocs = p %>% dplyr::group_by(.data$patches) %>%
    dplyr::summarize(x = mean(.data$x), y = mean(.data$y))

  pointLocs = as.data.frame(subset(pointLocs, select = c('x', 'y', 'patches')))

  newLandingCentroids = newLandings
  newLandingCentroids[!is.na(newLandingCentroids)] = NA

  cells = terra::cellFromXY(newLandingCentroids, pointLocs[, 1:2])

  if (withIDs) {
    newLandingCentroids[cells] = pointLocs$patches
  } else{
    newLandingCentroids[cells] = 1
  }
  return(newLandingCentroids)
}
