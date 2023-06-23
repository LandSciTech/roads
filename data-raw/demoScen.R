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

############################################################################
# function for generating input scenarios for the roads::projectRoads method
# changed to use terra spatRasters and sf lines
#
# returns a list of sub-lists, with each sub-list representing an input scenario. The scenarios (sub-lists) contain the following named elements:
#       - scen.number: an integer value representing the scenario number (generated scenarios are numbered incrementally from 1 to n.scenarios)
#       - road.rast:   a logical RasterLayer representing existing roads.  TRUE is existing road. FALSE is not existing road.
#       - road.line:   sf object representing the existing roads
#       - cost.rast:   RasterLayer representing the cost of developing new roads on a given cell
#       - landings.points:  a SpatialPointsDataFrame representing landings sets and points
#                           - data frame includes a field named "set" containing integer values representing the landings set that each point belongs to
#       - landings.stack:  a SpatRaster representing the landings and landings sets
#                          - each layer in the stack represents an individual landings set as a logical layers where TRUE is a landing in that set and FALSE is not
#                            a landing in the set
#       - landings.poly: a single set of polygonal landings
#                         - will be NA if user set n.poly.landings argument to NA
#
# n.scenarios: integer representing the number of scenarios that are to be generated
# xy.size:     vector (length=2) of integers representing the number of columns and number of rows, respectively, for the output cost raster
# spat.corr:   logical representing whether or not generated cost raster should be spatially correlated (gaussian random field) or not. TRUE for spatially correlated.
# cost.lim:    numeric value (must be > 1) representing either: the upper limit of the cost values (only if spat.corr is FALSE)
#                                                       or:     the mean value for the gaussian random field (only if spat.corr is TRUE)
#              - if spat.corr is TRUE, cost values will represent a gaussian random field with mean of cost.lim
#              - if spat.corr is TRUE, generated cost values that are less than 1 will be set equal to 1
#              - if spat.corr is FALSE, cost values will be a uniform random sample between 1 and cost.lim
# std.dev: (ignored if spat.corr is FALSE) numeric value representing standard deviation for the gaussian random field
#          - see cost.lim, above, for setting the mean of the gaussian random field
#          - std.dev setting will be used as the sigma^2 component of the 'cov.pars' parameter of the geoR::grf function
#          - generated cost values that are less than 1 will be set equal to 1
# range:   (ignored if spat.corr is FALSE) either a numeric value representing the range or a vector (length=2) of numeric values representing a upper and
#           lower limits for the range value
#          - range should only include values > 0.  If range is a vector, then range[1] should not equal range[2]
#          - the range value will be used to determine the phi component of the 'cov.pars' parameter of the geoR::grf function
#          - if range is a single numeric value, then that value will be used when generating each input scenario
#          - if range is a vector (length=2) of numeric values, then the range value used when generating each scenario will be uniformly randomly sampled
#            from between range[1] and range[2]
# n.roads: either an integer representing the number of initial roads in the landscape or a vector (length=2) of integers representing the upper and
#          lower limits for the number of initial roads in each landscape
#          - n.roads should only inlcude integers > 0. If n.roads is a vector, then n.roads[1] should not equal n.roads[2]
#          - if n.roads is a single integer value, then that value will be used when generating each input scenario (number of roads in each landscape will
#            eqaul n.roads)
#          - if n.roads is a vector (length=2) of integer values, then the number of roads in each scenario will be uniformly randomly sampled from between
#            n.roads[1] and n.roads[2]
#          - each road is a rasterized straight line, connecting opposite sides of the landscape (i.e. left-right, or top-bottom)
# n.landing.sets: an integer value (>0) representing the number of landing sets to generate for each scenario
# n.landings:  either an integer representing the number of landings to be included in each landings set or a vector (length=2) of integers representing the
#              upper and lower limits for the number of landings in each landing set
#              - n.landings should only include values > 0. If n.landings is a vector, then n.landings[1] should not equal n.landings[2]
#              - if n.landings is a single numeric value, then that value will be used when generating each landing set in each scenario
#              - if n.landings is a vector (length=2) of integer values, then the number of landings in each landing set of each scenario will be uniformly
#                randomly sampled from between n.landings[1] and n.landings[2]
# n.poly.landings: either NA (in which case polygonal landings will not be generated) or a vector (length=2) of integer values representing the upper and
#                  lower limits for the number of polygonal landings in each scenario
#                  - polygonal landings will not be generated within 2 cells of: landscape edge, existing roads, or other polygonal landings
# poly.landings.xdim: (ignored if n.poly.landings is NA) a vector (length=2) of integer values representing the upper and lower limits of the width of each
#                      polygonal landing, in # of cells
# poly.landings.ydim: (ignored if n.poly.landings is NA) a vector (length=2) of integer values representing the upper and lower limits of the height of each
#                      polygonal landing, in # of cells
# seed.value:  either NA (in which case the random seed will not be set) or a numeric value representing the seed value for generating the random scenarios
#              - if seed.value is a numeric value, the random seed will be modified using the base::set.seed function
devtools::load_all(".")
# borrowed from prioritizr
simulate_data <- function(x, n = 1, scale = 0.5, intensity = 0, sd = 1, transform = identity) {

  # create object for simulation
  obj <- fields::circulantEmbeddingSetup(
    grid = list(
      # changed max to 20 fixed error
      x = seq(0, 20, length.out = terra::nrow(x)),
      y = seq(0, 20, length.out = terra::ncol(x))
    ),
    Covariance = "Exponential",
    aRange = scale
  )

  # generate populate rasters with values
  r <- terra::rast(lapply(seq_len(n), function(i) {
    ## populate with simulated values
    v <- c(t(fields::circulantEmbedding(obj)))
    v <- transform(v + stats::rnorm(length(v), mean = intensity, sd = sd))
    r <- terra::setValues(x[[1]], v[seq_len(terra::ncell(x))])
    ## apply mask for consistency
    r <- terra::mask(r, x[[1]])
    ## return result
    r
  }))

  # return result
  r
}

prepInputScenarios <- function(n.scenarios = 10, xy.size = c(100, 100), spat.corr = T, cost.lim = 10, std.dev = 1, range = c(1, 3),
                               n.roads = c(1, 4), n.landing.sets = 10, n.landings = c(1, 10), n.poly.landings = c(1, 8),
                               poly.landings.xdim = c(3, 20), poly.landings.ydim = c(3, 20), seed.value = NA,
                               use.crs = sf::st_crs("EPSG:5070")) {
  if (!is.na(seed.value)) {
    set.seed(seed.value)
  }
  if (cost.lim <= 1) {
    stop("cost.mean expected to be > 1")
  }
  outlist <- list()
  for (i in 1:n.scenarios) {
    scen <- list(
      scen.number = as.integer(i),
      road.rast = NA,
      road.line = NA,
      cost.rast = NA,
      landings.points = NA,
      landings.stack = NA,
      landings.poly = NA
    )
    r <- terra::rast(
      ncols = xy.size[1], nrows = xy.size[2], vals = 0,
      xmin = 0, ymin = 0, xmax = xy.size[1], ymax = xy.size[2]
    )
    if (spat.corr) {
      if (length(range) == 1) {
        rOut <- simulate_data(r, n = 1, sd = std.dev, intensity = cost.lim, scale = range)
        rOut[rOut < 1] <- 1
      } else {
        rOut <- simulate_data(r,
          n = 1, sd = std.dev, intensity = cost.lim,
          scale = runif(1, range[1], range[2])
        )
        rOut[rOut < 1] <- 1
      }
    } else {
      rOut <- terra::rast(terra::ext(0, xy.size[1], 0, xy.size[2]),
        res = 1,
        vals = runif(as.integer(prod(xy.size)), 1, cost.lim)
      )
    }
    ## prep roads
    if (length(n.roads) > 1) {
      road.num <- sample(n.roads[1]:n.roads[2], 1)
    } else {
      road.num <- n.roads
    }
    lineList <- list()
    for (j in 1:road.num) {
      startside <- sample(1:4, 1)
      if (startside %in% c(1, 2)) {
        endside <- c(1, 2)[c(1, 2) != startside]
      } else if (startside %in% c(3, 4)) {
        endside <- c(3, 4)[c(3, 4) != startside]
      }
      cellsToConnect <- c()
      for (side in c(startside, endside)) {
        if (side == 1) {
          cells <- terra::cellFromRowCol(rOut, row = 1, col = 1:terra::ncol(rOut))
        } else if (side == 2) {
          cells <- terra::cellFromRowCol(rOut, row = terra::nrow(rOut), col = 1:terra::ncol(rOut))
        } else if (side == 3) {
          cells <- terra::cellFromRowCol(rOut, row = 1:terra::nrow(rOut), col = 1)
        } else if (side == 4) {
          cells <- terra::cellFromRowCol(rOut, row = 1:terra::nrow(rOut), col = terra::ncol(rOut))
        }
        cellsToConnect <- c(cellsToConnect, sample(cells[2:(length(cells) - 1)], 1))
      }
      lineList[[length(lineList) + 1]] <- sf::st_sf(
        ID = j,
        geometry = sf::st_as_sfc(list(sf::st_linestring(terra::xyFromCell(rOut, cellsToConnect))))
      )
    }
    roadlines <- dplyr::bind_rows(lineList)
    rast_withroads <- burnRoadsInCost(roadlines, rOut)
    ## landings
    land_stack <- terra::rast(rOut)
    toSample <- rast_withroads
    toSample[toSample == 0] <- NA
    land_rast <- rast_withroads == 0
    for (li in 1:n.landing.sets) {
      land_rast[] <- FALSE
      if (length(n.landings > 1)) {
        n.land.samples <- sample(n.landings[1]:n.landings[2], 1)
      } else {
        n.land.samples <- n.landings
      }
      land <- terra::spatSample(toSample, size = n.land.samples, na.rm = T, as.points = T)
      land$set <- li
      land$ID <- 1:length(land)
      if (li == 1) {
        landings <- land
      } else {
        landings <- rbind(landings, land)
      }
      toSample[terra::cellFromXY(toSample, terra::crds(land))] <- NA
      land_rast[terra::cellFromXY(toSample, terra::crds(land))] <- TRUE
      if (li == 1) {
        land_stack <- land_rast
      } else {
        land_stack <- c(land_stack, land_rast)
      }
    }
    landings <- sf::st_as_sf(landings)
    # polygonal landings
    if (all(!is.na(n.poly.landings))) {
      if (all(n.poly.landings > 0)) {
        rast_valid <- rast_withroads
        continueAdding <- T
        n.try <- 100
        for (li in 1:sample(n.poly.landings[1]:n.poly.landings[2], 1)) {
          for (trynum in 1:n.try) {
            poly.size <- c(
              sample(poly.landings.xdim[1]:poly.landings.xdim[2], 1),
              sample(poly.landings.ydim[1]:poly.landings.ydim[2], 1)
            )
            topleft <- terra::spatSample(
              terra::rast(
                res = c(1, 1), xmin = 2,
                xmax = xy.size[1] - poly.size[1] - 2,
                ymin = poly.size[2] + 2,
                ymax = xy.size[2] - 2, vals = 0
              ),
              size = 1, xy = T
            )
            topleft <- as.matrix(topleft)
            tlbr <- rbind(topleft[, 1:2], topleft[, 1:2] + c(poly.size[1], -1 * poly.size[2])) # top left and bottom right
            tlbr.buff <- rbind(tlbr[1, ] + c(-2, 2), tlbr[2, ] - c(-2, 2))

            poly.buff <- sf::st_polygon(list(rbind(
              tlbr.buff[1, ],
              cbind(tlbr.buff[1, 1], tlbr.buff[2, 2]),
              tlbr.buff[2, ],
              cbind(tlbr.buff[2, 1], tlbr.buff[1, 2]),
              tlbr.buff[1, ]
            )))
            poly.buff <- sf::st_sf(ID = li, geometry = sf::st_as_sfc(list(poly.buff)))
            if (all(terra::extract(rast_valid, poly.buff)[, 1] > 0)) {
              poly <- sf::st_polygon(list(rbind(
                tlbr[1, ],
                cbind(tlbr[1, 1], tlbr[2, 2]),
                tlbr[2, ],
                cbind(tlbr[2, 1], tlbr[1, 2]),
                tlbr[1, ]
              )))
              poly <- sf::st_sf(ID = li, geometry = sf::st_as_sfc(list(poly)))
              rast_valid[terra::extract(rast_valid, poly, cells = T)$cell] <- 0
              break
            }
            if (trynum == n.try) {
              warning("Could not find valid spot for polygon ", li, " in scenario ",
                      i, " with ", n.try, " tries")
              continueAdding <- F
            }
          }
          if (!continueAdding) {
            break
          }
          if (li == 1) {
            polydf <- poly
          } else {
            polydf <- rbind(polydf, poly)
          }
        }
      }
    }
    terra::crs(rast_withroads) <- use.crs$wkt
    terra::crs(land_stack) <- use.crs$wkt
    scen$road.rast <- rast_withroads == 0
    scen$road.line <- roadlines %>% sf::st_set_crs(use.crs)
    scen$cost.rast <- rast_withroads
    scen$landings.points <- landings %>% sf::st_set_crs(use.crs)
    scen$landings.stack <- land_stack
    scen$landings.poly <- polydf %>% sf::st_set_crs(use.crs)
    outlist[[i]] <- scen
  }
  return(outlist)
} # end of prepInputScenarios function
#################################################
# generate 10 roads::projectRoads input scenarios
demoScen <- prepInputScenarios(n.scenarios = 10, seed.value = 1)

# wrap SpatRasters use prepExData to unwrap
demoScen <- rapply(demoScen, f = terra::wrap, classes = "SpatRaster", how = "replace")

# set sf objects to agr = "constant"
demoScen <- rapply(demoScen, f = sf::st_set_agr, classes = "sf", how = "replace", value = "constant")

usethis::use_data(demoScen, overwrite = TRUE)
#################################################
# view all cost rasters
# demoScen %>% purrr::map("cost.rast") %>% terra::rast() %>% terra::plot()


