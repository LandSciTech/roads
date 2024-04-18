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

#' Convert raster cell numbers in paths to lines
#'
#' copied from newRoadsToLines and made work with sf
#'
#' @param sim sim list
#' @noRd

pathsToLines <- function(sim){
  ## existing roads
  er <- sim$weightRaster == 0

  linelist <- lapply(1:length(sim$paths.list), function(i){

    # finds first match for start and end cells in paths
    inds <- match(sim$paths.list[[i]], sim$paths.v[[1]])

    if(any(is.na(inds))){
      stop("NA values in weightRaster along paths. Check for disconnected clumps in weightRaster. If weightFunction=slopePenaltyFn, try adjusting limit or setting limitWeight to high value that is not NA.", call. = FALSE)
    }

    if(inds[1] == inds[2]){
      return(NULL)
    }
    # cell indicies for vertices on line
    v <- sim$paths.v[[1]][inds[1]:inds[2]]

    # remove vertices in this line from sim object in parent environment
    sim$paths.v <<- sim$paths.v[-(inds[1]:inds[2]), ]

    # raster values at v
    er_v <- terra::extract(er, v)[[1]]
    
    # set er to 1 for these cells since they are now existing roads except last
    # cell because it may be needed for connecting to next road
    terra::set.values(er, v[-length(v)], 1)

    ## index of where new road connects to existing road
    conn <- which(er_v == 1)

    # outLine1 <- sf::st_linestring(raster::xyFromCell(sim$weightRaster, v))

    ## remove portions that run along existing road, if applicable. If there are
    ## multiple sections of new road with sections of old road in between we
    ## need to split it into multiple lines so it doesn't jump. Split at more
    ## than 1 true in a row followed by a false?
    ## something with cumsum(rle(er[v]==1)$lengths)?
    if(length(conn) > 0){

      if(length(which(er_v == 0)) == 0){
        # the whole path is on existing road
        return(NULL)
      }

      run_lengths <- rle(er_v == 0)$length
      if(length(run_lengths) > 1){
        if(er_v[1] == 0){
          if(er_v[length(v)] == 1){
            # first cell is not existing road and last cell is existing road
            run_lengths_mat <- run_lengths %>% cumsum()
            run_lengths_mat <- c(1, run_lengths_mat)
            run_lengths_mat <- run_lengths_mat[-length(run_lengths_mat)] %>%
              matrix(ncol = 2, byrow = T) %>%
              as.data.frame() %>%
              `names<-`(c("start", "end")) %>%
              mutate(end = .data$end + 1)
          } else {
            # first cell is not existing road and last cell is not existing road
            run_lengths_mat <- run_lengths %>% cumsum()
            run_lengths_mat <- c(1, run_lengths_mat) %>%
              matrix(ncol = 2, byrow = T) %>%
              as.data.frame() %>%
              `names<-`(c("start", "end")) %>%
              mutate(end = .data$end + 1)
          }
        } else {
          if(er_v[length(v)] == 1) {
            # first cell is existing road and last cell is existing road
            run_lengths_mat <- run_lengths %>% cumsum()
            run_lengths_mat <- run_lengths_mat[-length(run_lengths_mat)] %>%
              matrix(ncol = 2, byrow = T) %>%
              as.data.frame() %>%
              `names<-`(c("start", "end")) %>%
              mutate(end = .data$end + 1)
          } else {
            # first cell is existing road and last cell is not existing road
            run_lengths_mat <- run_lengths %>% cumsum() %>%
              matrix(ncol = 2, byrow = T) %>%
              as.data.frame() %>%
              `names<-`(c("start", "end")) %>%
              mutate(end = .data$end + 1)
          }
        }
        if(nrow(run_lengths_mat) == 1){
          keep <- list(seq(from = run_lengths_mat$start,
                       to = run_lengths_mat$end))
        } else {
          keep <- apply(run_lengths_mat, 1, function(x) seq(from = x[1], to = x[2]),
                        simplify = FALSE)
        }
      }

    } else {
      keep <- list(1:length(v))
    }
    if(length(keep) == 1){
      cellsToKeep <- v[keep[[1]]]
      cellsToKeep <- na.omit(cellsToKeep)
      outLine <- sf::st_linestring(terra::xyFromCell(sim$weightRaster,
                                                      cellsToKeep)) 
    } else {
      outLine <- lapply(keep, function(x){
        cellsToKeep <- v[x]
        cellsToKeep <- na.omit(cellsToKeep)
        sf::st_linestring(terra::xyFromCell(sim$weightRaster, cellsToKeep))
      })
      outLine <- do.call(c, outLine)
    }


    # strt_end <-raster::xyFromCell(sim$weightRaster, sim$paths.list[[i]]) %>%
    #   as.data.frame %>%
    #   st_as_sf(coords = c("x", "y"))
    #
    # plot(outLine1, col = "blue")
    # #raster::plot(sim$weightRaster, add = TRUE)
    # plot(sim$landings %>% st_geometry(), add = T)
    # plot(sim$roads %>% st_geometry(), add = T)
    # plot(outLine1, col = "blue", add = T)
    # plot(outLine, col = "red", add = T)
    # plot(strt_end, col = "red", add = T, pch = 4)
    # title(main = i)

    return(outLine)
  })

  # remove NULLs
  linelist <- linelist[vapply(linelist,function(x) !is.null(x), c(TRUE))]

  if(length(linelist) == 0){
    return(slice(sim$roads, 0))
  }

  outLines <- sf::st_sf(geometry = linelist %>% sf::st_as_sfc())
  return(sf::st_set_crs(outLines, sf::st_crs(sim$roads)))
}
