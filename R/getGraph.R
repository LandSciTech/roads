# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#NOTICE: This function has been modified from https://github.com/bcgov/clus/blob/master/R/SpaDES-modules/roadCLUS/roadCLUS.R


#' Set the graph which determines least cost paths
#'
#' Creates a graph in initiation phase which can be updated and solved for
#' paths
#'
#' @param sim a sim list
#' @param neighbourhood neighbourhood type
#' @noRd

getGraph<- function(sim, neighbourhood,method="old",weightFunction =  simpleCostFn,... ){
  #sim = list(weightRaster=weightRaster);neighbourhood="octagon"
  #gdistance method takes more time and less memory. See testAltGraphFns in RoadPaper repo for details.  resolution=res(sim$weightRaster)[1]

  inargs = list(...)
  if(!is.element("resolution",names(inargs))){
    if(terra::is.lonlat(sim$weightRaster)){
      sze <- terra::cellSize(sim$weightRaster)
      resolution <- terra::global(sze, fun = "mean")^0.5
      resolution <- resolution[1,1]
    } else {
      resolution = terra::res(sim$weightRaster)[1]
    }
  }else{
    resolution = inargs$resolution
  }

  if(!is.element(neighbourhood, c("rook", "octagon","queen"))) {
    stop("neighbourhood type not recognized")
  }

  if(method=="gdistance"){
    if(!requireNamespace("gdistance", quietly = TRUE)){
      stop("The gdistance package is required to use method gdistance")
    }

    dirs = switch(neighbourhood,
                  rook=4,
                  octagon=8,
                  queen=8)
    x = gdistance::transition(as(sim$weightRaster, "Raster"), transitionFunction=function(x) 1/weightFunction(x[1],x[2],hdistance=resolution,...), directions=dirs)

    if(neighbourhood=="octagon"){
      #correct for diagonal distances and other aspects of geographic distance
      x = gdistance::geoCorrection(x,type="c",scl=T)
    }
    y = gdistance::transitionMatrix(x)
    g <- igraph::graph_from_adjacency_matrix(y, mode="undirected", weighted=TRUE)

    igraph::E(g)$weight <- 1/igraph::E(g)$weight
    return(invisible(g))
  }else{
    # define global varaiables to avoid NOTEs on R CMD check
    weight <- weightV <- from <- to <- w1 <- w2 <- NULL

    # prepare the weightRaster raster #===========
    # get weight as data.table from raster
    weightV <- data.table(weight = terra::values(sim$weightRaster, mat = FALSE))

    # get the id for ther verticies which is used to merge with the edge list from
    # adj
    weightV[, id := seq_len(.N)]

    # rooks case
    if(!is.element(neighbourhood, c("rook", "octagon", "queen"))) {
      stop("neighbourhood type not recognized")
    }

    nc <- terra::ncol(sim$weightRaster)
    ncel <- terra::ncell(sim$weightRaster) %>% as.integer()

    edges <- terra::adjacent(sim$weightRaster, cells = 1:as.integer(ncel),
                             directions = "rook", pairs = TRUE) %>%
      data.table::as.data.table()

    # switch to and from where to > from so can remove duplicates
    edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]

    edges <- unique(edges)

    # merge in the weights from a weightRaster
    edges.weight <- merge(x = edges, y = weightV, by.x = "from", by.y = "id")

    # reformat
    data.table::setnames(edges.weight, c("from", "to", "w1"))

    # merge in the weights to a weightRaster
    edges.weight <- data.table::setDT(merge(x = edges.weight, y = weightV,
                                          by.x = "to", by.y = "id"))

    # reformat
    data.table::setnames(edges.weight, c("from", "to", "w1", "w2"))

    # apply the weightFunction across the two pixels and remove w1 w2
    edges.weight[,`:=`(weight = weightFunction(w1,w2,hdistance=resolution,...),
                     w1 = NULL,
                     w2 = NULL)]

    if(neighbourhood != "rook"){

      # bishop's case - multiply horizontal distance by 2^0.5
      if(neighbourhood == "octagon"){
        mW = 2^0.5
      } else {mW = 1}
      #weightV[, weight := weight*mW]

      edges <- terra::adjacent(sim$weightRaster, cells = 1:as.integer(ncel),
                               directions = "bishop", pairs = TRUE) %>%
        data.table::as.data.table()

      # edges[from < to, c("from", "to") := .(to, from)]
      edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]

      edges <- unique(edges)

      # merge in the weights from a weightRaster
      edges_bishop <- merge(x = edges, y = weightV, by.x = "from", by.y = "id")

      # reformat
      data.table::setnames(edges_bishop, c("from", "to", "w1"))

      # merge in the weights to a weightRaster
      edges_bishop <- data.table::setDT(merge(x = edges_bishop, y = weightV,
                                              by.x = "to", by.y = "id"))

      # reformat
      data.table::setnames(edges_bishop, c("from", "to", "w1", "w2"))

      # apply weightFunction across the two pixels and remove w1 w2
      edges_bishop[,`:=`(weight = weightFunction(w1,w2,hdistance=resolution*mW,...),
                         w1 = NULL,
                         w2 = NULL)]

      # get the edges list #==================
      edges.weight = rbind(edges.weight, edges_bishop)
      edges.weight = edges.weight[order(from,to),]

      rm(edges_bishop, mW)

    }

    # get rid of NAs caused by barriers. Drop the w1 and w2 columns.
    edges.weight <- na.omit(edges.weight)

    # set the ids of the edge list. Faster than using as.integer(row.names())
    edges.weight[, id := seq_len(.N)]

    # clean-up
    rm(edges, weightV, nc, ncel)

    # make the graph #================
    edge_mat <- as.matrix(edges.weight)
    # browser()
    # create the graph using to and from columns. Requires a matrix input
    g <- igraph::graph_from_edgelist(edge_mat[,1:2], dir = FALSE)

    # assign weights to the graph. Requires a matrix input
    igraph::E(g)$weight <- edge_mat[,3]

    #------clean up
    rm(edges.weight)
    return(invisible(g))
  }
}

