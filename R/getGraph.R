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

getGraph<- function(sim, neighbourhood,method="old",weightFunction = function(x1,x2) (x1+x2)/2){
  #sim = list(costSurface=costRaster);neighbourhood="octagon"
  #gdistance method takes more time and less memory. See testAltGraphFns in RoadPaper repo for details.
  if(method=="gdistance"){
    if(!is.element(neighbourhood, c("rook", "octagon","queen"))) {
      stop("neighbourhood type not recognized")
    }

    dirs = switch(neighbourhood,
                  rook=4,
                  octagon=8,
                  queen=8)
    x = gdistance::transition(as(sim$costSurface, "Raster"), transitionFunction=function(x) 1/weightFunction(x[1],x[2]), directions=dirs)

    if(neighbourhood=="octagon"){
      #correct for diagonal distances and other aspects of geographic distance
      x = gdistance::geoCorrection(x,type="c",scl=T)
    }
    y = gdistance::transitionMatrix(x)
    sim$g <- igraph::graph_from_adjacency_matrix(y, mode="undirected", weighted=TRUE)
    igraph::E(sim$g)$weight <- 1/igraph::E(sim$g)$weight
    return(invisible(sim))
  }else{
    # define global varaiables to avoid NOTEs on R CMD check
    w1 <- w2 <- NULL

    sim$paths.v <- NULL
    # prepare the cost surface raster #===========
    # get cost as data.table from raster
    weight <- data.table(weight = terra::values(sim$costSurface, mat = FALSE))

    # get the id for ther verticies which is used to merge with the edge list from
    # adj
    weight[, id := seq_len(.N)]

    # get the adjacency using SpaDES function adj #=======
    # rooks case
    if(!is.element(neighbourhood, c("rook", "octagon", "queen"))) {
      stop("neighbourhood type not recognized")
    }

    nc <- terra::ncol(sim$costSurface)
    ncel <- terra::ncell(sim$costSurface) %>% as.integer()

    edges <- terra::adjacent(sim$costSurface, cells = 1:as.integer(ncel),
                             directions = "rook", pairs = TRUE) %>%
      data.table::as.data.table()

    # switch to and from where to > from so can remove duplicates
    edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]

    edges <- unique(edges)

    # merge in the weights from a cost surface
    edges_rook <- merge(x = edges, y = weight, by.x = "from", by.y = "id")

    # reformat
    data.table::setnames(edges_rook, c("from", "to", "w1"))

    # merge in the weights to a cost surface
    edges_rook <- data.table::setDT(merge(x = edges_rook, y = weight,
                                          by.x = "to", by.y = "id"))

    # reformat
    data.table::setnames(edges_rook, c("from", "to", "w1", "w2"))

    # take the average cost between the two pixels and remove w1 w2
    edges_rook[,`:=`(weight = weightFunction(w1,w2),
                     w1 = NULL,
                     w2 = NULL)]

    if(neighbourhood == "rook"){
      edges.weight = edges_rook
    } else {

      # bishop's case - multiply weights by 2^0.5
      if(neighbourhood == "octagon"){
        mW = 2^0.5
      } else {mW = 1}
      weight[, weight := weight*mW]

      edges <- terra::adjacent(sim$costSurface, cells = 1:as.integer(ncel),
                               directions = "bishop", pairs = TRUE) %>%
        data.table::as.data.table()

      # edges[from < to, c("from", "to") := .(to, from)]
      edges[edges$from < edges$to, ] <- edges[edges$from < edges$to, c('to','from')]

      edges <- unique(edges)

      # merge in the weights from a cost surface
      edges_bishop <- merge(x = edges, y = weight, by.x = "from", by.y = "id")

      # reformat
      data.table::setnames(edges_bishop, c("from", "to", "w1"))

      # merge in the weights to a cost surface
      edges_bishop <- data.table::setDT(merge(x = edges_bishop, y = weight,
                                              by.x = "to", by.y = "id"))

      # reformat
      data.table::setnames(edges_bishop, c("from", "to", "w1", "w2"))

      # take the average cost between the two pixels and remove w1 w2
      edges_bishop[,`:=`(weight = weightFunction(w1,w2),
                         w1 = NULL,
                         w2 = NULL)]

      # get the edges list #==================
      edges.weight = rbind(edges_rook, edges_bishop)
      edges.weight = edges.weight[order(from,to),]

    }

    # get rid of NAs caused by barriers. Drop the w1 and w2 costs.
    edges.weight <- na.omit(edges.weight)

    # set the ids of the edge list. Faster than using as.integer(row.names())
    edges.weight[, id := seq_len(.N)]

    # clean-up
    rm(edges_rook, edges_bishop, edges, weight, mW, nc, ncel)

    # make the graph #================
    edge_mat <- as.matrix(edges.weight)[,1:2]
    # browser()
    # create the graph using to and from columns. Requires a matrix input
    sim$g <- igraph::graph.edgelist(edge_mat, dir = FALSE)

    # assign weights to the graph. Requires a matrix input
    igraph::E(sim$g)$weight <- as.matrix(edges.weight)[,3]

    #------clean up
    rm(edges.weight)
    return(invisible(sim))
  }
}
