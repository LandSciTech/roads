#' Grade penalty edge weight function
#'
#' Method for calculating the weight of an edge between two nodes from the value
#' of the input raster at each of those nodes (`x1` and `x2`), designed for a single
#' DEM input. The method assumes an input `weightRaster` in which:
#'   * `NA` indicates a road cannot be built
#'   * Negative values are costs for crossing streams or other barriers that are
#'   crossable but expensive. Edges that link to barrier penalty (negative value)
#'   nodes are assigned the largest barrier penalty weight.
#'   * Zero values are assumed to be existing roads.
#'   * All other values are interpreted as elevation in the units of the raster
#'    map (so that a difference between two cells equal to the map resolution can be
#'     interpreted as 100% grade)
#' This is a simplified version of the grade penalty approach taken by Anderson and Nelson (2004):
#' The approach does not distinguish between adverse and favourable grades.
#' Default construction cost values are from the BC interior appraisal manual.
#' The approach ignores (unknown) grade penalties beside roads and barriers in order to
#' avoid increased memory and computational burden associated with multiple input rasters.
#' @references Anderson AE, Nelson J (2004) Projecting vector-based road networks with a
#'  shortest path algorithm. Canadian Journal of Forest Research 34:1444–1457. https://doi.org/10.1139/x04-030
#'
#' @param x1,x2 Number. Value of the input raster at two nodes.
#' @param hdistance Number. Horizontal distance between nodes. `hdistance`, `x1`, and `x2`
#'  should have the same units.
#' @param baseCost Number. Construction cost of 0% grade road per km.
#' @param limit Number. Maximum grade (%) on which roads can be built.
#' @param penalty Number. Cost increase (per km) associated with each
#'   additional % increase in road grade.
#' @param limitWeight Number. Value assigned to edges that exceed the grade
#'   limit. Try setting to a high (not `NA`) value if encountering problems with
#'   disconnected graphs.
#' 
#' @export
#'
#' @examples
#' gradePenaltyFn(0.5,0.51,1)
#' gradePenaltyFn(0.5,0.65,1)
#' # grade > 20% so NA
#' gradePenaltyFn(0.5,0.75,1)
gradePenaltyFn <- function(x1, x2, hdistance, baseCost = 16178, limit = 20,
                           penalty = 504, limitWeight = NA){

  # Don't calculate grade penalty cost if one of the nodes is a barrier.
  cond <- pmin(x1, x2) >= 0
  cond[is.na(cond)] <- F

  # Apply grade penalty if both nodes have elevation values.
  # If one node is a road use base cost.
  grade <- 100 * abs(x1 - x2) * cond / hdistance #This is % (dimensionless)

  slp <- baseCost + grade * penalty * (pmin(x1, x2) > 0) #This is cost per km.
  slp[grade > limit] <- limitWeight

  # If both 0 this is an existing road link. Otherwise it is a barrier.
  slp[!cond] <- abs(pmin(x1, x2))[!cond]

  # Multiply by hdistance to penalize diagonals.
  # Note that multiplying all edge weights by a constant does not change algorithm behaviour,
  # so we ignore the unit of hdistance here.
  slp = slp*hdistance

  return(slp)
}


#' Simple cost edge weight function
#'
#' Calculates the weight of an edge between two nodes as the mean value
#' of an input cost raster at each of those nodes (`x1` and `x2`).
#'
#' @param x1,x2 Number. Value of the input cost raster at two nodes.
#' @param hdistance Number. Horizontal distance between the nodes - for penalizing longer diagonal edges.
#' @export
#'
#' @examples
#' simpleCostFn(0.5,0.7,1)
simpleCostFn <- function(x1,x2,hdistance){

  # Multiply by hdistance to penalize diagonals.
  # Note that multiplying all edge weights by a constant does not change algorithm behaviour - so we ignore the unit of hdistance.
  return(hdistance*(x1+x2)/2)
}

