#' Grade penalty edge weight function
#'
#' Method for calculating the weight of an edge between two nodes from the value
#' of the input raster at each of those nodes (x1 and x2), designed for a single
#' DEM input. The method assumes an input raster in which:
#'   * NA indicates a road cannot be built
#'   * Negative values are costs for crossing streams or other barriers that are
#'   crossable but expensive. Edges that link to barrier penalty (negative value)
#'   nodes are assigned the largest barrier penalty weight.
#'   * Zero values are assumed to be existing roads.
#'   * All other values are interpreted as elevation in the units of the raster
#'    map (so that a difference between two cells equal to the map resolution can be
#'     interpreted as 100% grade)
#' This is a simplified version of the grade penalty approach taken by Anderson and Nelson:
#' The approach does not distinguish between adverse and favourable grades.
#' Default construction cost values are from the BC interior appraisal manual.
#' The approach ignores (unknown) grade penalties beside roads and barriers in order to
#' avoid increased memory and computational burden associated with multiple input rasters.
#'
#' @param x1,x2 Value of the input raster at two nodes. A difference of 1 implies a 100% slope.
#' @param resolution Resolution of the input raster. Resolution and x1/x2 should have the same units.
#' @param baseCost Construction cost of 0% grade road.
#' @param limit Maximum grade (%) on which roads can be built.
#' @param penalty Cost increase associated with each additional % increase in road grade.
#' @param limitWeight Value assigned to edges that exceed the grade limit. Try setting to a high (not NA) value if encountering problems with disconnected graphs.
#'
#' @export
#'
#' @examples
#' slopePenaltyFn(0.5,0.51)
#' slopePenaltyFn(0.5,0.65)
#' # grade > 20% so NA
#' slopePenaltyFn(0.5,0.75)
slopePenaltyFn<-function(x1,x2,resolution=1, baseCost = 16178,limit=20,penalty=504,limitWeight=NA){
  #If one of the nodes is a road or barrier ignore grade penalty
  cond = pmin(x1,x2)>=0
  cond[is.na(cond)]=F
  grade = 100*abs(x1-x2)*cond/resolution #percent slope, if both nodes have elevation values.
  #stop(print(max(grade)))
  slp = baseCost+grade*penalty*(pmin(x1,x2)>0)
  slp[grade>limit]=limitWeight

  slp[!cond] = abs(pmin(x1,x2))[!cond] # if both 0 this is an existing road link. Otherwise it is a barrier.

  return(slp)
}

