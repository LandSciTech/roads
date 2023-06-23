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

#' Demonstration set of 10 input scenarios
#'
#' A demonstration set of scenarios that can be used as input to
#' \code{\link{projectRoads}} method. The data contains "SpatRaster" objects that
#' must be wrapped to be stored. To unwrap them use [prepExData()]
#'
#' @docType data
#'
#' @usage data(demoScen)
#'
#' @format A list of sub-lists, with each sub-list representing an input
#'   scenario. The scenarios (sub-lists) each contain the following components:
#'  \describe{
#'   \item{scen.number}{An integer value representing the scenario number (generated scenarios are numbered incrementally from 1).}
#'   \item{road.rast}{A logical PackedSpatRaster representing existing roads.  TRUE is existing road. FALSE is not existing road.}
#'   \item{road.line}{A sf object representing existing roads.}
#'   \item{cost.rast}{A PackedSpatRaster representing the cost of developing new roads on a given cell.}
#'   \item{landings.points}{A sf object representing landings sets and landing locations within each set. The data frame includes
#'   a field named 'set' which contains integer values representing the landings set that each point belongs to}
#'   \item{landings.stack}{A PackedSpatRaster with multiple layers representing the landings and landings sets. Each logical layer represents
#'   one landings set. Values of TRUE are a landing in the given set. Values of FALSE are not.}
#'   \item{landings.poly}{A sf object representing a single set of polygonal landings.}
#' }
#' 
#' @examples 
#' demoScen[[1]]
#' demoScen <- prepExData(demoScen)
#' demoScen[[1]]
#'
#' @seealso \code{\link{projectRoads}}

"demoScen"
