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

#' Grade penalty example data
#'
#' A list containing two rasters covering an area near Revelstoke, British
#' Columbia, Canada. `ex_elev` is elevation data and `ex_wat` is the proportion
#' of the cell that contains water. Both are subsets of data downloaded with the
#' geodata package at 30 arc seconds resolution.`SpatRaster` files created with
#' the terra package must be saved with [terra::wrap()] and need to be unwrapped
#' before they are used. [prepExData()] does this.
#' 
#' Elevation data are primarily from Shuttle Radar Topography Mission (SRTM),
#' specifically the hole-filled CGIAR-SRTM (90 m resolution) from
#' https://srtm.csi.cgiar.org/.
#' 
#' Water data are derived from the ESA WorldCover data set at 0.3-seconds
#' resolution. (License CC BY 4.0). See https://esa-worldcover.org/en for more
#' information.
#' 
#' @references Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., Souverijns,
#'   N., Brockmann, C., Quast, R., Wevers, J., Grosu, A., Paccini, A., Vergnaud,
#'   S., Cartus, O., Santoro, M., Fritz, S., Georgieva, I., Lesiv, M., Carter,
#'   S., Herold, M., Li, Linlin, Tsendbazar, N.E., Ramoino, F., Arino, O., 2021.
#'   ESA WorldCover 10 m 2020 v100. doi:10.5281/zenodo.5571936.
#' 
#' @docType data
#'
#' @usage data(dem_example)
#'
#' @format A named list with components: 
#' * ex_elev: a `PackedSpatRaster` of elevation.
#' * ex_wat: a `PackedSpatRaster` of proportion water.
#'
#' @examples
#' dem_example
#' prepExData(dem_example)
"dem_example"