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

#' Data from the CLUS example
#'
#' From Kyle Lochhead and Tyler Muhly's CLUS road simulation example. SpatRaster
#' files created with the terra package must be saved with [terra::wrap()] and
#' need to be unwrapped before they are used. [prepExData()] does this.
#'
#' @docType data
#'
#' @usage data(CLUSexample)
#'
#' @format A named list where: $cost is an object of class PackedSpatRaster
#'   representing road cost, $landings is an object of class sf representing
#'   landing locations and $roads is an object of class PackedSpatRaster
#'   representing existing roads
#'
#' @examples
#' CLUSexample
#' prepExData(CLUSexample)
"CLUSexample"
