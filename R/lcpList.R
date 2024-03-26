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


lcpList<- function(sim){
  if(nrow(sim$landings) == 0){
    return(invisible(sim))
  }
  ##Get a list of of cell indexs for to and from points
  paths.matrix <- cbind(terra::cellFromXY(sim$weightRaster,
                                         sf::st_coordinates(sim$landings)), 
                      terra::cellFromXY(sim$weightRaster, sim$roads.close.XY))
  
  sim$paths.list <- split(paths.matrix, 1:nrow(paths.matrix))
  
  rm(paths.matrix)
  
  return(invisible(sim))
}
