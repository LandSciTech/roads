
#' copied from newRoadsToLines and made work with sf

pathsToLines <- function(sim){
  ## existing roads
  er <- sim$costSurface == 0 
  
  linelist <- lapply(1:length(sim$paths.list), function(i){
    
    # finds first match for start and end cells in paths
    inds <- match(sim$paths.list[[i]], sim$paths.v$V1)
    
    if(any(is.na(inds))){
      stop("NA values in cost raster along paths, check raster", call. = FALSE)
    }
    
    if(inds[1] == inds[2]){
      return(NULL)
    }
    # cell indicies for vertices on line
    v <- sim$paths.v$V1[inds[1]:inds[2]]
    
    # remove vertices in this line from sim object in parent environment
    sim$paths.v <<- sim$paths.v[-(inds[1]:inds[2]), ]
    
    ## index of where new road connects to existing road
    conn <- which(er[v] == 1)
    
    ## remove portions that run along existing road, if applicable
    if(length(conn) > 0){
      keep <- which(er[v] == 0)
      if(min(keep) > 1){
        keep <- c(min(keep)-1, keep)
      }
      if(max(keep) < length(v)){
        keep <- c(keep, max(keep)+1)
      }
      v <- v[keep]
    }
    
    #id <- names(sim$paths.list)[[i]]
    
    outLine <- sf::st_linestring(raster::xyFromCell(sim$costSurface, v))
    
    return(outLine)
  })
  
  outLines <- sf::st_as_sfc(linelist)  %>% 
    sf::st_union() %>% 
    {sf::st_sf(geometry = .)}
  return(sf::st_set_crs(outLines, sf::st_crs(sim$roads)))
}