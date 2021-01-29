
#' copied from newRoadsToLines and made work with sf

pathsToLines <- function(sim){
  ## existing roads
  er <- sim$costSurface == 0 
  
  linelist <- lapply(1:length(sim$paths.list), function(i){
    
    # finds first match for start and end cells in paths
    inds <- match(sim$paths.list[[i]], sim$paths.v$V1)
    #print(i)
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
    
    #outLine1 <- sf::st_linestring(raster::xyFromCell(sim$costSurface, v))
    
    ## remove portions that run along existing road, if applicable. If there are
    ## multiple sections of new road with sections of old road in between we
    ## need to split it into multiple lines so it doesn't jump. Split at more
    ## than 1 true in a row followed by a false?
    ## something with cumsum(rle(er[v]==1)$lengths)?
    if(length(conn) > 0){
      keep <- which(er[v] == 0)
      if(length(keep) == 0){
        # the whole path is on existing road
        return(NULL)
      }
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
    
    # plot(sim$landings %>% st_geometry())
    # plot(sim$roads %>% st_geometry(), add = T)
    # plot(outLine1, col = "blue", add = T)
    # plot(outLine, col = "red", add = T)
    # title(main = i) %>% print()
    
    return(outLine)
  })
  
  # remove NULLs
  linelist <- linelist[vapply(linelist,function(x) !is.null(x), c(TRUE))]
  
  outLines <- sf::st_as_sfc(linelist)  %>% 
    sf::st_union() %>% 
    {sf::st_sf(geometry = .)}
  return(sf::st_set_crs(outLines, sf::st_crs(sim$roads)))
}