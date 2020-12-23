
#' copied from newRoadsToLines and made work with sf

pathsToLines <- function(sim){
  ## pr is projectRoads results (or 'sim' when it contains the results)
  ## used for only the 'lcp' and 'mst' roadMethods
  
  ## existing roads
  er <- sim$costSurface == 0 
  
  linelist <- lapply(1:length(sim$paths.list), function(i){
    
    inds <- match(sim$paths.list[[i]], sim$paths.v$V1)
    if(inds[1] == inds[2]){
      return(NULL)
    }
    # cell indicies for vertices on line
    v <- sim$paths.v$V1[inds[1]:inds[2]]
    
    # remove vertices in this line from pr object in parent environment
    sim$paths.v <<- sim$paths.v[-(inds[1]:inds[2]), ]
    
    ## index of where new road connects to existing road
    conn <- match(1, er[v]) 
    
    if (!is.na(conn)){
      ## remove portions that run along existing road, if applicable
      if (conn==1){ 
        ## if road starts with connection to existing road, reverse it and find
        ## connection
        v <- rev(v)
        conn <- match(1,er[v])
      }
      
      ## remove portions that run along existing road
      v <- v[1:conn] 
    }
    
    #id <- names(sim$paths.list)[[i]]
    
    outLine <- sf::st_linestring(raster::xyFromCell(sim$costSurface, v))
    
    return(outLine)
  })
  
  outLines <- sf::st_as_sfc(linelist)  %>% 
    sf::st_union() %>% 
    {sf::st_sf(geometry = .)}
  return(sf::st_set_crs(outLines, sf::st_crs(sim$costSurface)))
}