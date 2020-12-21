
#' copied from newRoadsToLines and made work with sf

pathsToLines <- function(pr){
  ## pr is projectRoads results (or 'sim' when it contains the results)
  ## used for only the 'lcp' and 'mst' roadMethods
  
  ## existing roads
  er <- pr$costSurface == 0 
  
  linelist <- lapply(1:length(pr$paths.list), function(i){
    
    inds <- match(pr$paths.list[[i]], pr$paths.v$V1)
    
    # cell indicies for vertexs on line
    v <- pr$paths.v$V1[inds[1]:inds[2]]
    
    # remove vertices in this line from pr object in parent environment
    pr$paths.v <<- pr$paths.v[-(inds[1]:inds[2]), ]
    
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
    
    id <- names(pr$paths.list)[[i]]
    
    return(sf::st_sf(
      geometry = sf::st_sfc(sf::st_linestring(raster::xyFromCell(pr$costSurface,
                                                                 v))),
      ID = id
    ))
  })
  
  outLines <- do.call(rbind, linelist)
  return(sf::st_set_crs(outLines, sf::st_crs(pr$costSurface)))
}