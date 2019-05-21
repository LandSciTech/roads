#' Visualize roads projection input scenario and, optionally, output.
#'
#' @details
#' some details...
#'
#' @param costRast A RasterLayer represening the cost of new roads. Existing road network will be identiifed by cells with a cost of zero.
#' @param landings SpatialPointsDataFrame*, SpatialPoints*, SpatialPolygonsDataFrame*, SpatialPolygons*, RasterLayer, RasterStack, or RasterBrick.
#' Represents landing locations, or areas in the case of polygons. Layers in RasterStack or RasterBrick objects are assumed to represent multiple time points.
#' If NA or NULL, landing locations will not be inculuded in the visualization.
#' @param projRoadsResults NA, NULL, or one of the following return types from projectRoads:  list or RasterBrick.
#' If NA or NULL, projected roads will not be inculuded in the visualization.
#' @param col.cost A vector of colours (as returned by a colour palette). Represents colours for displaying cost.
#' If NA or NULL, a default colour palette will be used, based on colorRamps::matlab.like.
#' @param main Character. The main title for the visualization.
#' @param xlim An integer vector of length 2 representing the minumum[0] and maximum[1] of the range (in the x direction) of the cost raster that
#' is to be shown. If NA or NULL, the full range of the cost raster will be shown in the x direction.
#' @param ylim An integer vector of length 2 representing the minumum[0] and maximum[1] of the range (in the y direction) of the cost raster that
#' is to be shown. If NA or NULL, the full range of the cost raster will be shown in the y direction.
#' @param out.file A character representing the path to an output image file.  If NA or NULL, the visualization will be generated in a new graphics
#' device. If a path to a file is provided, an image file will be generated at the specified location, containing the visualization. Supported file types are .png,
#' .jpg, .bmp, .tif, .pdf.
#' @examples
#' scen <- demoScen[[1]] # demonstration scenario 1
#' landings <- scen$landings.points[scen$landings.points$set%in%1:3,] # use landing sets 1 to 3
#' visualize(scen$cost.rast,landings) # visualize just the input scenario
#' # project roads
#' pr <- projectRoads(landings,scen$cost.rast,scen$cost.rast==0,roadMethod='mst')
#' # visualize both the input scenario and the projected roads
#' visualize(scen$cost.rast,landings,pr,main='Scenario 1: Landing sets 1 to 3')
#' # zoom in on the lower left quadrant
#' visualize(scen$cost.rast,landings,pr,xlim=c(0,50),ylim=c(0,50))
#' # generate the same visualization as above, but with a window that is 20 cm hight (monitor permitting)
#' visualize(scen$cost.rast,landings,pr,xlim=c(0,50),ylim=c(0,50),height=20)
#'
#' # generate visualization as an image file instead of a new graphics device:
#' ## set working directory as necessary, e.g. setwd('c:/myOutputFolder')
#' visualize(scen$cost.rast,landings,pr,main='Scenario 1',height=20,out.file='myVisualization.png')
#'
#'
#' scen <- demoScen[[2]] # demonstration scenario 2
#' landings <- scen$landings.poly # use landing polygons
#' # project roads and visualize
#' pr <- projectRoads(landings,scen$cost.rast,scen$cost.rast==0,roadMethod='mst')
#' visualize(scen$cost.rast,landings,pr,main='Scenario 2: Polygonal landings')
#'
#'
#' scen <- demoScen[[3]] # demonstration scenario 3
#' visualize(scen$cost.rast) # visualize just the cost raster and existing roads network
#' # use landing sets 1 to 4 of the landings RasterStack for multi-temporal roads projection
#' landings <- scen$landings.stack[[1:4]]
#' # project roads and visualize
#' pr <- projectRoads(landings,scen$cost.rast,scen$cost.rast==0,roadMethod='mst')
#' visualize(scen$cost.rast,landings,pr) # visualize results
#'# zoom in on the lower left corner
#' visualize(scen$cost.rast,landings,pr,xlim=c(0,50),ylim=c(0,35))
#' @rdname visualize
#' @export
#' 
visualize <- function(costRast,landings=NA,projRoadsResults=NA,col.cost=NA,main='',xlim=NA,ylim=NA,height=15,out.file=NA){
  graphics.off()
  #########################
  ### CHECK AND ADJUST INPUTS
  ## set NULL inputs to NA
  if (is.null(landings)){landings<-NA}
  if (is.null(projRoadsResults)){projRoadsResults<-NA}
  if (is.null(col.cost)){col.cost<-NA}
  if (is.null(xlim)){xlim<-NA}
  if (is.null(ylim)){ylim<-NA}  
  if (is.null(out.file)){out.file<-NA}
  ## default heightof 12 cm
  if (is.null(height)){height<-15}
  if (is.na(height)){height<-15}
  # if user entered NULL or NA for main, use the default ''
  if (is.null(main)){
    main <- ''
  }else if(is.na(main)){
    main <- ''
  }
  ## use default colour ramp if col.cost is NA
  if (any(is.na(col.cost))){
    col.cost <- c(colorRamps::matlab.like(255+50+50)[51:(50+255)])
  }
  ## check and adjust (if necessary) out.file
  if(!is.na(out.file)){
    splitpath <- base::strsplit(out.file,'/|\\\\')[[1]]
    if (length(splitpath)==1){
      # if just a filename specified, use the current working directory
      out.file <- paste0(getwd(),'/',out.file)
    }else{
      # otherwise, ensute that the specified directory exists. If not, stop and notify user.
      dir <- paste(utils::head(strsplit(out.file,'/|\\\\')[[1]],-1),collapse='/')
      if(!dir.exists(dir)){
        stop('Could not find directory for output image file:\n             ',dir)
      }
    }
  }
  spatialSubsetting <- FALSE # indicates whether spatial subsetting of the extent will occur for display
  ## prepare and check xlim and ylim
  if (any(is.na(xlim))){
    xlim<-c(costRast@extent@xmin,costRast@extent@xmax)
  }else{
    if (length(xlim)!=2){
      stop('xlim should be NA, or a numerical vector of length 2.')
    }
    if(xlim[1]>costRast@extent@xmin | xlim[1]<costRast@extent@xmin){
      spatialSubsetting <- TRUE # spatial subsetting of the extent will occur for display
    }
    if(xlim[1]<costRast@extent@xmin){
      xlim[1]<-costRast@extent@xmin
      }
    if(xlim[2]>costRast@extent@xmax){
      xlim[2]<-costRast@extent@xmax
      }
  }
  if (any(is.na(ylim))){
    ylim<-c(costRast@extent@ymin,costRast@extent@ymax)
  }else{
    if (length(ylim)!=2){
      stop('ylim should be NA, or a numerical vector of length 2.')
    }
    if(ylim[1]>costRast@extent@ymin | ylim[2]<costRast@extent@ymax){
      spatialSubsetting <- TRUE # spatial subsetting of the extent will occur for display
    }
    if(ylim[1]<costRast@extent@ymin){
      ylim[1]<-costRast@extent@ymin
      }
    if(ylim[2]>costRast@extent@ymax){
      ylim[2]<-costRast@extent@ymax
      }
  }
  ## landings prep (prepped landings called 'land')
  if (is(landings,'SpatialPointsDataFrame')){
    ## if landings are already SpatialPointsDataFrame, keep as is
    land <- landings
  }else if (is(landings,'SpatialPoints') & !is(landings,'SpatialPointsDataFrame')){
    ## if landings are SpatialPoints but not SpatialPointsDataFrame, convert to
    ## SpatialPointsDataFrame with no 'set' column
    land <- sp::SpatialPointsDataFrame(landings,data=data.frame(z=rep(1,length(landings))))
  }else if ( is(landings,'RasterStack') | is(landings,'RasterBrick') ){
    ## if landings are a RasterStack or RasterBrick, convert to SpatialPointsDataFrame with 'set'
    ## representing the layer number
    land <- do.call(rbind,lapply(1:raster::nlayers(landings),FUN=function(r){
      pnts<-raster::rasterToPoints(landings[[r]],fun=function(x){x>=1},spatial=TRUE)
      names(pnts@data)<-'set'
      pnts$set<-r
      return(pnts)
    }))
  }else if (is(landings,'RasterLayer')){
    ## if landings are a single RasterLayer, convert to SpatialPointsDataFrame with no 'set' column
    land <- raster::rasterToPoints(landings,fun=function(x){x>=1},spatial=TRUE)
  }else if (is(landings,'SpatialPolygonsDataFrame')){
    land <- landings
  }else if ( is(landings,'SpatialPolygons') & !is(landings,'SpatialPolygonsDataFrame') ){
    ## if landings are SpatialPolygons but not SpatialPolygonsDataFrame, convert to
    ## SpatialPolygonsDataFrame with no 'set' column
    land <- sp::SpatialPolygonsDataFrame(landings,data=data.frame(z=rep(1,length(landings))))
  }else{
    land <- NA
  }
  #########################
  ### TEXT SIZE / SYMBOLS / SPACING ADJUSTMENT
  ## text size and symmbol settings
  ## graphics display height (in cm). Will be multiplied by 2.5 if plotting to out.file
  dev.height.in <- height*0.393701
  dev.height.ratio <- dev.height.in/10
  cex.main <- 2*dev.height.ratio      ## text size magnification for the main title
  cex.rast.axis <- 1*dev.height.ratio*1.5 ## text size magnification for raster axes labels
  cex.leg.mainlabels <- 2*dev.height.ratio ## text size magnification for labels in the legend
  col.roads.original <- 'black'  ## colour for original roads
  col.newRoad.single <- 'grey50' ## colour for new roads, when there is a single time point
  if (is(projRoadsResults,'RasterBrick')){
    nlayers <- raster::nlayers(projRoadsResults)
    ## colour for new roads, when there is a multi-temporal roads projection
    col.newRoad.multi <- grDevices::grey.colors(nlayers+1)[2:nlayers]
  }
  cex.land.pnts <- 3  ## symbol size magnification for landings points
  land.pch <- 21      ## landing point symbols (see ?pch)
  land.bg  <- 'white' ## landing point symbol background colour (if land.pch represents a symbol that uses a bg colour)
  density.land.poly <- 20 ## density for shading lines if landings are polygons
  ## legend spacing adjustments
  cost.x.left <- 0.01
  cost.x.right <- 0.2
  cost.y.top <- 0.9
  cost.y.bottom <- 0.7
  text.x.nudge <- 0.05
  text.y.nudge <- 0.015
  yspacing.elements <- 0.1
  #########################
  ### PREPARE LAYOUT AND GRAPHICS DEVICE
  xTOy <- diff(xlim)/diff(ylim)
  lyt.mat <- matrix(NA,nrow=43+7,ncol=43*xTOy+7+12)
  lyt.mat[,1:3] <- 4
  lyt.mat[,4:(ncol(lyt.mat)-16)] <- 1
  lyt.mat[,(ncol(lyt.mat)-15):(ncol(lyt.mat)-12)] <- 5
  lyt.mat[,(ncol(lyt.mat)-11):ncol(lyt.mat)] <- 2
  lyt.mat[1:4,] <- 3
  lyt.mat[(nrow(lyt.mat)-2):nrow(lyt.mat),] <- 6
  dev.width.in <- (dev.height.in*1.24)-(1-xTOy)*(43/50)*dev.height.in
  ### graphics device, based on user-specification for out.file
  if(!is.na(out.file)){
    ext <- utils::tail(strsplit(out.file,'\\.')[[1]],1)
    if (ext=='png'){
      grDevices::png(out.file,height=dev.height.in,width=dev.width.in,units='in',res=300)
    }else if (ext=='jpg'){
      grDevices::jpeg(out.file,height=dev.height.in,width=dev.width.in,units='in',res=300)
    }else if (ext=='bmp'){
      grDevices::bmp(out.file,height=dev.height.in,width=dev.width.in,units='in',res=300)
    }else if (ext=='tif'){
      grDevices::tiff(out.file,height=dev.height.in,width=dev.width.in,units='in',res=300)
    }else if (ext=='pdf'){
      grDevices::pdf(out.file,height=dev.height.in,width=dev.width.in)
    }
  }else{
    grDevices::dev.new(height=dev.height.in,width=dev.width.in,noRStudioGD=T)
  }
  lyt <- graphics::layout(lyt.mat) ## apply layout
  #########################
  ### MAIN PLOT PREPARATION
  graphics::par(xaxs='i',yaxs='i')
  if(dev.height.ratio<0){
    graphics::par(mar=c(rep(2-2*dev.height.ratio,2),rep(3-3*dev.height.ratio,2)))
  }else{
    graphics::par(mar=c(1,1,2,2))
  }
  #########################
  ### MAIN PLOT: COST RASTER
  ## get copy of cost raster with existing roads excluded
  rastNoZero <- costRast
  rastNoZero[rastNoZero==0] <- NA
  ## get range of cost values
  cost.range <- c(raster::minValue(rastNoZero),raster::maxValue(rastNoZero))
  if (spatialSubsetting){
    ## spatially subset rastNoZero to xlim and ylim and display
    ## subsetting is done to avoid memory error when using raster::image with small xlim/ylim extent relative to full image extent
    ylim.rows <- raster::rowFromY(rastNoZero,c(ylim[1]+1,ylim[2]))
    xlim.cols <- raster::colFromX(rastNoZero,c(xlim[1],xlim[2]-1))
    rastNoZero.display <- raster::raster(x=matrix(rastNoZero[ylim.rows[2]:ylim.rows[1],xlim.cols[1]:xlim.cols[2]],
                                                  byrow=T,ncol=abs(diff(xlim.cols))+1),
                                         xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2],crs=costRast@crs)
    raster::image(rastNoZero.display,axes=F,col=col.cost,
                  breaks=seq(cost.range[1],cost.range[2],diff(cost.range)/length(col.cost)),
                  xlab=NA,ylab=NA,main=NA,xpd=F)
  }else{
    raster::image(rastNoZero,axes=F,col=col.cost,breaks=seq(cost.range[1],cost.range[2],diff(cost.range)/length(col.cost)),
                  xlab=NA,ylab=NA,main=NA,xpd=F)
  }
  box()
  ## axes
  xTicks <- graphics::axTicks(1)[graphics::axTicks(1)>=costRast@extent@xmin & graphics::axTicks(1)<=costRast@extent@xmax]
  yTicks <- graphics::axTicks(2)[graphics::axTicks(2)>=costRast@extent@ymin & graphics::axTicks(2)<=costRast@extent@ymax]
  graphics::axis(1,at=xTicks,labels=xTicks,pos=ylim[1],padj=-1+dev.height.ratio,cex.axis=cex.rast.axis,xpd=T) # bottom axis
  graphics::axis(3,at=xTicks,labels=xTicks,pos=ylim[2],padj=1-dev.height.ratio,cex.axis=cex.rast.axis,xpd=T)  # top axis
  graphics::axis(2,at=yTicks,labels=yTicks,pos=xlim[1],las=1,cex.axis=cex.rast.axis,xpd=T)    # left axis
  graphics::axis(4,at=yTicks,labels=yTicks,pos=xlim[2],las=1,cex.axis=cex.rast.axis,xpd=T)    # right axis
  #########################
  ### MAIN PLOT: ROADS (PROJECTED AND EXISTING)
  if (is(projRoadsResults,'list') | is(projRoadsResults,'RasterLayer')){
    ## roads from based on a single time step
    if (is(projRoadsResults,'list')){
      ## if projRoadsResults is a list returned by roads::projectRoads, get new roads as a logical raster RasterLayer
      if (!('roads'%in%names(projRoadsResults))){
        stop('Unexpected input.  projRoadsResults should be specified as the object returned by roads::projectRoads.',
             ' If a list, it should contain an component named \'roads\'.')
      }
      roads <- projRoadsResults$roads > 0 # new roads
    }else{
      ## otherwise, it is a RasterLayer
      if (raster::dataType(projRoadsResults)!='LOG1S'){
        # if not a logical RasterLayer, assume it is an integer RasterLayer, where values > 0 represent new roads,
        # and get new roads as a logical RasterLayer
        roads <- projRoadsResults > 0
      }else{
        ## otherwise, assume that projRoadsResults is already a logical RasterLayer representing new roads
        roads <- projRoadsResults
      }
    }
    ## plot the new roads and existing roads
    if ('roads'%in%names(projRoadsResults)){
      roads <- projRoadsResults$roads > 0 # new roads
      roads[!roads] <- NA
      raster::image(roads,add=T,col=col.newRoad.single)
      roads <- costRast==0 # original roads
      roads[!roads] <- NA
      if (spatialSubsetting){
        # subset roads to xlim,xlim and display
        roads.display <- raster::raster(x=matrix(roads[ylim.rows[2]:ylim.rows[1],xlim.cols[1]:xlim.cols[2]],
                                                 byrow=T,ncol=abs(diff(xlim.cols))+1),
                                        xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2],crs=costRast@crs)
        # suppressWarnings used for case where warning is raised when no roads are present in the given xlim/ylim
        suppressWarnings(raster::image(roads.display,add=T,col=col.roads.original))
      }else{
        raster::image(roads,add=T,col=col.roads.original)
      }
    }else{
      stop('Unexpected input.  projRoadsResults should be specified as ',
           'the object returned by roads::projectRoads.')
    }
  }else if (is(projRoadsResults,'RasterBrick')){
    ## roads from based on multiple time steps
    roads <- raster::raster(ext=raster::extent(costRast),res=raster::res(costRast),
                            crs=raster::crs(costRast))
    nlayers <- raster::nlayers(projRoadsResults)
    for (i in nlayers:1){roads[projRoadsResults[[i]]]<-i}
    if (spatialSubsetting){
      # subset roads to xlim,xlim and display
      roads.display <- raster::raster(x=matrix(roads[ylim.rows[2]:ylim.rows[1],xlim.cols[1]:xlim.cols[2]],
                                               byrow=T,ncol=abs(diff(xlim.cols))+1),
                                      xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2],crs=costRast@crs)
      # suppressWarnings used for case where warning is raised when no roads are present in the given xlim/ylim
      suppressWarnings(raster::image(roads.display,col=c(col.roads.original,col.newRoad.multi),add=T))
    }else{
      raster::image(roads,col=c(col.roads.original,col.newRoad.multi),add=T)
    }
  }else if (is.na(projRoadsResults)){
    ## no projectRoads results, just plot existing roads (value of 0 in the cost raster)
    roads <- costRast==0 # existing roads
    roads[!roads] <- NA  # set non-roads to NA
    if (spatialSubsetting){
      # subset roads to xlim,xlim and display
      roads.display <- raster::raster(x=matrix(roads[ylim.rows[2]:ylim.rows[1],xlim.cols[1]:xlim.cols[2]],
                                               byrow=T,ncol=abs(diff(xlim.cols))+1),
                                      xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2],crs=costRast@crs)
      # suppressWarnings used for case where warning is raised when no roads are present in the given xlim/ylim
      suppressWarnings(raster::image(roads.display,col=col.roads.original,add=T))
    }else{
      raster::image(roads,col=col.roads.original,add=T)
    }
  }
  #########################
  ## MAIN PLOT: TITLE
  if(nchar(main)>0){
    mtext(main,side=3,padj=-(4/cex.main)*dev.height.ratio,cex=cex.main)
  }
  #########################
  ### MAIN PLOT: LANDING LOCATIONS
  if (is(land,'SpatialPointsDataFrame')){
    graphics::points(land,cex=cex.land.pnts,pch=land.pch,bg=land.bg,xpd=!spatialSubsetting)
    if ('set'%in%names(land)){
      graphics::text(land@coords,labels=land$set,font=2,adj=c(0.5,0.3),xpd=!spatialSubsetting)
    }
  }else if (is(land,'SpatialPolygonsDataFrame')){
    sp::plot(land,add=T,density=density.land.poly,xpd=F)
  }
  ######################
  ### LEGEND PREPARATION
  graphics::par(xaxs='i',yaxs='i')
  graphics::par(mar=c(0,0,0,0))
  graphics::plot(x=0,y=0,xlim=c(0,1),ylim=c(0,1),axes=F,col=NA)
  ######################
  ## LEGEND: COST RASTER
  ## title
  cost.title <- 'Cost'
  graphics::text(cost.x.left,cost.y.top+text.y.nudge*2.5,cost.title,adj=c(0,-0.5),cex=cex.leg.mainlabels,xpd=T) # colour ramp title
  graphics::lines(c(cost.x.left,cost.x.left+strwidth(cost.title,cex=cex.leg.mainlabels)),rep(cost.y.top+text.y.nudge*2.7,2),
                  lwd=2*dev.height.ratio) # underline title
  ## colour ramp
  col.n <- length(col.cost)
  z <- sapply(1:col.n,function(x){graphics::rect(cost.x.left,cost.y.bottom+(cost.y.top-cost.y.bottom)/col.n*(x-1),
                                                 cost.x.right,cost.y.bottom+(cost.y.top-cost.y.bottom)/col.n*x,
                                                 col=col.cost[x],border=NA,xpd=T)})
  graphics::rect(cost.x.left,cost.y.bottom,cost.x.right,cost.y.top,lwd=2*dev.height.ratio,xpd=T) # border around colour ramp
  ## colour ramp ticks and labels
  z <- sapply(pretty(cost.range,5),function(x){if(x>=cost.range[1] & x<=cost.range[2]){
    y.coord <- cost.y.bottom+(cost.y.top-cost.y.bottom)*((x-cost.range[1])/diff(cost.range))
    graphics::lines(c(cost.x.right,cost.x.right+text.x.nudge),rep(y.coord,2),xpd=T,lwd=2*dev.height.ratio,xpd=T)
    graphics::text(x=cost.x.right+text.x.nudge*2,y=y.coord,round(x,2),adj=0,cex=cex.leg.mainlabels,xpd=T)
  }})
  graphics::text(x=cost.x.left,y=cost.y.bottom-text.y.nudge*1.5,round(cost.range[1],2),adj=0,cex=cex.leg.mainlabels,xpd=T)
  graphics::text(x=cost.x.left,y=cost.y.top+text.y.nudge*1.5,round(cost.range[2],2),adj=0,cex=cex.leg.mainlabels,xpd=T)
  ######################
  ## LEGEND: ROADS
  roads.y.top <- cost.y.bottom - yspacing.elements*1.25
  if (is(projRoadsResults,'RasterBrick')){
    ## project roads results in the form of a RasterBrick
    n.tsteps <- length(col.newRoad.multi)
    roads.y.bottom <- roads.y.top - 0.2
    if ((n.tsteps<4)){
      roads.y.bottom <- roads.y.bottom+(roads.y.top-roads.y.bottom)*(4-n.tsteps)/5
    }
    ## title
    roads.title <- 'Roads at time t'
    graphics::text(cost.x.left,roads.y.top+text.y.nudge,roads.title,adj=c(0,-0.5),cex=cex.leg.mainlabels,xpd=T) ## colour ramp title
    graphics::lines(c(cost.x.left,cost.x.left+strwidth(roads.title,cex=cex.leg.mainlabels)),rep(roads.y.top+text.y.nudge*1.2,2),
                    lwd=2*dev.height.ratio) ## underline title
    ## colour ramp
    z <- sapply(1:(n.tsteps+1),function(x){graphics::rect(cost.x.left,roads.y.bottom+(roads.y.top-roads.y.bottom)/(n.tsteps+1)*(x-1),
                                                          cost.x.right,roads.y.bottom+(roads.y.top-roads.y.bottom)/(n.tsteps+1)*x,
                                                          col=c(col.roads.original,col.newRoad.multi)[x],border=NA,xpd=T)})
    graphics::rect(cost.x.left,roads.y.bottom,cost.x.right,roads.y.top,lwd=2*dev.height.ratio,xpd=T) ## border around colour ramp
    # ticks and labels
    z <- sapply(pretty(0:n.tsteps,3),function(x){if (x<=n.tsteps){
      y.coord <- roads.y.bottom+(roads.y.top-roads.y.bottom)*((x+0.5)/(n.tsteps+1))
      graphics::lines(c(cost.x.right,cost.x.right+text.x.nudge),rep(y.coord,2),xpd=T,lwd=2*dev.height.ratio,xpd=T)
      graphics::text(x=cost.x.right+text.x.nudge*2,y=y.coord,labels=paste0('t = ',x),adj=0,cex=cex.leg.mainlabels,xpd=T)
    }})
  }else if(is(projRoadsResults,'list') | is(projRoadsResults,'RasterLayer')){
    ## project roads results in the form of a list or RasterLayer
    roads.boxheight <- 0.04
    ## title
    roads.title <- 'Roads'
    graphics::text(cost.x.left,roads.y.top+text.y.nudge,roads.title,adj=c(0,-0.5),cex=cex.leg.mainlabels,xpd=T) ## colour ramp title
    graphics::lines(c(cost.x.left,cost.x.left+strwidth(roads.title,cex=cex.leg.mainlabels)),rep(roads.y.top+text.y.nudge*1.2,2),
                    lwd=2*dev.height.ratio) ## underline title
    ## new roads
    graphics::rect(cost.x.left,roads.y.top-roads.boxheight,cost.x.right,roads.y.top,col=col.newRoad.single,border=T,
                   lwd=2*dev.height.ratio,xpd=T)
    graphics::text(cost.x.right+text.x.nudge,roads.y.top-roads.boxheight/2,'Projected roads',adj=c(0,0.5),
                   cex=cex.leg.mainlabels,xpd=T) ## colour ramp title
    ## existing roads
    graphics::rect(cost.x.left,roads.y.top-roads.boxheight*2.5,cost.x.right,roads.y.top-roads.boxheight*1.5,
                   col=col.roads.original,border=T,lwd=2*dev.height.ratio,xpd=T)
    graphics::text(cost.x.right+text.x.nudge,roads.y.top-roads.boxheight*2,'Existing roads',adj=c(0,0.5),
                   cex=cex.leg.mainlabels,xpd=T) ## colour ramp title
    ##
    roads.y.bottom <- roads.y.top-roads.boxheight*2.5
  }else if (is.na(projRoadsResults)) {
    roads.boxheight <- 0.04
    ## title
    roads.title <- 'Roads'
    graphics::text(cost.x.left,roads.y.top+text.y.nudge,roads.title,adj=c(0,-0.5),cex=cex.leg.mainlabels,xpd=T) ## colour ramp title
    graphics::lines(c(cost.x.left,cost.x.left+strwidth(roads.title,cex=cex.leg.mainlabels)),rep(roads.y.top+text.y.nudge*1.2,2),
                    lwd=2*dev.height.ratio) ## underline title
    ## existing roads
    graphics::rect(cost.x.left,roads.y.top-roads.boxheight,cost.x.right,roads.y.top,col=col.roads.original,border=T,
                   lwd=2*dev.height.ratio,xpd=T)
    graphics::text(cost.x.right+text.x.nudge,roads.y.top-roads.boxheight/2,'Existing roads',adj=c(0,0.5),cex=cex.leg.mainlabels,xpd=T) ## colour ramp title
    roads.y.bottom <-  roads.y.top-roads.boxheight*1.5
  }
  ######################
  ## LEGEND: LANDINGS
  if (is(land,'SpatialPointsDataFrame') | is(land,'SpatialPolygonsDataFrame')){
    land.y.top <- roads.y.bottom - yspacing.elements
    ## title
    land.title <- 'Landings'
    graphics::text(cost.x.left,land.y.top+text.y.nudge*1.5,land.title,adj=c(0,-0.5),cex=cex.leg.mainlabels,xpd=T) ## colour ramp title
    graphics::lines(c(cost.x.left,cost.x.left+strwidth(land.title,cex=cex.leg.mainlabels)),rep(land.y.top+text.y.nudge*1.5*1.2,2),
                    lwd=2*dev.height.ratio) ## underline title
  }
  if (is(land,'SpatialPointsDataFrame')){
    ## landings point
    graphics::points(x=(cost.x.left+cost.x.right)/2,y=land.y.top,cex=cex.land.pnts,pch=land.pch,bg=land.bg,xpd=T)     ## point symbol
    graphics::text(cost.x.right+text.x.nudge,land.y.top,'Landing location',adj=c(0,0.4),cex=cex.leg.mainlabels,xpd=T) ## text label
  }else if (is(land,'SpatialPolygonsDataFrame')){
    land.boxheight <- 0.04
    ## landings polygon.rectangle
    graphics::rect(cost.x.left,land.y.top+text.y.nudge/2-land.boxheight,cost.x.right,land.y.top+text.y.nudge/2,density=density.land.poly,xpd=T) ## polygon
    graphics::text(cost.x.right+text.x.nudge,land.y.top+text.y.nudge*0.5-land.boxheight/2,'Landing area',adj=c(0,0.3),cex=cex.leg.mainlabels,xpd=T) ## text label
  }
  ## if plotting to file, turn off the graphics device
  if(!is.na(out.file)){
    z<-dev.off()
  }
}


