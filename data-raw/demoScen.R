library(raster)
library(colorRamps)
library(rgeos)
library(geoR)
###########################################################################
# function for plotting input scenarios for the roads::projectRoads method
#
# outPath:   a character string representing the output path for the PNG image that is to be generated (must end with '.png')
# function does not return anything, but PNG images will be generated at the user-specified outPath
#
# cost.rast: the raster for input into roads::projectRoads, as cost
#            - existing roads are expected to have a cost value of zero
# landings:  a spatial points data frame represeneting landings point locations, or NA for no plotting of point landings
#            - can have a column names "set" containing integers representing which landings set each point belongs to
# landings.poly:  a spatial polygons data frame represeneting landings polygon locations, or NA for no plotting of polygonal landings
# main:       a character string representing the main title for the plot. If NA, no main title will be added
# col:        a vector (length=255) rperesenting colours to use for cose. If NA, a default colour ramp will be used, based on colorRamps::matlab.like
# add.legend: logical representing whether or not to inlude the legend.  If TRUE, the legend will be added. If FALSE, no legend will be added
plot.scenario <- function(outPath,cost.rast,landings=NA,landings.poly=NA,main=NA,col=NA,add.legend=T){
          rast <- cost.rast
          rastNoZero <- rast
          rastNoZero[rastNoZero==0] <- NA
          cost.range <- c(raster::minValue(rastNoZero),raster::maxValue(rast))
          cex <- 1.8
          if (is.na(col[1])){col<-c("black",colorRamps::matlab.like(255+50+50)[50:(50+255)])}
          pathsplit <- strsplit(outPath,"\\\\|/")[[1]]
          if(!dir.exists(paste(pathsplit[-length(pathsplit)],collapse="/"))){
            stop("Directory for output image does not exist. Please check 'outPath'.")
          }
          graphics.off()
          if (!is.na(main) | add.legend){
            png(outPath,width=1548,height=1500,res=150)
          }else{
            png(outPath,width=1590,height=1500,res=150)
          }
          par(xaxs="i",yaxs="i")
          if(!is.na(main) | add.legend){par(mar=c(5,5,11,11))}else{par(mar=c(5,5,5,5))}
          raster::plot(rast,axes=F,legend=F,box=F,col=col)
          mtext(main,3,line=4.5,cex=cex*1.2) # main title, if user chose to include one
          xTicks <- axTicks(1)[axTicks(1)>=rast@extent@xmin & axTicks(1)<=rast@extent@xmax]
          yTicks <- axTicks(2)[axTicks(2)>=rast@extent@ymin & axTicks(2)<=rast@extent@ymax]
          axis(1,at=xTicks,labels=xTicks,pos=ymin(rast),padj=0.5,cex.axis=cex) # bottom axis
          axis(3,at=xTicks,labels=xTicks,pos=ymax(rast),padj=0,cex.axis=cex)   # top axis
          axis(2,at=yTicks,labels=yTicks,pos=xmin(rast),las=1,cex.axis=cex)    # left axis
          axis(4,at=yTicks,labels=yTicks,pos=xmax(rast),las=1,cex.axis=cex)    # right axis
          if (!suppressWarnings(is.na(landings))){
            if ("set"%in%names(landings)){
              if (is.integer(landings$set)){
                landings <- landings[order(landings$set),]
                if(length(unique(landings$set))>10){n.sets<-10}else{n.sets<-length(unique(landings$set))}
                for (set.ind in 1:n.sets){
                  if (set.ind<=5){
                    bg.col<-"white"
                    pch<-20+set.ind
                  }else{
                    bg.col<-"grey50"
                    pch<-20+set.ind-5
                  }
                  land <- subset(landings,subset=landings$set==unique(landings$set)[set.ind])
                  points(land,pch=pch,bg=bg.col,cex=0.7)
                }
              }else{stop("'set' field in landings is expected to be integer")}
            }else{
              points(landings,pch=21,bg="white",cex=0.7)
            }
          }
          if (!suppressWarnings(is.na(landings.poly))){
            plot(landings.poly,density=20,add=T)
          }
          # legend
          if(add.legend){
            yrng <- raster::extent(rast)@ymax-raster::extent(rast)@ymin
            xrng <- raster::extent(rast)@xmax-raster::extent(rast)@xmin
            leg.leftedge <- 1.175 # multiplier for placing the left edge of the legend alignment
            cost.x.add <- 0.05    # multiplier for placing the right edge of the cost colour ramp (also used in position of other legend elements)
            cost.y.bottom <- 0.75 # multiplier for placing the bottom edge of the cost colour ramp
            # cost / colour ramp
            text(leg.leftedge*xrng,1.09*yrng,"cost",adj=0,cex=cex,xpd=T)
            z <- sapply(1:255,function(x){rect(leg.leftedge*xrng,cost.y.bottom*yrng+0.25*yrng/255*(x-1),
                                               (leg.leftedge+cost.x.add)*xrng,cost.y.bottom*yrng+0.25*yrng/255*x,
                                               col=col[x+1],border=NA,xpd=T)})
            rect(leg.leftedge*xrng,cost.y.bottom*yrng,(leg.leftedge+cost.x.add)*xrng,(cost.y.bottom+0.25)*yrng,lwd=2,xpd=T)
            z <- sapply(pretty(cost.range,2),function(x){if(x>=cost.range[1] & x<=cost.range[2]){
              y.coord <- cost.y.bottom*yrng+0.25*yrng*((x-cost.range[1])/diff(cost.range))
              lines(c((leg.leftedge+cost.x.add)*xrng,(leg.leftedge+cost.x.add+cost.x.add/4)*xrng),rep(y.coord,2),xpd=T,lwd=2,xpd=T)
              text(x=(leg.leftedge+cost.x.add*1.5)*xrng,y=y.coord,round(x,2),adj=0,cex=cex*0.8,xpd=T)
              }})
            text(x=leg.leftedge*xrng,y=cost.y.bottom*yrng-0.028*yrng,round(cost.range[1],2),adj=0,cex=cex*0.8,xpd=T)
            text(x=leg.leftedge*xrng,y=yrng+0.028*yrng,round(cost.range[2],2),adj=0,cex=cex*0.8,xpd=T)
            #roads
            rect(leg.leftedge*xrng,0.6*yrng,(leg.leftedge+cost.x.add)*xrng,0.65*yrng,col=col[1],xpd=T)
            text((leg.leftedge+cost.x.add+0.015)*xrng,0.625*yrng,"road",adj=0,cex=cex*0.8,xpd=T)
            # landings
            if (!suppressWarnings(is.na(landings))){
              if ("set"%in%names(landings)){
                if (is.integer(landings$set)){
                  text(leg.leftedge*xrng,0.53*yrng,"landings",adj=0,cex=cex,xpd=T)
                  for (set.ind in 1:n.sets){
                    if (set.ind<=5){
                      bg.col<-"white"
                      pch<-20+set.ind
                    }else{
                      bg.col<-"grey50"
                      pch<-20+set.ind-5
                    }
                    land <- subset(landings,subset=landings$set==unique(landings$set)[set.ind])
                    points(x=(leg.leftedge+cost.x.add/2)*xrng,y=0.52*yrng-0.052*(set.ind)*yrng,pch=pch,bg=bg.col,cex=0.7,xpd=T)
                    text(x=(leg.leftedge+cost.x.add)*xrng,y=0.52*yrng-0.052*(set.ind)*yrng,
                         unique(landings$set)[set.ind],adj=0,cex=(cex)*0.9,xpd=T)
                    landings.ybottom <- 0.52*yrng-0.052*(set.ind)*yrng
                  }
                }
              }else{
                text(leg.leftedge*xrng,0.53*yrng,"landings",adj=0,cex=cex,xpd=T)
                points(x=(leg.leftedge-cost.x.add/2)*xrng,y=0.53*yrng,pch=21,bg="white",cex=0.7,xpd=T)
                landings.ybottom <- 0.53*yrng
              }
            }
            if (!suppressWarnings(is.na(landings.poly))){
              if (suppressWarnings(is.na(landings))){
                text(leg.leftedge*xrng,0.53*yrng,"landings",adj=0,cex=cex,xpd=T)
                landings.ybottom <- 0.53*yrng
              }
              rect(xleft=(leg.leftedge+cost.x.add/4)*xrng,xright=(leg.leftedge+cost.x.add*1.5)*xrng,
                   ytop=landings.ybottom-0.04*yrng,ybottom=landings.ybottom-0.09*yrng,density=20,xpd=T)
              text(x=(leg.leftedge+cost.x.add*1.8)*xrng,y=landings.ybottom-0.065*yrng,
                   "poly",adj=0,cex=(cex)*0.9,xpd=T)
            }
          }
          z<-dev.off()
} # end of plot.scenario function
############################################################################
# function for generating input scenarios for the roads::projectRoads method
#
# returns a list of sub-lists, with each sub-list representing an input scenario. The scenarios (sub-lists) contain the following named elements:
#       - scen.number: an integer value representing the scenario number (generated scenarios are numbered incrementally from 1 to n.scenarios)
#       - road.rast:   a logical RasterLayer representing existing roads.  TRUE is existing road. FALSE is not existing road.
#                      - these roads are a rasterized versions of the road.line SpatialLines
#       - road.line:   SpatialLines representing the existing roads
#       - cost.rast:   RasterLayer representing the cost of developing new roads on a given cell
#       - landings.points:  a SpatialPointsDataFrame representing landings sets and points
#                           - data frame includes a field named "set" containing integer values representing the landings set that each point belongs to
#       - landings.stack:  a RasterStack representing the landings and landings sets
#                          - each layer in the stack represents an individual landings set as a logical RasterLayer where TRUE is a landing in that set and FALSE is not
#                            a landing in the set
#       - landings.poly:  a SpatialPolygonsDataFrame representing a single set of polygonal landings
#                         - will be NA if user set n.poly.landings argument to NA
#
# n.scenarios: integer representing the number of scenarios that are to be generated
# xy.size:     vector (length=2) of integers representing the number of columns and number of rows, respectively, for the output cost raster
# spat.corr:   logical representing whether or not generated cost raster should be spatially correlated (gaussian random field) or not. TRUE for spatially correlated.
# cost.lim:    numeric value (must be > 1) representing either: the upper limit of the cost values (only if spat.corr is FALSE)
#                                                       or:     the mean value for the gaussian random field (only if spat.corr is TRUE)
#              - if spat.corr is TRUE, cost values will represent a gaussian random field with mean of cost.lim
#              - if spat.corr is TRUE, generated cost values that are less than 1 will be set equal to 1
#              - if spat.corr is FALSE, cost values will be a uniform random sample between 1 and cost.lim
# std.dev: (ignored if spat.corr is FALSE) numeric value representing standard deviation for the gaussian random field
#          - see cost.lim, above, for setting the mean of the gaussian random field
#          - std.dev setting will be used as the sigma^2 component of the 'cov.pars' parameter of the geoR::grf function
#          - generated cost values that are less than 1 will be set equal to 1
# range:   (ignored if spat.corr is FALSE) either a numeric value representing the range or a vector (length=2) of numeric values representing a upper and
#           lower limits for the range value
#          - range should only include values > 0.  If range is a vector, then range[1] should not equal range[2]
#          - the range value will be used to determine the phi component of the 'cov.pars' parameter of the geoR::grf function
#          - if range is a single numeric value, then that value will be used when generating each input scenario
#          - if range is a vector (length=2) of numeric values, then the range value used when generating each scenario will be uniformly randomly sampled
#            from between range[1] and range[2]
# n.roads: either an integer representing the number of initial roads in the landscape or a vector (length=2) of integers representing the upper and
#          lower limits for the number of initial roads in each landscape
#          - n.roads should only inlcude integers > 0. If n.roads is a vector, then n.roads[1] should not equal n.roads[2]
#          - if n.roads is a single integer value, then that value will be used when generating each input scenario (number of roads in each landscape will
#            eqaul n.roads)
#          - if n.roads is a vector (length=2) of integer values, then the number of roads in each scenario will be uniformly randomly sampled from between
#            n.roads[1] and n.roads[2]
#          - each road is a rasterized straight line, connecting opposite sides of the landscape (i.e. left-right, or top-bottom)
# n.landing.sets: an integer value (>0) representing the number of landing sets to generate for each scenario
# n.landings:  either an integer representing the number of landings to be included in each landings set or a vector (length=2) of integers representing the
#              upper and lower limits for the number of landings in each landing set
#              - n.landings should only include values > 0. If n.landings is a vector, then n.landings[1] should not equal n.landings[2]
#              - if n.landings is a single numeric value, then that value will be used when generating each landing set in each scenario
#              - if n.landings is a vector (length=2) of integer values, then the number of landings in each landing set of each scenario will be uniformly
#                randomly sampled from between n.landings[1] and n.landings[2]
# n.poly.landings: either NA (in which case polygonal landings will not be generated) or a vector (length=2) of integer values representing the upper and 
#                  lower limits for the number of polygonal landings in each scenario
#                  - polygonal landings will not be generated within 2 cells of: landscape edge, existing roads, or other polygonal landings
# poly.landings.xdim: (ignored if n.poly.landings is NA) a vector (length=2) of integer values representing the upper and lower limits of the width of each 
#                      polygonal landing, in # of cells
# poly.landings.ydim: (ignored if n.poly.landings is NA) a vector (length=2) of integer values representing the upper and lower limits of the height of each 
#                      polygonal landing, in # of cells
# seed.value:  either NA (in which case the random seed will not be set) or a numeric value representing the seed value for generating the random scenarios
#              - if seed.value is a numeric value, the random seed will be modified using the base::set.seed function
prepInputScenarios <- function(n.scenarios=10,xy.size=c(100,100),spat.corr=T,cost.lim=10,std.dev=20,range=c(0.1,2),
                               n.roads=c(1,4),n.landing.sets=10,n.landings=c(1,10),n.poly.landings=c(1,8),
                               poly.landings.xdim=c(3,20),poly.landings.ydim=c(3,20),seed.value=NA){
  if (!is.na(seed.value)){set.seed(seed.value)}
  if (cost.lim<=1){stop("cost.mean expected to be > 1")}
  outlist = list()
  for (i in 1:n.scenarios){
    scen <- list(scen.number=as.integer(i),
                 road.rast=NA,
                 road.line=NA,
                 cost.rast=NA,
                 landings.points=NA,
                 landings.stack=NA,
                 landings.poly=NA)
    if (spat.corr){
      if(length(range)==1){
        rfield <- geoR::grf(nsim=1,grid="reg",nx=xy.size[1],ny=xy.size[2],cov.pars=c(std.dev,range),mean=cost.lim,messages=F)
        rfield$data[rfield$data<1]<-1
      }else{
        rfield <- geoR::grf(nsim=1,grid="reg",nx=xy.size[1],ny=xy.size[2],cov.pars=c(std.dev,runif(1,range[1],range[2])),mean=cost.lim,messages=F)
        rfield$data[rfield$data<1]<-1
      }
      mat <- matrix(rfield$data,nrow=xy.size[1],ncol=xy.size[2])
      mat <- t(mat)
      mat <- mat[nrow(mat):1,]
      rast <- raster::raster(mat,xmn=0,ymn=0,xmx=xy.size[1],ymx=xy.size[2])
    }else{
      rast <- raster::raster(extent(0,xy.size[1],0,xy.size[2]),res=1,
                             vals=runif(as.integer(prod(xy.size)),1,cost.lim))
    }
    ## prep roads
    if (length(n.roads)>1){
      road.num <- sample(n.roads[1]:n.roads[2],1)
    }else{
      road.num <- n.roads
    }
    lineList <- list()
    for (r in 1:road.num){
      startside <- sample(1:4,1)
      if (startside%in%c(1,2)){endside<-c(1,2)[c(1,2)!=startside]
      }else if(startside%in%c(3,4)){endside<-c(3,4)[c(3,4)!=startside]}
      cellsToConnect <- c()
      for(side in c(startside,endside)){
        if (side==1){cells<-raster::cellFromRow(rast,1)
        }else if (side==2){cells<-raster::cellFromRow(rast,dim(rast)[1])
        }else if (side==3){cells<-raster::cellFromCol(rast,1)
        }else if (side==4){cells<-raster::cellFromCol(rast,dim(rast)[2])}
        cellsToConnect <- c(cellsToConnect,sample(cells[2:(length(cells)-1)],1))
      }
      lineList[[length(lineList)+1]]<-sp::Lines(list(sp::Line(xyFromCell(rast,cellsToConnect))),ID=r)
    }
    roadlines <- sp::SpatialLines(lineList)
    roadcells <- as.data.frame(do.call(rbind,raster::extract(rast,roadlines,cellnumbers=T)))$cell
    rast_withroads <- rast
    rast_withroads[roadcells] <- 0
    ## landings
    land_stack <- raster::stack()
    toSample <- rast_withroads
    toSample[toSample==0] <- NA
    land_rast <- rast_withroads==0
    for (li in 1:n.landing.sets){
      land_rast[] <- FALSE
      if (length(n.landings>1)){
        n.land.samples<-sample(n.landings[1]:n.landings[2],1)
      }else{
        n.land.samples<-n.landings
      }
      land <- raster::sampleRandom(toSample,size=n.land.samples,na.rm=T,sp=T)
      land$set <- li
      land$ID  <- 1:length(land)
      if (li==1){
        landings <- land
      }else{
        landings <- rbind(landings,land)
      }
      toSample[raster::cellFromXY(toSample,land@coords)] <- NA
      land_rast[raster::cellFromXY(toSample,land@coords)] <- TRUE
      land_stack <- raster::addLayer(land_stack,land_rast)
    }
    # polygonal landings
    if (all(!is.na(n.poly.landings))){
      if (all(n.poly.landings>0)){
        rast_valid <- rast_withroads
        continueAdding <- T
        n.try <- 100
        for (li in 1:sample(n.poly.landings[1]:n.poly.landings[2],1)){
          for (trynum in 1:n.try){
            poly.size <- c(sample(poly.landings.xdim[1]:poly.landings.xdim[2],1),sample(poly.landings.ydim[1]:poly.landings.ydim[2],1))
            topleft <- raster::sampleRandom(raster::raster(res=c(1,1),xmn=2,xmx=xy.size[1]-poly.size[1]-2,ymn=poly.size[2]+2,ymx=xy.size[2]-2,vals=0),size=1,xy=T)
            tlbr <- rbind(topleft[,1:2],topleft[,1:2]+c(poly.size[1],-1*poly.size[2])) # top left and bottom right
            tlbr.buff <- rbind(tlbr[1,]+c(-2,2),tlbr[2,]-c(-2,2))
            poly.buff <- sp::Polygon(coords=rbind(tlbr.buff[1,],cbind(tlbr.buff[1,1],tlbr.buff[2,2]),tlbr.buff[2,],cbind(tlbr.buff[2,1],tlbr.buff[1,2])),hole=F)
            poly.buff <- sp::SpatialPolygons(list(sp::Polygons(list(poly.buff),li)))
            if(all(unlist(raster::extract(rast_valid,poly.buff))>0)){
              poly <- sp::Polygon(coords=rbind(tlbr[1,],cbind(tlbr[1,1],tlbr[2,2]),tlbr[2,],cbind(tlbr[2,1],tlbr[1,2])),hole=F)
              poly <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(list(sp::Polygons(list(poly),li))),data=data.frame(ID=li,row.names=c(li)))
              rast_valid[raster::extract(rast_valid,poly,cellnumber=T)[[1]][,1]]<-0
              break
            }
            if (trynum==n.try){
              warning("Could not find valid spot for polygon ",li," in scenario ",i," with ",n.try," tries")
              continueAdding <- F
            }
          }
          if (!continueAdding){break}
          if (li==1){
            polydf <- poly
          }else{
            polydf <- rbind(polydf,poly)
          }
        }
      }
    }
    scen$road.rast  <- rast_withroads==0
    scen$road.line  <- roadlines
    scen$cost.rast  <- rast_withroads
    scen$landings.points <- landings
    scen$landings.stack  <- land_stack
    scen$landings.poly <- polydf
    outlist[[i]] <- scen
  }
  return(outlist)
} # end of prepInputScenarios function
#################################################
# generate 10 roads::projectRoads input scenarios
demoScen <- prepInputScenarios(n.scenarios=10,seed.value=1)
save(demoScen,file="data/demoScen.rda")
#################################################
# to generate plots of the demo scenarios, set outputFolder (below), and uncomment and run the following three lines of code
# outputFolder <- "c:/myWorkingFolder/folderForScenarioPlots"
# if (!grepl("/$|\\$",outputFolder)){outputFolder <- paste0(outputFolder,"/")}
# z<-lapply(demoScen,function(scen){plot.scenario(outPath=paste0(outputFolder,sprintf("%02d",scen$scen.number),".png"),scen$cost.rast,scen$landings.points,scen$landings.poly)})
#################################################


