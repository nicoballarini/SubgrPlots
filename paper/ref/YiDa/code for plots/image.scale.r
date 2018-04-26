#############################################################################################
#                                                                                           #
# image.scale:   This function creates a color scale for use with the image()               #
#                function. Input parameters should be consistent with those                 #
#                used in the corresponding image plot. The "axis.pos" argument              #
#                defines the side of the axis. The "add.axis" argument defines              #
#                whether the axis is added (default: TRUE)or not (FALSE).                   #
#                                                                                           #
#                                                                                           #
# revised by Yi-Da Chiu 31/08/2017                                                          #
#                                                                                           #
#############################################################################################

image.scale <- function(z, zlim, col = grDevices::heat.colors(12),
                        breaks, axis.pos=1, add.axis=TRUE, add.box = FALSE, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  if(axis.pos %in% c(1,3)){ylim<-c(0,1); xlim<-range(breaks)}
  if(axis.pos %in% c(2,4)){ylim<-range(breaks); xlim<-c(0,1)}
  graphics::plot(1,1,t="n",ylim=ylim, xlim=xlim, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i", ...)
  for(i in seq(poly)){
    if(axis.pos %in% c(1,3)){
      graphics::polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(axis.pos %in% c(2,4)){
      graphics::polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
  if(add.box) graphics::box()
  if(add.axis) {graphics::axis(axis.pos)}
}
