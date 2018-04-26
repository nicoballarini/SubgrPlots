# require(sp)
# require(spatstat)

circleSP <- function (x0, y0, r,  npts = 100,  ...) {
  ## Return SpatialPolygons (single) circle

  theta <- seq(0, 2 * pi, length = npts)
  xh <-  r * cos(theta)
  yh <-  r * sin(theta)
  x  <- x0 + xh
  x[npts]<-x[1]	## force closure of polygon
  y  <- y0 + yh
  y[npts]<-y[1]	## force closure of polygon
  return(sp::SpatialPolygons(list(sp::Polygons(list(with(list(x = x, y = y),sp::Polygon(cbind(x,y)))),1))) )
}
