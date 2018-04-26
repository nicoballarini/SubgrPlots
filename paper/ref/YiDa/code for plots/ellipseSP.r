ellipseSP <- function (x0, y0, a, b, phi=pi/2, npts = 100, ...) {

  ## Returns SpatialPolygons ellipse
  ## phi defaults to 90 degrees

  ## whereby a lies in the N-S and

  ## b in the E-W directions
  theta <- seq(0, 2 * pi, length = npts)
  xh <-  a * cos(theta)
  yh <-  b * sin(theta)
  co <- cos(phi)
  si <- sin(phi)
  x  <- x0 + co*xh - si*yh
  x[npts]<-x[1]	## force closure of polygon
  y  <- y0 + si*xh + co*yh
  y[npts]<-y[1]	## force closure of polygon
  return(sp::SpatialPolygons(list(sp::Polygons(list(with(list(x = x, y = y),
                                                         sp::Polygon(cbind(x,y)))), 1))) )
}
