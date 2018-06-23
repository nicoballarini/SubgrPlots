rectangleSP_test <- function (x0, y0, w, h) {
## Return SpatialPolygons (single) rectangle
  x = c(x0 + w/2, x0 - w/2, x0 - w/2, x0 + w/2)
  y = c(y0 + h/2, y0 + h/2, y0 - h/2, y0 - h/2)
  x = c(x, x[1]) ## force closure of polygon
  y = c(y, y[1]) ## force closure of polygon
  return(sp::SpatialPolygons(list(sp::Polygons(list(with(list(x = x, y = y),
                                                         sp::Polygon(cbind(x,y)))),1))) )
}
