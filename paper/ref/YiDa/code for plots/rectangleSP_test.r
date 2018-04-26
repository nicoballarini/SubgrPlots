# require(sp)
# require(spatstat)
rectangleSP_test <- function (x0, y0, w, h) {

  ## Return SpatialPolygons (single) rectangle

 # d.y.adj = h/(x.range) * y.range        #(x.max + abs(-x.max)) * y.max
 # d.y.adj = d.y.adj/k   # when k = 2, divided by 2 due to the ratio 1:1 in width for the
 #                       # layout; when k = 3,  divided by 3 due to the ratio 1:1:1 in width for the layout

  x = c(x0 + w/2, x0 - w/2, x0 - w/2, x0 + w/2)
  #y = c(y0 + d.y.adj/2, y0 + d.y.adj/2, y0 - d.y.adj/2, y0 - d.y.adj/2)
  y = c(y0 + h/2, y0 + h/2, y0 - h/2, y0 - h/2)

  x = c(x, x[1]) ## force closure of polygon
  y = c(y, y[1]) ## force closure of polygon

  return(sp::SpatialPolygons(list(sp::Polygons(list(with(list(x = x, y = y),
                                                         sp::Polygon(cbind(x,y)))),1))) )
}
