
text.pos<- function (x0, y0, r, angle,...) {  
     
  xh <-  r * cos(angle)       
  yh <-  r * sin(angle)        
  x.adj  <- x0 + xh 
  y.adj  <- y0 + yh  
  return(c(x.adj, y.adj))
}   
