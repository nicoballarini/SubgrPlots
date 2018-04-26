library(SubgrPlots)
library(survival)
data(prca)
prca

cox.result = coxph(Surv(survtime,cens) ~ rx, data = prca)
summary(cox.result)

prca$age
unit. = 1
x.lim = range(prca$age)
grid.pts = seq(x.lim[1], x.lim[2], unit.)

center.x = grid.pts[1]
dist = abs(prca$age - center.x)
maxdist = max(dist)
weight. = (1 - (dist/maxdist)^3)^3
which(!(weight.>0))
cox.result = coxph(Surv(survtime,cens) ~ rx, data = prca, weights = weight., subset = (weight.>0))
summary(cox.result)
data.frame(x = center.x, estimate = cox.result$coefficients)



# results = do.call(rbind,lapply(grid.pts,FUN = function(i){
#   center.x = i
#   dist = abs(prca$age - center.x)
#   maxdist = max(dist)
#   weight. = (1 - (dist/maxdist)^3)^3
#   which(!(weight.>0))
#   cox.result = coxph(Surv(survtime,cens) ~ rx, data = prca, weights = weight., subset = (weight.>0))
#   summary(cox.result)
#   data.frame(x = center.x, estimate = cox.result$coefficients)
# }))
# results
# plot(results$x, results$estimate)
#
#
# # Now do a bivariate local regression ####
# prca$age
# prca$weight
# unit.x = 1
# unit.y = 1
# x.lim = range(prca$age)
# y.lim = range(prca$weight)
# grid.pts.x = seq(x.lim[1], x.lim[2], unit.x)
# grid.pts.y = seq(y.lim[1], y.lim[2], unit.y)
# grid.xy = expand.grid(grid.pts.x, grid.pts.y)
#
#
# center.x = grid.xy[1, 1]
# center.y = grid.xy[1, 2]
# cat(center.x, center.y)
# dist = sqrt((prca$age - center.x)^2 + (prca$weight - center.y)^2)
# maxdist = max(dist)
# weight. = (1 - (dist/maxdist)^3)^3
# which(!(weight.>0))
# cox.result = coxph(Surv(survtime,cens) ~ rx, data = prca, weights = weight., subset = (weight.>0))
# summary(cox.result)
# data.frame(x = center.x, y = center.y, estimate = cox.result$coefficients)
#
# results.xy = do.call(rbind, lapply(1:nrow(grid.xy), FUN = function(i){
#   center.x = grid.xy[i, 1]
#   center.y = grid.xy[i, 2]
#   cat(center.x, center.y)
#   dist = sqrt((prca$age - center.x)^2 + (prca$weight - center.y)^2)
#   maxdist = max(dist)
#   weight. = (1 - (dist/maxdist)^3)^3
#   which(!(weight.>0))
#   cox.result = coxph(Surv(survtime,cens) ~ rx, data = prca, weights = weight., subset = (weight.>0))
#   summary(cox.result)
#   data.frame(x = center.x, y = center.y, estimate = cox.result$coefficients)
# }))
# z.matrix = matrix(results.xy$estimate,
#        nrow = length(grid.pts.x),
#        ncol = length(grid.pts.y), byrow = F)
# rownames(z.matrix) = grid.pts.x
# colnames(z.matrix) = grid.pts.y
# z.matrix
# z.matrix[1:4, 1:4]
# head(results.xy)
# contour(grid.pts.x, grid.pts.y, z.matrix)
# contour(grid.pts.x, grid.pts.y, z.matrix)
# filled.contour(grid.pts.x, grid.pts.y, z.matrix, zlim = c(-1,1))
# filled.contour(grid.pts.x, grid.pts.y, z.matrix)
#
#
#
# contourplt_new
# prca$trt = as.numeric(prca$rx)
# model = loess(formula = survtime ~ trt * (age + weight), data = prca)
# x.lim = range(prca$age)
# y.lim = range(prca$weight)
# grid.pts.x = seq(x.lim[1], x.lim[2], unit.x)
# grid.pts.y = seq(y.lim[1], y.lim[2], unit.y)
# grid.xy = expand.grid(grid.pts.x, grid.pts.y)
# names(grid.xy) = c("age", "weight")
# model$pars
# treatment.0 = predict(model, newdata = data.frame(trt = 0, grid.xy))
# treatment.1 = predict(model, newdata = data.frame(trt = 1, grid.xy))
# treatment.1 = predict(model, newdata = data.frame(grid.xy))
#
#
#
# i=1
# alpha = 0.2

prca$age
prca$weight
unit.x = 1
unit.y = 1
x.lim = range(prca$age)
y.lim = range(prca$weight)
grid.pts.x = seq(x.lim[1], x.lim[2], unit.x)
grid.pts.y = seq(y.lim[1], y.lim[2], unit.y)
grid.xy = expand.grid(grid.pts.x, grid.pts.y)
alpha = 0.8
var = 1/(1-alpha)^2
sqrt(var)
i=1
n.cutoff = 20
# Now do a bivariate local regression ####
results.xy = do.call(rbind, lapply(1:nrow(grid.xy), FUN = function(i){
  center.x = grid.xy[i, 1]
  center.y = grid.xy[i, 2]
  # cat(center.x, center.y)
  var = 1/(1-alpha)^2
  x.conf.up = center.x + 2 * sqrt(var)
  weight.up = exp(-mahalanobis(x = data.frame(x.conf.up, center.y),
                               center = c(center.x, center.y),
                               cov    = matrix(c(var,0,0,var), nrow = 2))/2)
  weight. = exp(-mahalanobis(x = data.frame(prca$age, prca$weight),
                             center = c(center.x, center.y),
                             cov    = matrix(c(var,0,0,var), nrow = 2))/2)

  if(sum(weight.>weight.up)>n.cutoff){
    # weight.
    # which(!(weight.>0))
    sum((weight.>0.01))
    # cox.result = coxph(Surv(survtime,cens) ~ rx,
    #                    data = prca[which(weight.>weight.up), ],
    #                    weights = weight.[which(weight.>weight.up)])
    cox.result = coxph(Surv(survtime,cens) ~ rx,
                       data = prca,
                       weights = weight.)
    summary(cox.result)
    data.frame(x = center.x, y = center.y, estimate = cox.result$coefficients)
  }else{
    data.frame(x = center.x, y = center.y, estimate = NA)
  }
}))

warnings()
z.matrix = matrix(results.xy$estimate,
                  nrow = length(grid.pts.x),
                  ncol = length(grid.pts.y), byrow = F)
rownames(z.matrix) = grid.pts.x
colnames(z.matrix) = grid.pts.y
z.matrix
z.matrix[1:4, 1:4]
head(results.xy)
filled.contour(grid.pts.x, grid.pts.y, z.matrix, zlim = c(-3,3))
filled.contour(grid.pts.x, grid.pts.y, z.matrix)




####### Create a good plot #####################################################
# dev.off()
pdf(file = "contour-local-reg.pdf", width = 6, height = 5)
font.size = c(1, 1, 0.7, 0.7, 0.75)
sub.title = NULL
title = main.title = NULL
# lab.vars = names(dat)[covari.sel]
lab.vars = c("age", "weight")
#
# cols = c('#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd')
# pal.YlRd = colorRampPalette(c("#fee090", "#d73027"),  space = "rgb")
# pal.WhBl = colorRampPalette(c("#e0f3f8", "#4575b4"),  space = "rgb")
# breaks = seq(min(range.strip) - 1e-8, max(range.strip) + 1e-8, length.out = length(brk.es)+1)

brk.es = seq(-3,3,length.out = 31)
n.brk.axis =  7
# col.vec.div.pos = pal.WhBl((length(brk.es)-1)/2)
# col.vec.div.neg = pal.YlRd((length(brk.es)-1)/2)
# col.vec = c(rev(col.vec.div.neg), col.vec.div.pos)
col.power = 0.75
col.vec = rev(colorspace::diverge_hcl(n = length(brk.es)-1,
                                      # h = c(218, 0),
                                      c = 100, l = c(50,90),
                                      power = col.power))
if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
cols = col.vec


outcome.type = "survival"
effect = "HR"
if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
cols = col.vec
layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(4,1))
par(mar=c(5,4,4,2))
plot(grid.xy$age, grid.xy$weight, type = "n",
     yaxs="i",xaxs="i",
     xlim = range(grid.pts.x),
     ylim = range(grid.pts.y),
     xlab = lab.vars[1], ylab = lab.vars[2],
     main = title, #sub = subtitle,
     col  = "gray80",
     cex.main = font.size[1],
     cex.lab  = font.size[2],
     cex.axis = font.size[2],
     cex.sub  = font.size[3])
breaks = pretty(c(-3,3), length(cols))
breaks.axis = pretty(c(-3,3), n.brk.axis)
.filled.contour(grid.pts.x, grid.pts.y, z.matrix,
                levels = breaks,
                col = rev(cols))
points(prca$age, prca$weight, cex = 0.5)
# box()
par(mar=c(5,2, 4, 2))
SubgrPlots:::image.scale(brk.es,
                         col= rev(cols),
                         breaks = breaks,
                         axis.pos = 4, add.axis = FALSE)
axis(2, at = breaks.axis, labels = round(breaks.axis, 3), las = 0, cex.axis = font.size[5])
dev.off()








circleFun <- function(center = c(0,0), r = 1, npoints = 100){
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# and make a circles
circle1 <- circleFun(c(10,10),20,npoints = 100)

i=1



## Create GIF ------------------------------------------------------------------
dev.off()
names(grid.xy) = c("age", "weight")
png(file="gif/contour_reg/example%04d.png", width=500, height=500)
                     for (i in seq(1,nrow(grid.xy), 10)){
                     # lapply(seq(1,nrow(grid.xy), 20), FUN = function(i){
  #results.xy = do.call(rbind, lapply(1:10, FUN = function(i){
  center.x = grid.xy[i, 1]
  center.y = grid.xy[i, 2]
  # cat(center.x, center.y)
  var = 1/(1-alpha)^2
  x.conf.up = center.x + 2 * sqrt(var)
  weight.up = exp(-mahalanobis(x = data.frame(x.conf.up, center.y),
                               center = c(center.x, center.y),
                               cov    = matrix(c(var,0,0,var), nrow = 2))/2)
  weight. = exp(-mahalanobis(x = data.frame(prca$age, prca$weight),
                             center = c(center.x, center.y),
                             cov    = matrix(c(var,0,0,var), nrow = 2))/2)
  plot(grid.xy$age, grid.xy$weight, type = "n",
       yaxs="i",xaxs="i",
       xlim = range(grid.pts.x),
       ylim = range(grid.pts.y),
       xlab = lab.vars[1], ylab = lab.vars[2],
       main = title, #sub = subtitle,
       col  = "gray80",
       cex.main = font.size[1],
       cex.lab  = font.size[2],
       cex.axis = font.size[2],
       cex.sub  = font.size[3])
  breaks = pretty(c(-3,3), length(cols))
  breaks.axis = pretty(c(-3,3), n.brk.axis)
  .filled.contour(grid.pts.x, grid.pts.y, z.matrix,
                  levels = breaks,
                  col = rev(cols))
  points(prca$age, prca$weight)
  points(center.x, center.y, pch = 19, col = "red")
  points(prca$age[weight.>weight.up],
         prca$weight[weight.>weight.up],
         col = "red")
  circle1 <- circleFun(c(center.x, center.y), x.conf.up - center.x, npoints = 100)
  lines(circle1$x, circle1$y, col = "red")
  if(sum(weight.>weight.up)>n.cutoff){
    # weight.
    # which(!(weight.>0))
    sum((weight.>0.01))
    # cox.result = coxph(Surv(survtime,cens) ~ rx,
    #                    data = prca[which(weight.>weight.up), ],
    #                    weights = weight.[which(weight.>weight.up)])
    cox.result = coxph(Surv(survtime,cens) ~ rx,
                       data = prca,
                       weights = weight.)
    summary(cox.result)
    data.frame(x = center.x, y = center.y, estimate = cox.result$coefficients)
  }else{
    data.frame(x = center.x, y = center.y, estimate = NA)
  }
}
dev.off()
# convert the .png files to one .gif file using ImageMagick.
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.

#setwd("/Users/nicoballarini/Desktop/SubgrPlots 2018 03 18/gif/")
# system("convert -delay 5 *.png example_1.gif")

# to not leave the directory with the single jpeg files
# I remove them.
# file.remove(list.files(pattern=".png"))
