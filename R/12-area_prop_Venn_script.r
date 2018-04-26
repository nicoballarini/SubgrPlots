##################################################################################################################################################
#                                                                                                                                                #
#                                                    Area-proportional Venn diagram for subgroup effect sizes                                    #
#                                                                                                                                                #
#  This script only creates two-set or three-set area-proportional Venn diagram. The sample size of the sets should be descreasing otherwise     #
#  it produces a wronge display. Note that the labels of subgroup sample sizes have to be placed in an appropriate position manually. So far, this#
#  works for a continuous response only.                                                                                                         #
#                                                                                                                                                #
#  created by Yi-Da Chiu 22/07/17                                                                                                                #
#                                                                                                                                                #
##################################################################################################################################################
#' Venn diagram for subgroup effect size
#'
#' This function produces a Venn diagram showing the treatment effect size of subgroups defined by sets from the categories of covariates.
#' Also, it prints out the minimum and maximum of the treatment effect size on the console so as to set an approapriate range for effect
#' size on the colour strip . Note that there are two options of graphical display; whether show the subgroup effect size of the complement
#' of the union of all the considered subgroups or not. In addition, this function only works up to 5 sets and does not run an area-proportional
#' algorithms for displaying two or three set. In addition, the function uses log odd ratio and log hazard ratio for displaying
#' subgroup effect sizes in binary and survival data, respectively.
#'
#'@param dat:          a data set
#'@param covari.sel:   a vector of indices of covariates
#'@param cat.sel:      a vector of indices of the categories for each covariate
#'@param trt.sel:      a covariate index specifying the treatment code
#'@param resp.sel:     a covariate index specifying the response variable
#'@param outcome.type: a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#'@param range.strip:  a vector with two elements specifying the range of treatment effect size for display
#'@param n.brk:        a number specifying the number of the points dividing the range of the argument "range.strip".
#'@param font.size:    a vector specifying the size of labels and text; the first element is for the main title; the second is for the category labels;
#'               the third is for the sample size labels; the fourth is for the legend text; the fifth is for the y-axis label of the colour strip;
#'               the sixth is for the unit label on the y axis.
#'@param title:        a string specifying the main title.
#'@param strip:        a string specifying the title of the colour strip.
#
# eg.1          main.title = paste("Treatment effect sizes across subgroups (N = 1000)", sep = "");
#               strip.title = paste("Treatment effect size");
#               vd(dat = dat, covari.sel = c(4, 5, 10), cat.sel = c(1, 2, 2), trt.sel = 2, resp.sel = 1, outcome.type = "continuous",
#               title = main.title, strip = strip.title)
#
# eg.2          main.title = paste("Treatment effect sizes across subgroups (N = 2985)", sep = "");
#               strip.title = paste("Treatment effect size (log odd ratio)");
#               vd(dat = dat2, covari.sel = c(2, 2, 3), cat.sel = c(1, 2, 1), trt.sel = 4, resp.sel = 5, outcome.type = "binary",
#               title = main.title, strip = strip.title)
#
# eg.3          main.title = paste("Treatment effect sizes across subgroups (N = 686)", sep = "");
#               strip.title = paste("Treatment effect size (log hazard ratio)");
#               vd(dat = dat3, covari.sel = c(6, 6, 7), cat.sel = c(1, 2, 1), trt.sel = 1, resp.sel = c(4,3), outcome.type = "survival",
#               title = main.title, strip = strip.title)
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
plot_venn_proportional <- function(dat, covari.sel, cat.sel, trt.sel, resp.sel,
                                   outcome.type, outside.area=FALSE,
                                   range.strip=c(-6, 6), n.brk=13,n.brk.axis=NULL,
                                   font.size = c(1, 1.5, 1, 0.9, 1, 1),
                                   title = NULL, strip = NULL,
                                   fill = FALSE, fill.background = FALSE,
                                   effect = "HR", show.overall = TRUE,
                                   palette = "divergent", col.power = 0.5){
####################################################### 1. create subgroup data  #################################################################
data.size = dim(dat)[1]
# covari.sel = c(6, 10, 4)    #  the covariates we select
# cat.sel = c(2, 2, 3)        #  the category indices in the above covariates
n.subgrp = length(covari.sel)
names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
if (outcome.type == "continuous"){
  names(dat)[resp.sel] = "resp"                        # rename the response variable
}else if (outcome.type == "binary"){
  names(dat)[resp.sel] = "resp"                        # rename the response variable
}else if (outcome.type == "survival"){
  names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
  names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
}




# Calculate overall Treatment effect ### TODO Look for confidence intervals ----------
if (outcome.type == "continuous"){
  model.int = lm(resp ~ trt,  data = dat)
  model.sum = summary(model.int)
  overall.treatment.mean = model.sum$coefficients[2, 1]
  overall.treatment.upper = 0
  overall.treatment.lower = 0
}else if (outcome.type == "binary"){
  model.int = glm(resp ~ trt, family = "binomial", data = dat)
  model.sum = summary(model.int)
  overall.treatment.mean = model.sum$coefficients[2, 1]
  overall.treatment.upper = 0
  overall.treatment.lower = 0
}else if (outcome.type == "survival"){
  if (effect == "HR"){
    model.int = coxph(Surv(time, status) ~ trt, data = dat)
    model.sum = summary(model.int)
    overall.treatment.mean = model.sum$coef[1, 1]
    overall.treatment.upper = log(model.sum$conf.int[1, 4])
    overall.treatment.lower = log(model.sum$conf.int[1, 3])
  }
  if (effect == "RMST"){
    dat.subgr.i = dat
    rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                          arm = dat.subgr.i$trt, tau = time)
    overall.treatment.mean = rmst$unadjusted.result[1,1]
    overall.treatment.upper = 0
    overall.treatment.lower = 0
  }
}


cats.var.all = list()
for (i in 1 : length(covari.sel)){
  cats.var.all[[i]] = names(table(dat[, covari.sel[i]]))
}
rearrangement.idx = order(sapply(1:n.subgrp, function(x) length(which((dat[, covari.sel[x]] == cats.var.all[[x]][cat.sel[x]])  == T))), decreasing = T)
covari.sel = covari.sel[rearrangement.idx]    #  the rearrangement of the covariates (so that the sample sizes of the rearranged set is decreasing)
cat.sel = cat.sel[rearrangement.idx]          #  the category indices in the above rearranged covariates
cats.var.all = list()
for (i in 1 : length(covari.sel)){
  cats.var.all[[i]] = names(table(dat[, covari.sel[i]]))
}
lab.vars = names(dat)[covari.sel]



if (n.subgrp == 2){

  A = dat[, covari.sel[1]] == cats.var.all[[1]][cat.sel[1]]
  B = dat[, covari.sel[2]] == cats.var.all[[2]][cat.sel[2]]

  cond = list()
  cond[[1]] = which( A & !B  == T  );   n.1 = length(which( A & !B  == T  ))
  cond[[2]] = which( !A & B  == T  );   n.2 = length(which( !A & B  == T  ))
  cond[[3]] = which( A & B == T  );     n.12 = length(which( A & B  == T  ))
  cond[[4]] = which( !A & !B == T  );   n.compl = length(which(!A & !B == T  ))

  data.subgrp = list()
  n.subgrp = length(covari.sel)
  n.subgrp.tol = sum(sapply(0:n.subgrp, function(x) choose(n.subgrp, x)))
  for (i in 1 : n.subgrp.tol ){
    data.subgrp[[i]] =  dat[cond[[i]], ]
  }

  # create matrices for treatment effect size and standard error of MLE

  # treatment.mean = matrix(rep(0, n.subgrp.tol), nrow = n.subgrp.tol, ncol = 1)
  # for (i in 1 : n.subgrp.tol ){
  #
  #   cond1 = sum(data.subgrp[[i]]$trt == "0") == 0
  #   cond2 = sum(data.subgrp[[i]]$trt == "1") == 0
  #
  #   if (cond1 | cond2 ){
  #     treatment.mean[i] = NA
  #   }else{
  #     model.int =  lm(resp ~ trt,  data = data.subgrp[[i]])
  #     model.sum = summary(model.int)
  #     treatment.mean[i] = model.sum$coefficients[2, 1]
  #   }
  # }
  ## search for the between-cetre distances and angles

  w.A = length(which( A  == T  ))/data.size #802/1000   #  length(which( A  == T  ))
  w.B = length(which( B  == T  ))/data.size #740/1000   #  length(which( B  == T  ))
  w.AB = length(which( A & B  == T  ))/data.size #603/1000  #  length(which( A & B  == T  ))

  r1 = sqrt(w.A/pi)  # the radius of the first circle
  r2 = sqrt(w.B/pi)  # the radius of the second circle

  ##
  dummy = FALSE
  d.AB = r1-r2     # the distance of the two circle centres, the intial value is set to the difference between the two radiuses
  for (i in 1 : 100000 ){
    if(dummy == TRUE) break
    d.AB = d.AB + 1/100000
    alpha = 2 * acos((d.AB^2 + r1^2 - r2^2)/(2*r1*d.AB))
    beta = 2 * acos((d.AB^2 + r2^2 - r1^2)/(2*r2*d.AB))
    area.AB = 1/2*r1^2 * (alpha - sin(alpha)) + 1/2*r2^2 * (beta - sin(beta))
    if ((area.AB < (w.AB + 0.00001)) & (area.AB > (w.AB - 0.00001)) | (d.AB >=  r1+ r2) )
    {dummy = TRUE
    # print(d.AB)
    }
  }

}else if (n.subgrp == 3){

  A = dat[, covari.sel[1]] == cats.var.all[[1]][cat.sel[1]]
  B = dat[, covari.sel[2]] == cats.var.all[[2]][cat.sel[2]]
  C = dat[, covari.sel[3]] == cats.var.all[[3]][cat.sel[3]]

  cond = list()
  cond[[1]] = which( A & !B & !C == T  );   n.1 = length(which( A & !B & !C == T  ))
  cond[[2]] = which( !A & B & !C == T  );   n.2 = length(which( !A & B & !C == T  ))
  cond[[3]] = which( !A & !B & C == T  );   n.3 = length(which( !A & !B & C == T  ))
  cond[[4]] = which( A & B & !C == T  );    n.12 = length(which( A & B & !C == T  ))
  cond[[5]] = which( A & !B & C == T  );    n.13 = length(which( A & !B & C == T  ))
  cond[[6]] = which( !A & B & C == T  );    n.23 = length(which(!A & B & C == T  ))
  cond[[7]] = which( A & B & C == T  );     n.123 = length(which(A & B & C == T  ))
  cond[[8]] = which( !A & !B & !C == T  );  n.compl = length(which(!A & !B & !C == T  ))
  # test
  # n.1 + n.2 + n.3 + n.12 + n.13 + n.23 + n.123 + n.compl
  # nrow(dat)

  data.subgrp = list()
  n.subgrp.tol = sum(sapply(0:n.subgrp, function(x) choose(n.subgrp, x)))
  for (i in 1 : n.subgrp.tol ){
    data.subgrp[[i]] =  dat[cond[[i]], ]
  }

  # create matrices for treatment effect size and standard error of MLE

  # treatment.mean = matrix(rep(0, n.subgrp.tol), nrow = n.subgrp.tol, ncol = 1)
  # for (i in 1 : n.subgrp.tol ){
  #
  #   cond1 = sum(data.subgrp[[i]]$trt == "0") == 0
  #   cond2 = sum(data.subgrp[[i]]$trt == "1") == 0
  #
  #   if (cond1 | cond2 ){
  #     treatment.mean[i] = NA
  #   }else{
  #     model.int =  lm(resp ~ trt,  data = data.subgrp[[i]])
  #     model.sum = summary(model.int)
  #     treatment.mean[i] = model.sum$coefficients[2, 1]
  #   }
  # }
  ## search for the between-cetre distances and angles

  w.A = length(which(A == T))/data.size #802/1000   #  length(which( A  == T  ))
  w.B = length(which(B == T))/data.size #740/1000   #  length(which( B  == T  ))
  w.C = length(which(C == T))/data.size #491/1000   #  length(which( C  == T  ))

  w.AB = length(which(A & B == T))/data.size #603/1000  #  length(which( A & B  == T  ))
  w.AC = length(which(C & A == T))/data.size #409/1000  #  length(which( C & A  == T  ))
  w.BC = length(which(B & C == T))/data.size #387/1000  #  length(which( B & C  == T  ))
  w.ABC = length(which(C & B & A == T))/data.size #329/1000 #  length(which( C & B & A  == T  ))

  r1 = sqrt(w.A/pi) # the radius of the first circle
  r2 = sqrt(w.B/pi) # the radius of the second circle
  r3 = sqrt(w.C/pi) # the radius of the third circle

  ##
  dummy = FALSE
  d.AB = r1-r2  # the distance between the centres of the 1st and 2nd circles, the intial value is set to the difference between the two radiuses
  for (i in 1 : 100000 ){
    if(dummy == TRUE) break
    d.AB = d.AB + 1/100000
    alpha = 2 * acos((d.AB^2 + r1^2 - r2^2)/(2*r1*d.AB))
    beta = 2 * acos((d.AB^2 + r2^2 - r1^2)/(2*r2*d.AB))
    area.AB = 1/2*r1^2 * (alpha - sin(alpha)) + 1/2*r2^2 * (beta - sin(beta))
    if ((area.AB < (w.AB + 0.00001)) & (area.AB > (w.AB - 0.00001)) | (d.AB >=  r1+ r2) )
    {dummy = TRUE
    # print(d.AB)
    }
  }

  ##

  dummy = FALSE
  d.AC = r1-r3  # the distance between the centres of the 1st and 3rd circles, the intial value is set to the difference between the two radiuses
  for (i in 1 : 100000 ){
    if(dummy == TRUE) break
    d.AC = d.AC + 1/100000
    alpha = 2 * acos((d.AC^2 + r1^2 - r3^2)/(2*r1*d.AC))
    beta = 2 * acos((d.AC^2 + r3^2 - r1^2)/(2*r3*d.AC))
    area.AC = 1/2*r1^2 * (alpha - sin(alpha)) + 1/2*r3^2 * (beta - sin(beta))
    if ((area.AC < (w.AC + 0.00001)) & (area.AC > (w.AC - 0.00001)) | (d.AC >=  r1+ r3) ) {
      dummy = TRUE
      # print(d.AC)
      }
  }

  ##

  dummy = FALSE
  d.BC = r2-r3   # the distance between the centres of the 2nd and 3rd circles, the intial value is set to the difference between the two radiuses
  for (i in 1 : 100000 ){
    if(dummy == TRUE) break
    d.BC = d.BC + 1/100000
    alpha = 2 * acos((d.BC^2 + r2^2 - r3^2)/(2*r2*d.BC))
    beta = 2 * acos((d.BC^2 + r3^2 - r2^2)/(2*r3*d.BC))
    area.BC = 1/2*r2^2 * (alpha - sin(alpha)) + 1/2*r3^2 * (beta - sin(beta))
    if ((area.BC < (w.BC + 0.00001)) & (area.BC > (w.BC - 0.00001)) | (d.BC >=  r2+ r3) )
    {dummy = TRUE
    # print(d.BC)
    }
  }

  ##
  # calculate the angle between the two lines where the first connects the centres of the 1st and 2nd circles, and the second connects
  # the centres of the 1st and 3rd circles

  angle.ACAB = acos((d.BC^2 - d.AB^2 - d.AC^2) * (-1/(2*d.AB*d.AC)))
}


if (fill) {
  # create matrices for treatment size and standard error of MLE
  treatment.mean = vector()
  for (i in 1 : n.subgrp.tol ){
    cond1 = sum(data.subgrp[[i]]$trt == "0") == 0
    cond2 = sum(data.subgrp[[i]]$trt == "1") == 0

    if (cond1 | cond2 ){
      treatment.mean[i] = NA
    }else{

      if (outcome.type == "continuous"){
        model.int = lm(resp ~ trt,  data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "binary"){
        model.int = glm(resp ~ trt,  family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "survival"){
        model.int = coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
      }
    }
  }

  cat("The minimum of treatment effect sizes is", c(min(treatment.mean, na.rm = T)), "\n")
  cat("The maximum of treatment effect sizes is", c(max(treatment.mean, na.rm = T)), "\n")
} else {
  treatment.mean = vector()
}


################################################ 2. produce a graph  #################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sp, rgeos)
#library(sp)
#library(rgeos)

grid::grid.newpage()

# pal.2 = colorRampPalette(c("black", "red", "yellow"), space="rgb")
# pal.2 = colorRampPalette(c("#fc8d59", "#ffffbf", "#91bfdb"), space = "rgb")
breaks <- seq(min(range.strip) - 0.0000001, max(range.strip) + 0.0000001, length.out= n.brk)
breaks.axis <- seq(min(range.strip) - 0.0000001, max(range.strip) + 0.0000001, length.out= n.brk.axis)
levs=breaks
colors = numeric(n.subgrp.tol-1)
pal.YlRd = colorRampPalette(c("#fee090", "#d73027"),  space = "rgb")
pal.WhBl = colorRampPalette(c("#e0f3f8", "#4575b4"),  space = "rgb")

breaks = seq(min(range.strip) - 1e-8, max(range.strip) + 1e-8, length.out = n.brk)
col.vec.div.pos = pal.WhBl((length(breaks)-1)/2)
col.vec.div.neg = pal.YlRd((length(breaks)-1)/2)
col.vec = c(rev(col.vec.div.neg), col.vec.div.pos)
if (outcome.type == "survival" & effect == "HR") col.vec = rev(col.vec)


if (palette == "hcl"){
  col.vec = colorspace::diverge_hcl(n = length(breaks)-1,
                                    c = 100, l = c(50,90),
                                    power = col.power)
  if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
}

# dev.new(width=10,height=10,noRStudioGD = TRUE)
# dev.off()
# layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(4,1))

# par(mar=c(1,1,1,1))
# par(mar=c(0,0,0,0))

if (fill) layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(4,1))
par(mar=c(1,1,1,1))

if (n.subgrp == 2){

  circle.ox = c(0,  d.AB)
  circle.oy = c(0,     0)

  theta = seq(0,2*pi, length= 1000)
  circle.x = matrix(rep(0, n.subgrp * 1001), nrow = n.subgrp)
  circle.y = matrix(rep(0, n.subgrp * 1001), nrow = n.subgrp)
  picture = list()
  for (i in 1 : n.subgrp){
    if (i == 1){
      r = r1
    }else if(i == 2){
      r = r2
    }

    circle.x[i, ] = c(r*cos(theta)+ circle.ox[i], r*cos(theta[1000])+ circle.ox[i])
    circle.y[i, ] = c(r*sin(theta)+ circle.oy[i], r*sin(theta[1000])+ circle.oy[i])

    picture[[i]] =  SpatialPolygons(list(Polygons(list(Polygon(cbind(rev(circle.x[i, ]), rev(circle.y[i, ])))), ID = i)))
  }

  ind = 0
  for (i in 1 : (n.subgrp - 1)){
    for (j in (i + 1) : n.subgrp){
      ind = ind + 1
      k = ind + n.subgrp
      picture[[k]] = gIntersection( picture[[i]], picture[[j]] )
    }
  }
  picture[[n.subgrp.tol - 1]] <- gIntersection(picture[[n.subgrp.tol - 2]], picture[[1]] )

  main.title = paste("Treatment effect sizes across subgroups (N = 1000)", sep = "");
  plot(-1:1, -1:1, type='n', axes = FALSE, xlab = "", ylab = "", main = main.title)

  treat_size = c(treatment.mean)

  if (-1>9){   # draw the color for the effect size of the region: !A & !B & !C & !D & !E
    if (is.na(treat_size[n.subgrp.tol]) == TRUE){
      col.idx = n.subgrp.tol
      colors[n.subgrp.tol] = "white"
    }else{
      col.idx = which(treat_size[n.subgrp.tol] < breaks)
      col.idx = col.idx[1] - 1
      colors[n.subgrp.tol] = col.vec[col.idx]
    }
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = colors[n.subgrp.tol])
  }


  for (i in 1 : (n.subgrp.tol - 1)){
    if (is.na(treat_size[i]) == TRUE){
      colors[i] = "white"    }else{
      col.idx = which(treat_size[i] < breaks)
      col.idx = col.idx[1] - 1
      colors[i] = col.vec[col.idx]
    }
    plot(picture[[i]], add=TRUE, xlim=c(-1,1), ylim=c(-1,1), col=colors[i], lty = 2, lwd = 2, border = "lightblue")
  }

  text(circle.ox[1]-0.1, circle.oy[1] -0.1, labels= n.1, col = "green")
  text(circle.ox[1] +0.55, circle.oy[2], labels= n.2, col = "green")
  text(circle.ox[1]+ 0.2, circle.oy[1], labels= n.12, col = "green")

  text(circle.ox[1]-r1, circle.oy[1] -r1, labels=c("A"), col = "black", cex = 1.5)
  text(circle.ox[2]+r2, circle.oy[2] -r2,  labels=c("B"), col = "black", cex = 1.5)
  text(.9, .9, labels= n.compl, col = "green", cex = 1)
  box()

}else if (n.subgrp ==3){

  circle.ox = c(0,  d.AB,  d.AC * cos(angle.ACAB))
  circle.oy = c(0,     0,  d.AC * sin(angle.ACAB))

  theta = seq(0,2*pi, length= 1000)
  circle.x = matrix(rep(0, n.subgrp * 1001), nrow = n.subgrp)
  circle.y = matrix(rep(0, n.subgrp * 1001), nrow = n.subgrp)
  picture = list()
  for (i in 1 : n.subgrp){
    if (i == 1){
      r = r1
    }else if(i == 2){
      r = r2
    }else if(i == 3){
      r = r3
    }

    circle.x[i, ] = c(r*cos(theta)+ circle.ox[i], r*cos(theta[1000])+ circle.ox[i])
    circle.y[i, ] = c(r*sin(theta)+ circle.oy[i], r*sin(theta[1000])+ circle.oy[i])

    picture[[i]] =  SpatialPolygons(list(Polygons(list(Polygon(cbind(rev(circle.x[i, ]), rev(circle.y[i, ])))), ID = i)))
  }

  ind = 0
  for (i in 1 : (n.subgrp - 1)){
    for (j in (i + 1) : n.subgrp){
      ind = ind + 1
      k = ind + n.subgrp
      picture[[k]] = gIntersection( picture[[i]], picture[[j]] )
    }
  }
  picture[[n.subgrp.tol - 1]] <- gIntersection(picture[[n.subgrp.tol - 2]], picture[[1]] )

  # main.title = paste("Treatment effect sizes across subgroups (N = 1000)", sep = "");
  # plot(-1:1, -1:1, type='n', axes = FALSE, xlab = "", ylab = "", main = main.title)
  # plot(c(-0.5,1), c(-0.5,1), type='n', axes = FALSE, xlab = "", ylab = "", main = main.title)
  plot(c(-0.5,0.75), c(-0.5,0.75), type='n', axes = FALSE, xlab = "", ylab = "", main = title)

  treat_size = c(treatment.mean)

  if (fill.background){   # draw the color for the effect size of the region: !A & !B & !C & !D & !E
    if (is.na(treat_size[n.subgrp.tol]) == TRUE){
      col.idx = n.subgrp.tol
      colors[n.subgrp.tol] = "white"
    }else{
      col.idx = which(treat_size[n.subgrp.tol] < breaks)
      col.idx = col.idx[1] - 1
      colors[n.subgrp.tol] = col.vec[col.idx]
    }
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = colors[n.subgrp.tol])
  }


  for (i in 1 : (n.subgrp.tol - 1)){

    if (is.na(treat_size[i]) == TRUE){
      col.idx = i
      colors[i] = "white"
    }else{
      col.idx = which(treat_size[i] < breaks)
      col.idx = col.idx[1] - 1
      colors[i] = col.vec[col.idx]
    }
    # border.color = "lightblue"
    border.color = "black"
    plot(picture[[i]], add=TRUE, #xlim=c(-0.5,0.5), ylim=c(-0.5,1),
         col=colors[i],
         lty = 1, lwd = 2, border = border.color)
  }

  text.color = "black"

  text(circle.ox[1] - 0.1, circle.oy[1] -0.1,     labels = n.1,   col = text.color, cex = 1)
  text(circle.ox[1] + 0.55, circle.oy[2],         labels = n.2,   col = text.color, cex = 1)
  text(circle.ox[3] + 0.05, circle.oy[3] + 0.1,   labels = n.3,   col = text.color, cex = 1)
  text(circle.ox[1] + 0.27, circle.oy[3] - 0.25,  labels = n.12,  col = text.color, cex = 1)
  text(circle.ox[3] - 0.1,  circle.oy[3],         labels = n.13,  col = text.color, cex = 1)
  text(circle.ox[3] + 0.12, circle.oy[3] - 0.05,  labels = n.23,  col = text.color, cex = 1)
  text(circle.ox[1] + 0.27, circle.oy[1] + 0.1,   labels = n.123, col = text.color, cex = 1)

  text(circle.ox[1] - r1, circle.oy[1] - r1,   labels = lab.vars[1], col = "black", cex = 1)
  text(circle.ox[2] + r2, circle.oy[2] - r2,   labels = lab.vars[2], col = "black", cex = 1)
  text(circle.ox[3], circle.oy[3] + r3 + 0.15, labels = lab.vars[3], col = "black", cex = 1)
  # text(.9, .9, labels= n.compl, col = text.color, cex = 1)
  text(.6, .6, labels= n.compl, col = text.color, cex = 1)
  box()

}

  xy.current.pos = par("usr")

  if (fill){
    par(mar=c(1,2, 1, 2))
    SubgrPlots:::image.scale(treatmeant.mean, col=col.vec,
                             breaks = breaks-1e-8, axis.pos = 4, add.axis = FALSE)
    axis(2,
         at = breaks.axis,
         labels = round(breaks.axis, 3),
         las = 0, cex.axis = font.size[6])
    if(show.overall){
      cat("Overall Treatment effect is:",
          overall.treatment.mean, ", with confidence interval: (",
          overall.treatment.lower,";",overall.treatment.upper,")")
      points(x = 0.5,
             (overall.treatment.mean), pch = 20)
      points(x = 0.5, overall.treatment.lower, pch = "-")
      points(x = 0.5, overall.treatment.upper, pch = "-")
      segments(x0 = 0.5, x1 = 0.5,
               y0 = overall.treatment.lower,
               y1 = overall.treatment.upper)
    }
    # abline(h=levs)
    mtext(strip, side=4, line=1, cex.lab = font.size[5])
    par(mfrow=c(1,1))
  }

}
# for (i in 1 : n.subgrp){
#   lab.subgrp[i] = paste(" (", LETTERS[i],") ", lab.vars[i], " = ", cats.var.all[[i]][cat.sel[i]], sep = "")
# }
# legend(xy.current.pos[1], xy.current.pos[4], lab.subgrp, bty = "n", cex = 0.9)


### make a color scale to show effect sizes
#
# par(mar=c(1,4.2, 3, 2.5))
# SubgrPlots:::image.scale(treat_size, col=pal.2(length(breaks)-1), breaks=breaks-1e-8, axis.pos=4, add.axis=FALSE)
# axis(4,at=breaks, labels= round(breaks, 3), las = 0, cex.axis = 1)
# box()
# abline(h=levs)
# title(ylab="Treatment effect size", cex.lab = 1.2, font.lab = 2)
# # dev.off()
