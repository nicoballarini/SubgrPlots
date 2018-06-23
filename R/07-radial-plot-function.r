#' Radial (Galbraith) plot for subgroup effect size
#'
#' This function produces a modified Galbraith's radial plot showing the treatment effect size of subgroups defined by the categories
#' of covariates. The x-axis represents the reciprocal of the standard error of subgroup treatment effect estimates.The y-axis means
#' standardized effect size difference (the difference between subgroup effect the full popultion effect is divided by the standard error
#' of the estimator for the overall population effect. Points here are for subgroups. The grey region indicates whether subgroup effects
#' are homogeneous to the full population effect or not. The two arcs on the right side show subgroup treatment effects in the original
#' scale, where the red spots are the projection of points from the origin on the left side. Note that the vertical range of display can
#' be changed by setting different values on the associated input argument. In addition, the function uses log odd ratio and log hazard
#' ratio for displaying subgroup effect sizes in binary and survival data, respectively.
#'
#'@param dat:              a data set
#'@param covari.sel:       a vector of indices of the two covariates
#'@param trt.sel:          a covariate index specifying the treatment code
#'@param resp.sel:         a covariate index specifying the response variable
#'@param outcome.type:     a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#'@param range.v:          a vector specifying the vertical range of graphical display.
#'@param adj.ann.subgrp:   a parameter adjusting the distance between a point and its corresponding subgroup label. The smaller the value
#' is, the larger the distance is.
#'@param font.size:        a vector specifying the size of labels and text; the first element is for the main title, the second is for
#' for x-axis and y-axis labels; the thrid is for the legend text of subgroups; the fourth is for the subgroup
#' labels near points; the fifth is for the unit labels on all the axes.
#'@param title:            a string specifying the main title.
#'@param lab.xy:           a list of two strings specifying the labels of the x and y axes.
#
# eg.1              main.title = "Radial plot"
#                   label.xy = list("1/SE", "Standardized effect size difference   ")
#                   radplt(dat = dat, covari.sel = c(4, 6), trt.sel = 2, resp.sel = 1, outcome.type = "continuous", title = main.title,
#                   lab.xy = label.xy, range.v = c(-4, 6))
#
# eg.2              main.title = "Radial plot"
#                   label.xy = list("1/SE", "Standardized effect size (log odds ratio) difference   ")
#                   radplt(dat = dat3, covari.sel = c(6, 7), trt.sel = 1, resp.sel = 3, outcome.type = "binary", title = main.title, adj.ann.subgrp = 1,
#                   lab.xy = label.xy, range.v = c(-11, 4))
#
# eg.3              main.title = "Radial plot"
#                   label.xy = list("1/SE", "Standardized effect size (log hazard ratio) difference   ")
#                   radplt(dat = dat3, covari.sel = c(6, 7), trt.sel = 1, resp.sel = c(4,3), outcome.type = "survival", title = main.title, adj.ann.subgrp = 1,
#                   lab.xy = label.xy, range.v = c(-14, 4))
#
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
#' @import grid
#' @import graphics
plot_radial <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type, range.v = NULL, adj.ann.subgrp = 4, font.size = c(1, 1, 0.85, 0.85, 1),
          title = NULL, lab.xy = NULL)
{

  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")

  if (missing(trt.sel)) stop("The variable specifying the treatment code (for treatment / control groups) has not been specified!")
  if (!(length(trt.sel) == 1)) stop("The variable specifying the treatment code can not have more than one component!")
  if (!(is.factor(dat[, trt.sel]))) stop("The variable specifying the treatment code is not categorical!")
  if (length(names(table(dat[, trt.sel]))) > 2) stop("The variable specifying the treatment code is not binary!")
  if (sum(is.element(names(table(dat[, trt.sel])), c("0","1"))) != 2) stop("The treatment code is not 1 and 0 (for treatment / control groups)!")

  type.all = c("continuous", "binary",  "survival")
  if (is.null(outcome.type)) stop("The type of the response variable has not been specified!")
  if (!(is.element(outcome.type, type.all)) == TRUE) stop("A unrecognized type has been inputed!")
  if (outcome.type == "continuous"){
    if (missing(resp.sel)) stop("The response variable has not been specified!")
    if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
    if (!(is.numeric(dat[, resp.sel]))) stop("The response variable is not numeric!")
  }else if (outcome.type == "binary"){
    if (missing(resp.sel)) stop("The response variable has not been specified!")
    if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
    if (!(is.factor(dat[, resp.sel]) || is.numeric(dat[, resp.sel])  )) stop("The response variable is not categorical or numerical!")
    if (length(names(table(dat[, resp.sel]))) > 2) stop("The response variable is not binary!")
    if (sum(is.element(names(table(dat[, resp.sel])), c("0","1"))) != 2) stop(" The response variable is not coded as 0 and 1!")
  }else if (outcome.type == "survival"){
    if (missing(resp.sel)) stop("The response variablehas not been specified!")
    if (!(length(resp.sel) == 2)) stop("The response variable for analysing survival data should have two components!")
    if (!(is.numeric(dat[, resp.sel[1]]))) stop("The response variable specifying survival time is not numeric!")
    if (!(is.numeric(dat[, resp.sel[2]]) || is.logical(dat[, resp.sel[2]]) ) ) stop("The response variable specifying indicators of right censoring should be numerical or logical!")
    if (length(names(table(dat[, resp.sel[2]]))) > 2) stop("The response variable specifying indicators of right censoring is not binary!")
    if (sum(is.element(names(table(dat[, resp.sel[2]])), c("0","1"))) != 2) stop("The response variable specifying indicators of right censoring is not coded as 0 and 1!")
  }

  if (!(length(range.v) == 2)) stop("The vertical range of graphical display should have two components (specifying the minimum and maximum!)")

  if (!(is.numeric(adj.ann.subgrp))) stop("The argument adjusting the distance between a point and its corresponding subgroup label is not numeric!")
  if (adj.ann.subgrp < 0) stop("The value adjusting the distance between a point and its corresponding subgroup label is not positive!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 5)) stop("The font size setups for labels or text should have five components only!")

  ################################################ 1. create subgroup data  #################################################################

  n.covari = length(covari.sel)
  lab.vars = names(dat)[covari.sel]                                                # set the names of the covariates which relates to the defined subgroup; if a covariate
                                                                                   # are considered for multiple times, we make their name identical. (otherwise, the resulsting
                                                                                   # names are like var, var.1, var.2 and so on.)

  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
  }

  for (i in 1: length(covari.sel)){
    cond = covari.sel == covari.sel[[i]]
    lab.vars[cond] = rep(lab.vars[i], length(which(cond == TRUE)))
  }
  cats.var = list()
  n.subgrp.tol = 0
  for (i in 1 : length(covari.sel)){
    cats.var[[i]] = names(table(dat[,covari.sel[i]]))
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])
  }

  cond = list()
  data.subgrp = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)
  k = 0
  for (i in 1 : length(covari.sel)) {
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      cond[[k]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
      ss.subgrp[k, k] = length(cond[[k]])
      data.subgrp[[k]] = dat[cond[[k]], ]
    }
  }

  k = n.subgrp.tol
  for (i in 1 : (n.subgrp.tol - 1) ){
    for (j in (i + 1) : (n.subgrp.tol) ){
      k = k + 1
      cond[[k]] = intersect(cond[[i]], cond[[j]])
      ss.subgrp[i, j] = length(cond[[k]])
      ss.subgrp[j, i] = length(cond[[k]])
    }
  }

  # create matrices for treatment size and standard error of MLE for subgroups and the full
  # population

  if (sum((dat$trt == "1")) == 0 | sum((dat$trt == "0")) == 0){
    treatment.mean.full = NA
    treatment.std.full = NA
    zscore.full = NA

  }else{

    if (outcome.type == "continuous"){

      model.int = lm(resp ~ trt,  data = dat)
      model.sum = summary(model.int)
      treatment.mean.full = model.sum$coefficients[2, 1]
      treatment.std.full = model.sum$coefficients[2, 2]
      zscore.full = treatment.mean.full / treatment.std.full

    }else if (outcome.type == "binary"){

      model.int = glm(resp ~ trt, family = "binomial", data = dat)
      model.sum = summary(model.int)
      treatment.mean.full = model.sum$coefficients[2, 1]
      treatment.std.full = model.sum$coefficients[2, 2]
      zscore.full = treatment.mean.full / treatment.std.full

    }else if (outcome.type == "survival"){

      model.int = survival::coxph(Surv(time, status) ~ trt, data = dat)
      model.sum = summary(model.int)
      treatment.mean.full = model.sum$coef[1, 1]
      treatment.std.full = model.sum$coef[1, 3]
      zscore.full = treatment.mean.full / treatment.std.full

      }
    }

  treatment.mean = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  treatment.std  = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  zscore = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  for (i in 1 : (n.subgrp.tol)){
    if (sum((data.subgrp[[i]]$trt == "1")) == 0 | sum((data.subgrp[[i]]$trt == "0")) == 0){
      treatment.mean[i] = NA
      treatment.std[i] = NA
      zscore[i] = NA

    }else{

      if (outcome.type == "continuous"){

        model.int =  lm(resp ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
        treatment.std[i] = model.sum$coefficients[2, 2]
        zscore[i] = treatment.mean[i] / treatment.std[i]                             # calculate the z-score of the subgroup effect size relative
                                                                                        # to the standardized error of the full population effect size
                                                                                        # estimator
      }else if (outcome.type == "binary"){

        model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
        treatment.std[i] = model.sum$coefficients[2, 2]
        zscore[i] = treatment.mean[i] / treatment.std[i]                        # calculate the z-score of the subgroup effect size relative
                                                                                        # to the standardized error of the full population effect size
                                                                                        # estimator
      }else if (outcome.type == "survival"){

        model.int = survival::coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.std[i] = model.sum$coef[1, 3]
        zscore[i] = treatment.mean[i] / treatment.std[i]
      }
    }

  }

  lab.subgrp = vector()
  lab.subgrp2 = vector()
  k = 0
  for (i in 1: length(covari.sel)){
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      lab.subgrp[k] = paste("(", LETTERS[i], j, ") ", lab.vars[i], " = ", cats.var[[i]][j], sep = "")
      lab.subgrp2[k]  = paste(LETTERS[i], j, sep = "")
    }
  }
  dimnames(treatment.mean) = list(c(lab.subgrp), c("mean") )
  dimnames(treatment.std) = list(c(lab.subgrp), c("std") )
  dimnames(zscore) = list(c(lab.subgrp), c("zscore") )

  ################################################ 2. produce a graph  #################################################################


  par(mar = c(4,4,2,2))

  if (is.null(range.v)){
   y.lim.max = ceiling(max(zscore, na.rm = TRUE))
   y.lim.min = floor(min(zscore, na.rm = TRUE))
   if (y.lim.max <= 2)  {y.lim.max = y.lim.max + 6}
   if (y.lim.min >= -2) {y.lim.min = y.lim.min -3}
  }else{
   y.lim.max = range.v[2]
   y.lim.min = range.v[1]
  }

  plot(0, 0, type='n',
       xlab = lab.xy[[1]], ylab = lab.xy[[2]],
       ylim = c(y.lim.min, y.lim.max),
       xlim = c(0,13.5), xaxt = 'n', yaxt='n',
       main = title, cex.lab = font.size[2], cex.main = font.size[1])

  expand.fac = 2                                                                   # the expansion factor of the original unit on the x-axis
  length.unit.xaxis = 5                                                            # the number of the units on the x-axis
  upper.bd = 2; lower.bd = -2
  x.lab.max.pos = round(max(1/c(treatment.std, treatment.std.full))) + 0.1 #ceiling(max(1/treatment.std))
  x.lab.max.pos = round(max(1/c(treatment.std, treatment.std.full))) + 1 #ceiling(max(1/treatment.std))
  y.lab.upp.pos = zscore.full + upper.bd
  y.lab.low.pos = zscore.full + lower.bd
  axis(1, at= seq(0, length.unit.xaxis * expand.fac, 1 * expand.fac),
       labels = round(seq(0, x.lab.max.pos, len = length.unit.xaxis + 1), 2), cex.axis = font.size[5]  )

  xy.current.pos = par("usr")
  legend(xy.current.pos[1], xy.current.pos[4], lab.subgrp, bty = "n", cex = font.size[3])

  pos.esize.mean = zscore.full                  # the y-coordinate of the full
  pos.esize.lower = pos.esize.mean + lower.bd
  pos.esize.upper = pos.esize.mean + upper.bd

  yy1 = seq(pos.esize.mean, y.lim.min + zscore.full, -2)
  yy2 = seq(pos.esize.mean, y.lim.max + zscore.full,  2)
  lab = c(rev(yy1), yy2)
  lab = c(rep(NA, len = length(seq(y.lim.min, lower.bd - 1, 1))),
  c(lower.bd, NA, 0, NA, upper.bd),
  c(pos.esize.lower, NA, pos.esize.mean, NA, pos.esize.upper),
  rep(NA, len = length(seq(upper.bd +1, y.lim.max, 1))))
  # axis(2, at = round(seq(y.lim.min + zscore.full, y.lim.max + zscore.full, 2),2),
  #      las = 1,
  #      # labels = round(lab, 2),
  #      cex.axis = font.size[5])

  axis(2,
       at= seq(y.lab.low.pos - 1, y.lab.upp.pos + 1, 1)[c(2,4,6)],
       labels = c(NA, lower.bd, NA, 0, NA, upper.bd, NA)[c(2,4,6)],
       cex.axis = font.size[5] )


  x <- c(0, length.unit.xaxis * expand.fac, length.unit.xaxis * expand.fac, 0)
  y <- c(pos.esize.lower, pos.esize.lower, pos.esize.upper, pos.esize.upper)
  x = c(x, x[1])
  y = c(y, y[1])
  polygon(x, y,  col = "gray", border = NA)                                         # the confidence band (-2 to 2)

  x1 = seq(0, length.unit.xaxis * expand.fac, len=200)
  y1 = rep(pos.esize.mean, 200)
  lines(x1, y1)                                                                     # draw the central line
  y2 =rep(pos.esize.lower, 200)
  y3 =rep(pos.esize.upper, 200)
  lines(x1, y2, lty = 2)                                                            # draw the lower boundary
  lines(x1, y3, lty = 2)                                                            # draw the upper boundary

  lab.subgrp.pos.adj = 1/(adj.ann.subgrp * length.unit.xaxis * expand.fac) # 1/(5 * length.unit.xaxis * expand.fac) #1/50
  expand.fac.adj = expand.fac * length.unit.xaxis * 1/(x.lab.max.pos)
  points(1/treatment.std * expand.fac.adj,
         zscore,
         cex = font.size[4]/2, pch = 19)             # add the points for the subgroups
  text((1/treatment.std) * expand.fac.adj,# + 10*lab.subgrp.pos.adj  * expand.fac.adj,
       zscore + 10*lab.subgrp.pos.adj,
       labels = lab.subgrp2,  cex = font.size[4])      # the annotation for the points
  points(1/treatment.std.full * expand.fac.adj,
         zscore.full,
         cex = font.size[4]/2, pch = 19)             # add the points for the subgroups
  text((1/treatment.std.full) * expand.fac.adj,# + 10*lab.subgrp.pos.adj  * expand.fac.adj,
       zscore.full + 10*lab.subgrp.pos.adj,
       labels = "Full",  cex = font.size[4])      # the annotation for the points

  ### draw arcs for displaying effect sizes

  angle = vector()
  zscore.diff = zscore - zscore.full
  len.pt = dim(zscore)[1]
  for (i in 1: len.pt){
    angle[i] = atan( zscore.diff[i]/ ((1/treatment.std[i] * expand.fac.adj)))        # calculate the angle between two vectors
  }

  if (sum(angle>0) != length(angle)){                                               # when the zscores of subgroups are partially larger and less than the zscore of the full population

    axis.circle1.max = max(angle)                                                   # the difference between the maximal effect size and the nearest effect size unit
    axis.circle1.min = 0                                                            # the effect size for the full population

    x0 =0; y0= zscore.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + ((length.unit.xaxis + 0.2)* expand.fac)^2)) - 0.1     # the radius of the outer circle

    theta <- seq(axis.circle1.min,  axis.circle1.max ,  len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)                                                                                # draw the cicular axis

    r1 = r + 0.2
    theta1 <- seq(axis.circle1.min,  axis.circle1.max, len = 7)
    esize.unit <- seq(treatment.mean.full,  max(treatment.mean), len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5, y0 + r1 * sin(theta1[i]), labels = esize.unit[i], cex = font.size[5])  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.pos = angle[angle>0]
    for (i in 1: length(angle.pos)){
      x1 = x0 + seq(r * cos(angle.pos[i]), r1 * cos(angle.pos[i]), len=10)
      y1 = y0 + seq(r * sin(angle.pos[i]), r1 * sin(angle.pos[i]), len=10)
      lines(x1, y1, col = "red")                                                               # drew the lines for the unit used in effect size
    }

    axis.circle2.max = 0                                                                       # the effect size for the full population
    axis.circle2.min = min(angle)                                                              # the difference between the minmal effect size and the nearest effect size unit

    x0 =0; y0= zscore.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + ((length.unit.xaxis + 0.9)* expand.fac)^2)) - 0.1     # the radius of the outer circle

    theta <- seq(axis.circle2.min,  axis.circle2.max ,  len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)     # draw the cicular axis

    r1 = r + 0.2
    theta1 <- seq(axis.circle2.min,  axis.circle2.max, len = 7)
    esize.unit <- seq(min(treatment.mean), treatment.mean.full, len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5, y0 + r1 * sin(theta1[i]), labels = esize.unit[i], cex = font.size[5])  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.neg = angle[angle<0]
    for (i in 1: length(angle.neg)){
      x1 = x0 + seq(r * cos(angle.neg[i]), r1 * cos(angle.neg[i]), len=10)
      y1 = y0 + seq(r * sin(angle.neg[i]), r1 * sin(angle.neg[i]), len=10)
      lines(x1, y1, col = "red")                                                               # drew the lines for the unit used in effect size
    }

  }else if (sum(angle>0) == length(angle)){                                                    # when all zscores of subgroups are larger than the zscore of the full population

    axis.circle1.max = max(angle)                                                              # the difference between the maximal effect size and the nearest effect size unit
    axis.circle1.min = 0                                                                       # the effect size for the full population

    x0 =0; y0= zscore.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + (5.2* expand.fac) ^2)) - 0.1                           # the radius of the outer circle

    theta <- seq(axis.circle1.min,  axis.circle1.max ,  len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)                                                                                # draw the cicular axis

    r1 = r + 0.2
    theta1 <- seq(axis.circle1.min,  axis.circle1.max, len = 7)
    esize.unit <- seq(treatment.mean.full,  max(treatment.mean), len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5, y0 + r1 * sin(theta1[i]), labels = esize.unit[i], cex = font.size[5])  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.pos = angle[angle>0]
    for (i in 1: length(angle.pos)){
      x1 = x0 + seq(r * cos(angle.pos[i]), r1 * cos(angle.pos[i]), len=10)
      y1 = y0 + seq(r * sin(angle.pos[i]), r1 * sin(angle.pos[i]), len=10)
      lines(x1, y1, col = "red")                                                                # drew the lines for the unit used in effect size
    }

  }else if (sum(angle<0) == length(angle)){                                                     # when all zscores of subgroups are less than the zscore of the full population

    axis.circle2.max = 0                                                                        # the effect size for the full population
    axis.circle2.min = min(angle)                                                               # the difference between the minmal effect size and the nearest effect size unit

    x0 =0; y0= zscore.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + (5.9* expand.fac) ^2)) - 0.1                            # the radius of the outer circle

    theta <- seq(axis.circle2.min,  axis.circle2.max , len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)     # draw the cicular axis

    r1 = r + 0.2
    theta1 <- seq(axis.circle2.min,  axis.circle2.max, len = 7)
    esize.unit <- seq(min(treatment.mean), treatment.mean.full, len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5, y0 + r1 * sin(theta1[i]), labels = esize.unit[i], cex = font.size[5])  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.neg = angle[angle<0]
    for (i in 1: length(angle.neg)){
      x1 = x0 + seq(r * cos(angle.neg[i]), r1 * cos(angle.neg[i]), len=10)
      y1 = y0 + seq(r * sin(angle.neg[i]), r1 * sin(angle.neg[i]), len=10)
      lines(x1, y1, col = "red")                                                               # drew the lines for the unit used in effect size
    }

  }

}




#' Radial (Galbraith) plot for subgroup effect size
#'
#' This function produces a modified Galbraith's radial plot showing the treatment effect size of subgroups defined by the categories
#' of covariates. The x-axis represents the reciprocal of the standard error of subgroup treatment effect estimates.The y-axis means
#' standardized effect size difference (the difference between subgroup effect the full popultion effect is divided by the standard error
#' of the estimator for the overall population effect. Points here are for subgroups. The grey region indicates whether subgroup effects
#' are homogeneous to the full population effect or not. The two arcs on the right side show subgroup treatment effects in the original
#' scale, where the red spots are the projection of points from the origin on the left side. Note that the vertical range of display can
#' be changed by setting different values on the associated input argument. In addition, the function uses log odd ratio and log hazard
#' ratio for displaying subgroup effect sizes in binary and survival data, respectively.
#'
#'@param dat:              a data set
#'@param covari.sel:       a vector of indices of the two covariates
#'@param trt.sel:          a covariate index specifying the treatment code
#'@param resp.sel:         a covariate index specifying the response variable
#'@param outcome.type:     a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#'@param range.v:          a vector specifying the vertical range of graphical display.
#'@param adj.ann.subgrp:   a parameter adjusting the distance between a point and its corresponding subgroup label. The smaller the value
#' is, the larger the distance is.
#'@param font.size:        a vector specifying the size of labels and text; the first element is for the main title, the second is for
#' for x-axis and y-axis labels; the thrid is for the legend text of subgroups; the fourth is for the subgroup
#' labels near points; the fifth is for the unit labels on all the axes.
#'@param title:            a string specifying the main title.
#'@param lab.xy:           a list of two strings specifying the labels of the x and y axes.
#
# eg.1              main.title = "Radial plot"
#                   label.xy = list("1/SE", "Standardized effect size difference   ")
#                   radplt(dat = dat, covari.sel = c(4, 6), trt.sel = 2, resp.sel = 1, outcome.type = "continuous", title = main.title,
#                   lab.xy = label.xy, range.v = c(-4, 6))
#
# eg.2              main.title = "Radial plot"
#                   label.xy = list("1/SE", "Standardized effect size (log odds ratio) difference   ")
#                   radplt(dat = dat3, covari.sel = c(6, 7), trt.sel = 1, resp.sel = 3, outcome.type = "binary", title = main.title, adj.ann.subgrp = 1,
#                   lab.xy = label.xy, range.v = c(-11, 4))
#
# eg.3              main.title = "Radial plot"
#                   label.xy = list("1/SE", "Standardized effect size (log hazard ratio) difference   ")
#                   radplt(dat = dat3, covari.sel = c(6, 7), trt.sel = 1, resp.sel = c(4,3), outcome.type = "survival", title = main.title, adj.ann.subgrp = 1,
#                   lab.xy = label.xy, range.v = c(-14, 4))
#
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
#' @import grid
#' @import graphics
plot_radial2 <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type, range.v = NULL, adj.ann.subgrp = 4, font.size = c(1, 1, 0.85, 0.85, 1),
                        title = NULL, lab.xy = NULL, plot.full = FALSE)
{

  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")

  if (missing(trt.sel)) stop("The variable specifying the treatment code (for treatment / control groups) has not been specified!")
  if (!(length(trt.sel) == 1)) stop("The variable specifying the treatment code can not have more than one component!")
  if (!(is.factor(dat[, trt.sel]))) stop("The variable specifying the treatment code is not categorical!")
  if (length(names(table(dat[, trt.sel]))) > 2) stop("The variable specifying the treatment code is not binary!")
  if (sum(is.element(names(table(dat[, trt.sel])), c("0","1"))) != 2) stop("The treatment code is not 1 and 0 (for treatment / control groups)!")

  type.all = c("continuous", "binary",  "survival")
  if (is.null(outcome.type)) stop("The type of the response variable has not been specified!")
  if (!(is.element(outcome.type, type.all)) == TRUE) stop("A unrecognized type has been inputed!")
  if (outcome.type == "continuous"){
    if (missing(resp.sel)) stop("The response variable has not been specified!")
    if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
    if (!(is.numeric(dat[, resp.sel]))) stop("The response variable is not numeric!")
  }else if (outcome.type == "binary"){
    if (missing(resp.sel)) stop("The response variable has not been specified!")
    if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
    if (!(is.factor(dat[, resp.sel]) || is.numeric(dat[, resp.sel])  )) stop("The response variable is not categorical or numerical!")
    if (length(names(table(dat[, resp.sel]))) > 2) stop("The response variable is not binary!")
    if (sum(is.element(names(table(dat[, resp.sel])), c("0","1"))) != 2) stop(" The response variable is not coded as 0 and 1!")
  }else if (outcome.type == "survival"){
    if (missing(resp.sel)) stop("The response variablehas not been specified!")
    if (!(length(resp.sel) == 2)) stop("The response variable for analysing survival data should have two components!")
    if (!(is.numeric(dat[, resp.sel[1]]))) stop("The response variable specifying survival time is not numeric!")
    if (!(is.numeric(dat[, resp.sel[2]]) || is.logical(dat[, resp.sel[2]]) ) ) stop("The response variable specifying indicators of right censoring should be numerical or logical!")
    if (length(names(table(dat[, resp.sel[2]]))) > 2) stop("The response variable specifying indicators of right censoring is not binary!")
    if (sum(is.element(names(table(dat[, resp.sel[2]])), c("0","1"))) != 2) stop("The response variable specifying indicators of right censoring is not coded as 0 and 1!")
  }

  if (!(length(range.v) == 2)) stop("The vertical range of graphical display should have two components (specifying the minimum and maximum!)")

  if (!(is.numeric(adj.ann.subgrp))) stop("The argument adjusting the distance between a point and its corresponding subgroup label is not numeric!")
  if (adj.ann.subgrp < 0) stop("The value adjusting the distance between a point and its corresponding subgroup label is not positive!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 5)) stop("The font size setups for labels or text should have five components only!")

  ################################################ 1. create subgroup data  #################################################################


  n.covari = length(covari.sel)
  lab.vars = names(dat)[covari.sel]                                                # set the names of the covariates which relates to the defined subgroup; if a covariate
  # are considered for multiple times, we make their name identical. (otherwise, the resulsting
  # names are like var, var.1, var.2 and so on.)

  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
  }


  n_t = sum(dat$trt == 1)
  n_c = sum(dat$trt == 0)

  for (i in 1: length(covari.sel)){
    cond = covari.sel == covari.sel[[i]]
    lab.vars[cond] = rep(lab.vars[i], length(which(cond == TRUE)))
  }
  cats.var = list()
  n.subgrp.tol = 0
  for (i in 1 : length(covari.sel)){
    cats.var[[i]] = names(table(dat[,covari.sel[i]]))
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])
  }

  cond = list()
  data.subgrp = list()
  lambda_t = lambda_c = lambda = deltaS = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)
  k = 0
  for (i in 1 : length(covari.sel)) {
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      cond[[k]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
      ss.subgrp[k, k] = length(cond[[k]])
      data.subgrp[[k]] = dat[cond[[k]], ]
      lambda_t[[k]] = sum(dat[, covari.sel[i]] == cats.var[[i]][j] & dat$trt == 1)/sum(dat$trt == 1)
      lambda_c[[k]] = sum(dat[, covari.sel[i]] == cats.var[[i]][j] & dat$trt == 0)/sum(dat$trt == 0)
      lambda[[k]]   = sum(dat[, covari.sel[i]] == cats.var[[i]][j])/nrow(dat)

      if (outcome.type == "survival"){
        which. = dat[, covari.sel[i]] == cats.var[[i]][j]
        # dat[which.,]
        lambda[[k]] = sum(dat[which., "status"] == 1)/sum(dat$status == 1)
      }
      if (outcome.type == "continuous"){
      deltaS[[k]]   = mean(dat$resp[which(dat[, covari.sel[i]] == cats.var[[i]][j] & dat$trt == 1)]) -
                      mean(dat$resp[which(dat[, covari.sel[i]] == cats.var[[i]][j] & dat$trt == 0)])
      }
    }
  }

  k = n.subgrp.tol
  for (i in 1 : (n.subgrp.tol - 1) ){
    for (j in (i + 1) : (n.subgrp.tol) ){
      k = k + 1
      cond[[k]] = intersect(cond[[i]], cond[[j]])
      ss.subgrp[i, j] = length(cond[[k]])
      ss.subgrp[j, i] = length(cond[[k]])
    }
  }

  # create matrices for treatment size and standard error of MLE for subgroups and the full
  # population

  if (sum((dat$trt == "1")) == 0 | sum((dat$trt == "0")) == 0){
    treatment.mean.full = NA
    treatment.std.full = NA
    zscore.full = NA

  }else{

    if (outcome.type == "continuous"){

      model.int = lm(resp ~ trt,  data = dat)
      model.sum = summary(model.int)
      treatment.mean.full = model.sum$coefficients[2, 1]
      treatment.std.full  = model.sum$coefficients[2, 2]
      zscore.full = treatment.mean.full / treatment.std.full
      zscore.full = 0

      sigma2      = sum((model.int$residuals^2))/model.int$df.residual
      varDeltaF   = (sigma2/sum(dat$trt == 1) + sigma2/sum(dat$trt == 0))
      deltaF = mean(dat$resp[which(dat$trt == 1)]) -
               mean(dat$resp[which(dat$trt == 0)])

    }else if (outcome.type == "binary"){

      model.int = glm(resp ~ trt, family = "binomial", data = dat)
      model.sum = summary(model.int)
      treatment.mean.full = model.sum$coefficients[2, 1]
      treatment.std.full = model.sum$coefficients[2, 2]
      zscore.full = treatment.mean.full / treatment.std.full

    }else if (outcome.type == "survival"){

      model.int = survival::coxph(Surv(time, status) ~ trt, data = dat)
      model.sum = summary(model.int)
      treatment.mean.full = model.sum$coef[1, 1]
      treatment.std.full = model.sum$coef[1, 3]
      zscore.full = treatment.mean.full / treatment.std.full
      zscore.full = 0

      varDeltaF   = treatment.std.full^2
      deltaF = treatment.mean.full

    }
  }

  treatment.mean = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  treatment.std  = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  zscore = VarDsDf = sdDsDf = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  i=1
  for (i in 1 : (n.subgrp.tol)){
    if (sum((data.subgrp[[i]]$trt == "1")) == 0 | sum((data.subgrp[[i]]$trt == "0")) == 0){
      treatment.mean[i] = NA
      treatment.std[i] = NA
      zscore[i] = NA

    }else{

      if (outcome.type == "continuous"){
        model.int =  lm(resp ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
        treatment.std[i] = model.sum$coefficients[2, 2]
        # zscore[i] = treatment.mean[i] / treatment.std[i]                             # calculate the z-score of the subgroup effect size relative
        # to the standardized error of the full population effect size
        # estimator
        VarDsDf[i] = ((1-lambda_t[[i]])/lambda_t[[i]]/n_t +
                      (1-lambda_c[[i]])/lambda_c[[i]]/n_c) * sigma2
        zscore[i] = (deltaS[[i]] - deltaF) / sqrt(VarDsDf[i])
        sdDsDf[i] = sqrt(VarDsDf[i])
      }else if (outcome.type == "binary"){

        model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
        treatment.std[i] = model.sum$coefficients[2, 2]
        zscore[i] = treatment.mean[i] / treatment.std[i]                        # calculate the z-score of the subgroup effect size relative
        # to the standardized error of the full population effect size
        # estimator
      }else if (outcome.type == "survival"){

        model.int = survival::coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.std[i] = model.sum$coef[1, 3]
        zscore[i] = treatment.mean[i] / treatment.std[i]

        VarDsDf[i] = treatment.std[i]^2 + varDeltaF -
          2 * sqrt(lambda[[i]])*treatment.std[i]*sqrt(varDeltaF)
        zscore[i] = (treatment.mean[i] - deltaF) / sqrt(VarDsDf[i])
        sdDsDf[i] = sqrt(VarDsDf[i])

      }
    }

  }

  lab.subgrp = vector()
  lab.subgrp2 = vector()
  k = 0
  for (i in 1: length(covari.sel)){
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      lab.subgrp[k] = paste("(", LETTERS[i], j, ") ", lab.vars[i], " = ", cats.var[[i]][j], sep = "")
      lab.subgrp2[k]  = paste(LETTERS[i], j, sep = "")
    }
  }
  dimnames(treatment.mean) = list(c(lab.subgrp), c("mean") )
  dimnames(treatment.std) = list(c(lab.subgrp), c("std") )
  dimnames(zscore) = list(c(lab.subgrp), c("zscore") )
  dimnames(sdDsDf) = list(c(lab.subgrp), c("sdDsDf") )
  print(treatment.mean.full)
  print(data.frame(treatment.mean, zscore, sdDsDf, treatment.std,
                   lambda_t=unlist(lambda_t),
                   lambda_c=unlist(lambda_c),
                   lambda = unlist(lambda)))
  ################################################ 2. produce a graph  #################################################################
  # par(mar = c(4,5,2,2))
  par(mar = c(4,4,1,1))
  if (is.null(range.v)){
    y.lim.max = ceiling(max(zscore, na.rm = TRUE))
    y.lim.min = floor(min(zscore, na.rm = TRUE))
    if (y.lim.max <= 2)  {y.lim.max = y.lim.max + 6}
    if (y.lim.min >= -2) {y.lim.min = y.lim.min -3}
  }else{
    y.lim.max = range.v[2]
    y.lim.min = range.v[1]
  }

  plot(0, 0, type='n',
       xlab = "",#lab.xy[[1]],
       ylab = "",#lab.xy[[2]],
       ylim = c(y.lim.min, y.lim.max),
       xlim = c(0,13.5), xaxt = 'n', yaxt='n', xaxs = "i",
       main = title, cex.lab = font.size[2], cex.main = font.size[1])

  mtext(text =  lab.xy[[2]], side = 2, line = 2)
  mtext(text =  lab.xy[[1]], side = 1, line = 3)

  expand.fac = 2                                                                  # the expansion factor of the original unit on the x-axis
  length.unit.xaxis = 5                                                            # the number of the units on the x-axis
  upper.bd = 2; lower.bd = -2
  x.lab.max.pos = round(max(1/c(treatment.std, treatment.std.full))) + 0.1 #ceiling(max(1/treatment.std))
  x.lab.max.pos = round(max(1/c(sdDsDf, sqrt(varDeltaF)))) + 1 #ceiling(max(1/treatment.std))
  y.lab.upp.pos = zscore.full + upper.bd
  y.lab.low.pos = zscore.full + lower.bd
  axis(1, at= seq(0, length.unit.xaxis * expand.fac, 1 * expand.fac),
       labels = round(seq(0, x.lab.max.pos, len = length.unit.xaxis + 1), 2), cex.axis = font.size[5]  )

  xy.current.pos = par("usr")
  legend(xy.current.pos[1], xy.current.pos[4], lab.subgrp, bty = "n", cex = font.size[3])

  pos.esize.mean  = zscore.full                  # the y-coordinate of the full
  pos.esize.lower = pos.esize.mean + lower.bd
  pos.esize.upper = pos.esize.mean + upper.bd

  yy1 = seq(pos.esize.mean, y.lim.min + zscore.full, -2)
  yy2 = seq(pos.esize.mean, y.lim.max + zscore.full,  2)
  lab = c(rev(yy1), yy2)
  lab = c(rep(NA, len = length(seq(y.lim.min, lower.bd - 1, 1))),
          c(lower.bd, NA, 0, NA, upper.bd),
          c(pos.esize.lower, NA, pos.esize.mean, NA, pos.esize.upper),
          rep(NA, len = length(seq(upper.bd +1, y.lim.max, 1))))
  # axis(2, at = round(seq(y.lim.min + zscore.full, y.lim.max + zscore.full, 2),2),
  #      las = 1,
  #      # labels = round(lab, 2),
  #      cex.axis = font.size[5])

  axis(2,
       at= seq(y.lab.low.pos - 1, y.lab.upp.pos + 1, 1)[c(2,4,6)],
       labels = c(NA, lower.bd, NA, 0, NA, upper.bd, NA)[c(2,4,6)],
       cex.axis = font.size[5] )


  x <- c(0, length.unit.xaxis * expand.fac, length.unit.xaxis * expand.fac, 0)
  y <- c(pos.esize.lower, pos.esize.lower, pos.esize.upper, pos.esize.upper)
  x = c(x, x[1])
  y = c(y, y[1])
  polygon(x, y,  col = "gray", border = NA)                                         # the confidence band (-2 to 2)

  x1 = seq(0, length.unit.xaxis * expand.fac, len=200)
  y1 = rep(pos.esize.mean, 200)
  lines(x1, y1)                                                                     # draw the central line
  y2 =rep(pos.esize.lower, 200)
  y3 =rep(pos.esize.upper, 200)
  lines(x1, y2, lty = 2)                                                            # draw the lower boundary
  lines(x1, y3, lty = 2)                                                            # draw the upper boundary

  lab.subgrp.pos.adj = 1/(adj.ann.subgrp * length.unit.xaxis * expand.fac) # 1/(5 * length.unit.xaxis * expand.fac) #1/50
  expand.fac.adj = expand.fac * length.unit.xaxis * 1/(x.lab.max.pos)
  points(1/sdDsDf * expand.fac.adj,
         zscore,
         cex = font.size[4]/2, pch = 19)             # add the points for the subgroups
  text((1/sdDsDf) * expand.fac.adj,# + 10*lab.subgrp.pos.adj  * expand.fac.adj,
       zscore + 10*lab.subgrp.pos.adj,
       labels = lab.subgrp2,  cex = font.size[4])      # the annotation for the points

  if (plot.full){
  points(1/sqrt(varDeltaF) * expand.fac.adj,
         zscore.full,
         cex = font.size[4]/2, pch = 19)             # add the points for the subgroups
  text((1/treatment.std.full) * expand.fac.adj,# + 10*lab.subgrp.pos.adj  * expand.fac.adj,
       zscore.full + 10*lab.subgrp.pos.adj,
       labels = "Full",  cex = font.size[4])      # the annotation for the points
  }
  ### draw arcs for displaying effect sizes

  angle = vector()
  zscore.diff = zscore - zscore.full
  len.pt = dim(zscore)[1]
  for (i in 1: len.pt){
    angle[i] = atan( zscore.diff[i]/ ((1/sdDsDf[i] * expand.fac.adj)))        # calculate the angle between two vectors
  }

  if (sum(angle>0) != length(angle)){                                               # when the zscores of subgroups are partially larger and less than the zscore of the full population

    axis.circle1.max = max(angle)                                                   # the difference between the maximal effect size and the nearest effect size unit
    axis.circle1.min = 0                                                            # the effect size for the full population

    x0 =0; y0= zscore.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + ((length.unit.xaxis + 0.2)* expand.fac)^2)) - 0.1     # the radius of the outer circle

    theta <- seq(axis.circle1.min,  axis.circle1.max ,  len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)                                                                                # draw the cicular axis

    r1 = r + 0.2
    theta1 <- seq(axis.circle1.min,  axis.circle1.max, len = 7)
    esize.unit <- seq(treatment.mean.full,  max(treatment.mean), len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5, y0 + r1 * sin(theta1[i]), labels = esize.unit[i], cex = font.size[5])  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.pos = angle[angle>0]
    for (i in 1: length(angle.pos)){
      x1 = x0 + seq(r * cos(angle.pos[i]), r1 * cos(angle.pos[i]), len=10)
      y1 = y0 + seq(r * sin(angle.pos[i]), r1 * sin(angle.pos[i]), len=10)
      lines(x1, y1, col = "red")                                                               # drew the lines for the unit used in effect size
    }

    axis.circle2.max = 0                                                                       # the effect size for the full population
    axis.circle2.min = min(angle)                                                              # the difference between the minmal effect size and the nearest effect size unit

    x0 =0; y0= zscore.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + ((length.unit.xaxis + 0.9)* expand.fac)^2)) - 0.1     # the radius of the outer circle

    theta <- seq(axis.circle2.min,  axis.circle2.max ,  len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)     # draw the cicular axis

    r1 = r + 0.2
    theta1 <- seq(axis.circle2.min,  axis.circle2.max, len = 7)
    esize.unit <- seq(min(treatment.mean), treatment.mean.full, len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5, y0 + r1 * sin(theta1[i]), labels = esize.unit[i], cex = font.size[5])  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.neg = angle[angle<0]
    for (i in 1: length(angle.neg)){
      x1 = x0 + seq(r * cos(angle.neg[i]), r1 * cos(angle.neg[i]), len=10)
      y1 = y0 + seq(r * sin(angle.neg[i]), r1 * sin(angle.neg[i]), len=10)
      lines(x1, y1, col = "red")                                                               # drew the lines for the unit used in effect size
    }

  }else if (sum(angle>0) == length(angle)){                                                    # when all zscores of subgroups are larger than the zscore of the full population

    axis.circle1.max = max(angle)                                                              # the difference between the maximal effect size and the nearest effect size unit
    axis.circle1.min = 0                                                                       # the effect size for the full population

    x0 =0; y0= zscore.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + (5.2* expand.fac) ^2)) - 0.1                           # the radius of the outer circle

    theta <- seq(axis.circle1.min,  axis.circle1.max ,  len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)                                                                                # draw the cicular axis

    r1 = r + 0.2
    theta1 <- seq(axis.circle1.min,  axis.circle1.max, len = 7)
    esize.unit <- seq(treatment.mean.full,  max(treatment.mean), len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5, y0 + r1 * sin(theta1[i]), labels = esize.unit[i], cex = font.size[5])  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.pos = angle[angle>0]
    for (i in 1: length(angle.pos)){
      x1 = x0 + seq(r * cos(angle.pos[i]), r1 * cos(angle.pos[i]), len=10)
      y1 = y0 + seq(r * sin(angle.pos[i]), r1 * sin(angle.pos[i]), len=10)
      lines(x1, y1, col = "red")                                                                # drew the lines for the unit used in effect size
    }

  }else if (sum(angle<0) == length(angle)){                                                     # when all zscores of subgroups are less than the zscore of the full population

    axis.circle2.max = 0                                                                        # the effect size for the full population
    axis.circle2.min = min(angle)                                                               # the difference between the minmal effect size and the nearest effect size unit

    x0 =0; y0= zscore.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + (5.9* expand.fac) ^2)) - 0.1                            # the radius of the outer circle

    theta <- seq(axis.circle2.min,  axis.circle2.max , len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)     # draw the cicular axis

    r1 = r + 0.2
    theta1 <- seq(axis.circle2.min,  axis.circle2.max, len = 7)
    esize.unit <- seq(min(treatment.mean), treatment.mean.full, len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5, y0 + r1 * sin(theta1[i]), labels = esize.unit[i], cex = font.size[5])  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.neg = angle[angle<0]
    for (i in 1: length(angle.neg)){
      x1 = x0 + seq(r * cos(angle.neg[i]), r1 * cos(angle.neg[i]), len=10)
      y1 = y0 + seq(r * sin(angle.neg[i]), r1 * sin(angle.neg[i]), len=10)
      lines(x1, y1, col = "red")                                                               # drew the lines for the unit used in effect size
    }

  }

}




#' Radial (Galbraith) plot for subgroup effect size
#'
#' This function produces a modified Galbraith's radial plot showing the treatment effect size of subgroups defined by the categories
#' of covariates. The x-axis represents the reciprocal of the standard error of subgroup treatment effect estimates.The y-axis means
#' standardized effect size difference (the difference between subgroup effect the full popultion effect is divided by the standard error
#' of the estimator for the overall population effect. Points here are for subgroups. The grey region indicates whether subgroup effects
#' are homogeneous to the full population effect or not. The two arcs on the right side show subgroup treatment effects in the original
#' scale, where the red spots are the projection of points from the origin on the left side. Note that the vertical range of display can
#' be changed by setting different values on the associated input argument. In addition, the function uses log odd ratio and log hazard
#' ratio for displaying subgroup effect sizes in binary and survival data, respectively.
#'
#'@param dat:              a data set
#'@param covari.sel:       a vector of indices of the two covariates
#'@param trt.sel:          a covariate index specifying the treatment code
#'@param resp.sel:         a covariate index specifying the response variable
#'@param outcome.type:     a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#'@param range.v:          a vector specifying the vertical range of graphical display.
#'@param adj.ann.subgrp:   a parameter adjusting the distance between a point and its corresponding subgroup label. The smaller the value
#' is, the larger the distance is.
#'@param font.size:        a vector specifying the size of labels and text; the first element is for the main title, the second is for
#' for x-axis and y-axis labels; the thrid is for the legend text of subgroups; the fourth is for the subgroup
#' labels near points; the fifth is for the unit labels on all the axes.
#'@param title:            a string specifying the main title.
#'@param lab.xy:           a list of two strings specifying the labels of the x and y axes.
#
# eg.1              main.title = "Radial plot"
#                   label.xy = list("1/SE", "Standardized effect size difference   ")
#                   radplt(dat = dat, covari.sel = c(4, 6), trt.sel = 2, resp.sel = 1, outcome.type = "continuous", title = main.title,
#                   lab.xy = label.xy, range.v = c(-4, 6))
#
# eg.2              main.title = "Radial plot"
#                   label.xy = list("1/SE", "Standardized effect size (log odds ratio) difference   ")
#                   radplt(dat = dat3, covari.sel = c(6, 7), trt.sel = 1, resp.sel = 3, outcome.type = "binary", title = main.title, adj.ann.subgrp = 1,
#                   lab.xy = label.xy, range.v = c(-11, 4))
#
# eg.3              main.title = "Radial plot"
#                   label.xy = list("1/SE", "Standardized effect size (log hazard ratio) difference   ")
#                   radplt(dat = dat3, covari.sel = c(6, 7), trt.sel = 1, resp.sel = c(4,3), outcome.type = "survival", title = main.title, adj.ann.subgrp = 1,
#                   lab.xy = label.xy, range.v = c(-14, 4))
#
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
#' @import grid
#' @import graphics
plot_radial3 <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type, range.v = NULL, adj.ann.subgrp = 4, font.size = c(1, 1, 0.85, 0.85, 1),
                        title = NULL, lab.xy = NULL)
{

  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")

  if (missing(trt.sel)) stop("The variable specifying the treatment code (for treatment / control groups) has not been specified!")
  if (!(length(trt.sel) == 1)) stop("The variable specifying the treatment code can not have more than one component!")
  if (!(is.factor(dat[, trt.sel]))) stop("The variable specifying the treatment code is not categorical!")
  if (length(names(table(dat[, trt.sel]))) > 2) stop("The variable specifying the treatment code is not binary!")
  if (sum(is.element(names(table(dat[, trt.sel])), c("0","1"))) != 2) stop("The treatment code is not 1 and 0 (for treatment / control groups)!")

  type.all = c("continuous", "binary",  "survival")
  if (is.null(outcome.type)) stop("The type of the response variable has not been specified!")
  if (!(is.element(outcome.type, type.all)) == TRUE) stop("A unrecognized type has been inputed!")
  if (outcome.type == "continuous"){
    if (missing(resp.sel)) stop("The response variable has not been specified!")
    if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
    if (!(is.numeric(dat[, resp.sel]))) stop("The response variable is not numeric!")
  }else if (outcome.type == "binary"){
    if (missing(resp.sel)) stop("The response variable has not been specified!")
    if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
    if (!(is.factor(dat[, resp.sel]) || is.numeric(dat[, resp.sel])  )) stop("The response variable is not categorical or numerical!")
    if (length(names(table(dat[, resp.sel]))) > 2) stop("The response variable is not binary!")
    if (sum(is.element(names(table(dat[, resp.sel])), c("0","1"))) != 2) stop(" The response variable is not coded as 0 and 1!")
  }else if (outcome.type == "survival"){
    if (missing(resp.sel)) stop("The response variablehas not been specified!")
    if (!(length(resp.sel) == 2)) stop("The response variable for analysing survival data should have two components!")
    if (!(is.numeric(dat[, resp.sel[1]]))) stop("The response variable specifying survival time is not numeric!")
    if (!(is.numeric(dat[, resp.sel[2]]) || is.logical(dat[, resp.sel[2]]) ) ) stop("The response variable specifying indicators of right censoring should be numerical or logical!")
    if (length(names(table(dat[, resp.sel[2]]))) > 2) stop("The response variable specifying indicators of right censoring is not binary!")
    if (sum(is.element(names(table(dat[, resp.sel[2]])), c("0","1"))) != 2) stop("The response variable specifying indicators of right censoring is not coded as 0 and 1!")
  }

  if (!(length(range.v) == 2)) stop("The vertical range of graphical display should have two components (specifying the minimum and maximum!)")

  if (!(is.numeric(adj.ann.subgrp))) stop("The argument adjusting the distance between a point and its corresponding subgroup label is not numeric!")
  if (adj.ann.subgrp < 0) stop("The value adjusting the distance between a point and its corresponding subgroup label is not positive!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 5)) stop("The font size setups for labels or text should have five components only!")

  ################################################ 1. create subgroup data  #################################################################


  n.covari = length(covari.sel)
  lab.vars = names(dat)[covari.sel]                                                # set the names of the covariates which relates to the defined subgroup; if a covariate
  # are considered for multiple times, we make their name identical. (otherwise, the resulsting
  # names are like var, var.1, var.2 and so on.)

  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
  }

  for (i in 1: length(covari.sel)){
    cond = covari.sel == covari.sel[[i]]
    lab.vars[cond] = rep(lab.vars[i], length(which(cond == TRUE)))
  }
  cats.var = list()
  n.subgrp.tol = 0
  for (i in 1 : length(covari.sel)){
    cats.var[[i]] = names(table(dat[,covari.sel[i]]))
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])
  }

  cond = list()
  data.subgrp = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)
  k = 0
  for (i in 1 : length(covari.sel)) {
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      cond[[k]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
      ss.subgrp[k, k] = length(cond[[k]])
      data.subgrp[[k]] = dat[cond[[k]], ]
    }
  }

  k = n.subgrp.tol
  for (i in 1 : (n.subgrp.tol - 1) ){
    for (j in (i + 1) : (n.subgrp.tol) ){
      k = k + 1
      cond[[k]] = intersect(cond[[i]], cond[[j]])
      ss.subgrp[i, j] = length(cond[[k]])
      ss.subgrp[j, i] = length(cond[[k]])
    }
  }

  # create matrices for treatment size and standard error of MLE for subgroups and the full
  # population

  if (sum((dat$trt == "1")) == 0 | sum((dat$trt == "0")) == 0){
    treatment.mean.full = NA
    treatment.std.full = NA
    zscore.full = NA

  }else{

    if (outcome.type == "continuous"){

      model.int = lm(resp ~ trt,  data = dat)
      model.sum = summary(model.int)
      treatment.mean.full = model.sum$coefficients[2, 1]
      treatment.std.full = model.sum$coefficients[2, 2]
      zscore.full = treatment.mean.full / treatment.std.full

    }else if (outcome.type == "binary"){

      model.int = glm(resp ~ trt, family = "binomial", data = dat)
      model.sum = summary(model.int)
      treatment.mean.full = model.sum$coefficients[2, 1]
      treatment.std.full = model.sum$coefficients[2, 2]
      zscore.full = treatment.mean.full / treatment.std.full

    }else if (outcome.type == "survival"){

      model.int = survival::coxph(Surv(time, status) ~ trt, data = dat)
      model.sum = summary(model.int)
      treatment.mean.full = model.sum$coef[1, 1]
      treatment.std.full = model.sum$coef[1, 3]
      zscore.full = treatment.mean.full / treatment.std.full

    }
  }

  treatment.mean = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  treatment.std  = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  zscore = matrix(0, nrow = n.subgrp.tol, ncol = 1)
  for (i in 1 : (n.subgrp.tol)){
    if (sum((data.subgrp[[i]]$trt == "1")) == 0 | sum((data.subgrp[[i]]$trt == "0")) == 0){
      treatment.mean[i] = NA
      treatment.std[i] = NA
      zscore[i] = NA

    }else{

      if (outcome.type == "continuous"){

        model.int =  lm(resp ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
        treatment.std[i] = model.sum$coefficients[2, 2]
        zscore[i] = treatment.mean[i] / treatment.std[i]                             # calculate the z-score of the subgroup effect size relative
        # to the standardized error of the full population effect size
        # estimator
      }else if (outcome.type == "binary"){

        model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]                                # record subgroup effect size
        treatment.std[i] = model.sum$coefficients[2, 2]
        zscore[i] = treatment.mean[i] / treatment.std[i]                        # calculate the z-score of the subgroup effect size relative
        # to the standardized error of the full population effect size
        # estimator
      }else if (outcome.type == "survival"){

        model.int = survival::coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.std[i] = model.sum$coef[1, 3]
        zscore[i] = treatment.mean[i] / treatment.std[i]
      }
    }

  }

  lab.subgrp = vector()
  lab.subgrp2 = vector()
  k = 0
  for (i in 1: length(covari.sel)){
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      lab.subgrp[k] = paste("(", LETTERS[i], j, ") ", lab.vars[i], " = ", cats.var[[i]][j], sep = "")
      lab.subgrp2[k]  = paste(LETTERS[i], j, sep = "")
    }
  }
  dimnames(treatment.mean) = list(c(lab.subgrp), c("mean") )
  dimnames(treatment.std) = list(c(lab.subgrp), c("std") )
  dimnames(zscore) = list(c(lab.subgrp), c("zscore") )
  zscore = (treatment.mean - treatment.mean.full)/treatment.std
  ################################################ 2. produce a graph  #################################################################
  par(mar = c(4,4,1,1))
  if (is.null(range.v)){
    y.lim.max = ceiling(max(zscore, na.rm = TRUE))
    y.lim.min = floor(min(zscore, na.rm = TRUE))
    if (y.lim.max <= 2)  {y.lim.max = y.lim.max + 6}
    if (y.lim.min >= -2) {y.lim.min = y.lim.min -3}
  }else{
    y.lim.max = range.v[2]
    y.lim.min = range.v[1]
  }

  plot(0, 0,
       type='n',
       xlab = "",#lab.xy[[1]],
       ylab = "",#lab.xy[[2]],
       ylim = c(y.lim.min, y.lim.max),
       xlim = c(0,13.5), xaxt = 'n', yaxt='n', xaxs = "i",
       main = title, cex.lab = font.size[2], cex.main = font.size[1])

  mtext(text =  lab.xy[[2]], side = 2, line = 2)
  mtext(text =  lab.xy[[1]], side = 1, line = 3)
  expand.fac = 2                                                                   # the expansion factor of the original unit on the x-axis
  length.unit.xaxis = 5                                                            # the number of the units on the x-axis
  upper.bd = 2; lower.bd = -2
  x.lab.max.pos = round(max(1/c(treatment.std, treatment.std.full))) + 0.1 #ceiling(max(1/treatment.std))
  x.lab.max.pos = round(max(1/c(treatment.std, treatment.std.full))) + 1 #ceiling(max(1/treatment.std))
  y.lab.upp.pos = treatment.mean.full + upper.bd
  y.lab.low.pos = treatment.mean.full + lower.bd
  axis(1, at= seq(0, length.unit.xaxis * expand.fac, 1 * expand.fac),
       labels = round(seq(0, x.lab.max.pos, len = length.unit.xaxis + 1), 2),
       cex.axis = font.size[5])

  xy.current.pos = par("usr")
  legend(xy.current.pos[1], xy.current.pos[4], lab.subgrp, bty = "n", cex = font.size[3])

  # pos.esize.mean  = zscore.full                  # the y-coordinate of the full
  # pos.esize.lower = pos.esize.mean + lower.bd
  # pos.esize.upper = pos.esize.mean + upper.bd
  pos.esize.mean  = treatment.mean.full                  # the y-coordinate of the full
  pos.esize.lower = pos.esize.mean + lower.bd
  pos.esize.upper = pos.esize.mean + upper.bd

  yy1 = seq(pos.esize.mean, y.lim.min + treatment.mean.full, -2)
  yy2 = seq(pos.esize.mean, y.lim.max + treatment.mean.full,  2)
  lab = c(rev(yy1), yy2)
  lab = c(rep(NA, len = length(seq(y.lim.min, lower.bd - 1, 1))),
          c(lower.bd, NA, 0, NA, upper.bd),
          c(pos.esize.lower, NA, pos.esize.mean, NA, pos.esize.upper),
          rep(NA, len = length(seq(upper.bd +1, y.lim.max, 1))))
  # axis(2, at = round(seq(y.lim.min + zscore.full, y.lim.max + zscore.full, 2),2),
  #      las = 1,
  #      # labels = round(lab, 2),
  #      cex.axis = font.size[5])

  axis(2,
       at= seq(y.lab.low.pos - 1, y.lab.upp.pos + 1, 1)[c(2,4,6)],
       labels = c(NA, lower.bd, NA, 0, NA, upper.bd, NA)[c(2,4,6)],
       cex.axis = font.size[5] )


  x <- c(0, length.unit.xaxis * expand.fac, length.unit.xaxis * expand.fac, 0)
  y <- c(pos.esize.lower, pos.esize.lower, pos.esize.upper, pos.esize.upper)
  x = c(x, x[1])
  y = c(y, y[1])
  polygon(x, y,  col = "gray", border = NA)                                         # the confidence band (-2 to 2)

  x1 = seq(0, length.unit.xaxis * expand.fac, len=200)
  y1 = rep(pos.esize.mean, 200)
  lines(x1, y1)                                                                     # draw the central line
  y2 =rep(pos.esize.lower, 200)
  y3 =rep(pos.esize.upper, 200)
  lines(x1, y2, lty = 2)                                                            # draw the lower boundary
  lines(x1, y3, lty = 2)                                                            # draw the upper boundary

  lab.subgrp.pos.adj = 1/(adj.ann.subgrp * length.unit.xaxis * expand.fac) # 1/(5 * length.unit.xaxis * expand.fac) #1/50
  expand.fac.adj = expand.fac * length.unit.xaxis * 1/(x.lab.max.pos)
  points(1/treatment.std * expand.fac.adj,
         zscore,
         cex = font.size[4]/2, pch = 19)             # add the points for the subgroups
  text((1/treatment.std) * expand.fac.adj,# + 10*lab.subgrp.pos.adj  * expand.fac.adj,
       zscore + 10*lab.subgrp.pos.adj,
       labels = lab.subgrp2,  cex = font.size[4])      # the annotation for the points
  # points(1/treatment.std.full * expand.fac.adj,
  #        zscore.full,
  #        cex = font.size[4]/2, pch = 19)             # add the points for the subgroups
  # text((1/treatment.std.full) * expand.fac.adj,# + 10*lab.subgrp.pos.adj  * expand.fac.adj,
  #      zscore.full + 10*lab.subgrp.pos.adj,
  #      labels = "Full",  cex = font.size[4])      # the annotation for the points

  ### draw arcs for displaying effect sizes

  angle = vector()
  zscore.diff = zscore - treatment.mean.full
  len.pt = dim(zscore)[1]
  for (i in 1: len.pt){
    angle[i] = atan( zscore.diff[i]/ ((1/treatment.std[i] * expand.fac.adj)))        # calculate the angle between two vectors
  }

  if (sum(angle>0) != length(angle)){                                               # when the zscores of subgroups are partially larger and less than the zscore of the full population

    axis.circle1.max = max(angle)                                                   # the difference between the maximal effect size and the nearest effect size unit
    axis.circle1.min = 0                                                            # the effect size for the full population

    x0 = 0; y0 = treatment.mean.full
    npts = 200
    r = ceiling(sqrt(pos.esize.mean^2 + ((length.unit.xaxis + 0.2)* expand.fac)^2)) - 0.1     # the radius of the outer circle

    theta <- seq(axis.circle1.min,  axis.circle1.max ,  len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)                                                                                # draw the cicular axis

    r1 = r + 0.1
    theta1 <- seq(axis.circle1.min,  axis.circle1.max, len = 7)
    esize.unit <- seq(treatment.mean.full,  max(treatment.mean), len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1:(length(theta1))){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)    # drew the ticks for the unit used in effect size
      if(i!=1){ #Exclude the first one as it overlaps with the second arc
      text( x0 + r1 * cos(theta1[i]) + 0.5,
            y0 + r1 * sin(theta1[i]),
            labels = esize.unit[i],
            cex = font.size[5], adj = 0.35)}  # add the corresponding effect size
    }

    r1 = r - 0.15
    angle.pos = angle[angle>0]
    for (i in 1: length(angle.pos)){
      x1 = x0 + seq(r * cos(angle.pos[i]), r1 * cos(angle.pos[i]), len=10)
      y1 = y0 + seq(r * sin(angle.pos[i]), r1 * sin(angle.pos[i]), len=10)
      lines(x1, y1, col = "red")                                                               # drew the lines for the unit used in effect size
    }

    axis.circle2.max = 0                                                                       # the effect size for the full population
    axis.circle2.min = min(angle)                                                              # the difference between the minmal effect size and the nearest effect size unit

    x0 = 0; y0 = treatment.mean.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + ((length.unit.xaxis + 0.9)* expand.fac)^2)) - 0.1     # the radius of the outer circle

    theta <- seq(axis.circle2.min,  axis.circle2.max ,  len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)     # draw the cicular axis

    r1 = r + 0.1
    theta1 <- seq(axis.circle2.min,  axis.circle2.max, len = 7)
    esize.unit <- seq(min(treatment.mean), treatment.mean.full, len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5,
            y0 + r1 * sin(theta1[i]),
            labels = esize.unit[i],
            cex = font.size[5], adj = 0.35)  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.neg = angle[angle<0]
    for (i in 1: length(angle.neg)){
      x1 = x0 + seq(r * cos(angle.neg[i]), r1 * cos(angle.neg[i]), len=10)
      y1 = y0 + seq(r * sin(angle.neg[i]), r1 * sin(angle.neg[i]), len=10)
      lines(x1, y1, col = "red")                                                               # drew the lines for the unit used in effect size
    }

  }else if (sum(angle>0) == length(angle)){                                                    # when all zscores of subgroups are larger than the zscore of the full population

    axis.circle1.max = max(angle)                                                              # the difference between the maximal effect size and the nearest effect size unit
    axis.circle1.min = 0                                                                       # the effect size for the full population

    x0 =0; y0= treatment.mean.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + (5.2* expand.fac) ^2)) - 0.1                           # the radius of the outer circle

    theta <- seq(axis.circle1.min,  axis.circle1.max ,  len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)                                                                                # draw the cicular axis

    r1 = r + 0.1
    theta1 <- seq(axis.circle1.min,  axis.circle1.max, len = 7)
    esize.unit <- seq(treatment.mean.full,  max(treatment.mean), len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5,
            y0 + r1 * sin(theta1[i]),
            labels = esize.unit[i],
            cex = font.size[5], adj = 0.35)  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.pos = angle[angle>0]
    for (i in 1: length(angle.pos)){
      x1 = x0 + seq(r * cos(angle.pos[i]), r1 * cos(angle.pos[i]), len=10)
      y1 = y0 + seq(r * sin(angle.pos[i]), r1 * sin(angle.pos[i]), len=10)
      lines(x1, y1, col = "red")                                                                # drew the lines for the unit used in effect size
    }

  }else if (sum(angle<0) == length(angle)){                                                     # when all zscores of subgroups are less than the zscore of the full population

    axis.circle2.max = 0                                                                        # the effect size for the full population
    axis.circle2.min = min(angle)                                                               # the difference between the minmal effect size and the nearest effect size unit

    x0 =0; y0= treatment.mean.full
    npts =200
    r = ceiling(sqrt(pos.esize.mean^2 + (5.9* expand.fac) ^2)) - 0.1                            # the radius of the outer circle

    theta <- seq(axis.circle2.min,  axis.circle2.max , len = 200)
    xh <-  r * cos(theta)
    yh <-  r * sin(theta)
    x  <- x0 + xh
    y  <- y0 + yh
    lines(x, y)     # draw the cicular axis

    r1 = r + 0.1
    theta1 <- seq(axis.circle2.min,  axis.circle2.max, len = 7)
    esize.unit <- seq(min(treatment.mean), treatment.mean.full, len = length(theta1))
    esize.unit = round(esize.unit,2)
    for (i in 1: length(theta1)){
      x1 = x0 + seq(r * cos(theta1[i]), r1 * cos(theta1[i]), len=10)
      y1 = y0 + seq(r * sin(theta1[i]), r1 * sin(theta1[i]), len=10)
      lines(x1, y1)                                                                                                # drew the lines for the unit used in effect size
      text( x0 + r1 * cos(theta1[i]) + 0.5, y0 + r1 * sin(theta1[i]), labels = esize.unit[i], cex = font.size[5])  # add the corresponding effect size
    }

    r1 =r - 0.15
    angle.neg = angle[angle<0]
    for (i in 1: length(angle.neg)){
      x1 = x0 + seq(r * cos(angle.neg[i]), r1 * cos(angle.neg[i]), len=10)
      y1 = y0 + seq(r * sin(angle.neg[i]), r1 * sin(angle.neg[i]), len=10)
      lines(x1, y1, col = "red")                                                               # drew the lines for the unit used in effect size
    }

  }
  box()
}
