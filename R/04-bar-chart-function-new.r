#' barchart for subgroup effect size
#'
#' this function produces a bar chart showing the treatment effect size of pairwise subgroups defined by the categories of two
#' covariates. Also, it prints out the minimum and maximum of the treatment effect size on the console. Note that each bar has a width
#' which is proportional to the ratio of the corresponding sample size to the full size. In addition, the function uses log odd ratio
#' and log hazard ratio for displaying subgroup effect sizes in binary and survival data, respectively.
#'
#'@param  dat:          a data set
#'@param  covari.sel:   a vector of indices of the two covariates
#'@param  trt.sel:      a covariate index specifying the treatment code
#'@param  resp.sel:     a covariate index specifying the response variable
#'@param  outcome.type: a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#'@param  font.size:    a vector specifying the size of labels and text; the first element is for the main title; the second element is for the covariates
#'              labels and the y-axis label; the third is for the category labels; the forth is for the unit label of the y axis.
#'@param  title:        a string specifying the main title.
#'@param  lab.y:        a string specifying the y-axis label.
# eg.1          main.title = paste("Treatment effect sizes across subgroups", sep = "");
#               lab.y.title = paste("Effect size");
#               barcht(dat = dat, covari.sel = c(4, 6), trt.sel = 2, resp.sel = 1, outcome.type = "continuous", title = main.title,
#               lab.y = lab.y.title)
#
# eg.2          main.title = paste("Treatment effect sizes across subgroups", sep = "");
#               lab.y.title = paste("Effect size (log odd ratio)");
#               barcht(dat = dat2, covari.sel = c(2, 3), trt.sel = 4, resp.sel = 5, outcome.type = "binary", title = main.title,
#               lab.y = lab.y.title)
#
# eg.3          main.title = paste("Treatment effect sizes across subgroups", sep = "");
#               lab.y.title = paste("Treatment effect size (log hazard ratio)");
#               barcht(dat = dat3, covari.sel = c(6, 7), trt.sel = 1, resp.sel = c(4, 3), outcome.type = "survival", title = main.title,
#               lab.y = lab.y.title)
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
#' @import grid
#' @import graphics
plot_barchart <- function(dat, covari.sel, trt.sel, resp.sel,
                          outcome.type, font.size = c(15, 12, 10, 0.6),
                          title = NULL, lab.y = NULL, effect = "RMST", time = 50, sig.dig = 0)
{

  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  for (i in 1 : length(covari.sel)) if (!(is.factor(dat[,covari.sel[i]]))) stop("The variables for defining subgroups are not categorical!")
  if (length(covari.sel) > 2) stop("This function only considers 2 covariates at most for defining subgroups!")

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

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 4)) stop("The font size setups for labels or text should have four components only!")

  ################################################ 1. create subgroup data  #################################################################

  lab.vars = names(dat)[covari.sel]                                                             # set the names of the covariates which relates to the defined subgroup; if a covariate
                                                                                                # are considered for multiple times, we make their name identical. (otherwise, the resulsting
                                                                                                # names are like var.1, var.2 and so on.)

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
  cats.var1 = names(table(dat[,covari.sel[1]]))                                                 # the names of categories of the selected first cavariate
  cats.var2 = names(table(dat[,covari.sel[2]]))                                                 # the names of categories of the selected second cavariate
  n.subgrp.var1 = dim(table(dat[,covari.sel[1]]))                                               # the number of categories for the first covariate (placed in the bottom of the figure)
  n.subgrp.var2 = dim(table(dat[,covari.sel[2]]))                                               # the number of categories for the second covariate (placed in the left of the figure)
  n.subgrp.tol = n.subgrp.var1 * n.subgrp.var2                                                  # the total number of subgroups
  if(max(n.subgrp.var1, n.subgrp.var2) > 10) stop("This function only consider 10 categories at most for a variable!")

  idx1 = list()
  idx2 = list()
  for (i in 1 : n.subgrp.var1 ) idx1[[i]] = which((dat[, covari.sel[1]] == cats.var1[i])  == T )
  for (i in 1 : n.subgrp.var2 ) idx2[[i]] = which((dat[, covari.sel[2]] == cats.var2[i])  == T )

  idx.subgrp = list()                                                                           # the index set of the subgroups
  data.subgrp = list()                                                                          # the data set of the subgroups
  ss.subgrp = matrix(0, nrow = n.subgrp.var2, ncol =n.subgrp.var1)                              # the data set of the mariginal subgroups
  for (j in 1 : n.subgrp.var1){
    for (i in 1 : n.subgrp.var2){
      k = i + (j - 1) * n.subgrp.var2
      idx.subgrp[[k]] =  intersect(idx1[[j]], idx2[[i]])
      data.subgrp[[k]] =  dat[idx.subgrp[[k]], ]
      ss.subgrp[i, j] = dim(data.subgrp[[k]])[1]
    }
  }

  treatment.mean = matrix(rep(0, n.subgrp.tol), nrow = n.subgrp.var2, ncol = n.subgrp.var1)     # a matrix storing the estimates of effect sizes for all subgroups
  treatment.std = matrix(rep(0, n.subgrp.tol), nrow = n.subgrp.var2, ncol = n.subgrp.var1)      # a matrix storing the stand errors of the effect sizes estimates for all subgroups
  for (j in 1 : n.subgrp.var1){
    for (i in 1 : n.subgrp.var2){
      k = i + (j - 1) * n.subgrp.var2
      if (sum((data.subgrp[[k]]$trt == "1")) == 0 | sum((data.subgrp[[k]]$trt == "0")) == 0){
        treatment.mean[i,j] = NA
        treatment.std[i,j] = NA
      }else{

        if (outcome.type == "continuous"){
          model.int = lm(resp ~ trt,  data = data.subgrp[[k]])
          model.sum = summary(model.int)
          treatment.mean[i,j] = model.sum$coefficients[2, 1]
          treatment.std[i,j] = model.sum$coefficients[ 2, 2]
        }else if (outcome.type == "binary"){
          model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[k]])
          model.sum = summary(model.int)
          treatment.mean[i,j] = model.sum$coefficients[2, 1]
          treatment.std[i,j] = model.sum$coefficients[ 2, 2]
        }else if (outcome.type == "survival"){
          if (effect == "HR"){
            model.int = survival::coxph(Surv(time, status) ~ trt, data = data.subgrp[[k]])
            model.sum = summary(model.int)
            treatment.mean[i,j] = model.sum$coef[1, 1]
            treatment.std[i,j] = model.sum$coef[1, 3]
          }
          if (effect == "RMST"){
            dat.subgr.i = data.subgrp[[k]]
            rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                  arm = dat.subgr.i$trt, tau = time)
            treatment.mean[i,j] = rmst$unadjusted.result[1,1]
            treatment.std[i,j]  = rmst$unadjusted.result[1,3]
          }
        }
      }
    }
  }

  lab.subgrp.row = cats.var2                                                                    #  the label of the second covariate (located on the top)
  lab.subgrp.col = cats.var1                                                                    #  the label of the first covariate (located on the left-bottom)
  lab.level = rep(0, max(n.subgrp.var1, n.subgrp.var2))
  for (i in 1 : max(n.subgrp.var1, n.subgrp.var2)){
    lab.level[i] = paste("Level", i, sep ="")
  }

  dimnames(treatment.mean) = list(lab.subgrp.row, lab.subgrp.col )
  dimnames(treatment.std) = list(lab.subgrp.row, lab.subgrp.col )
  dimnames(ss.subgrp) = list(lab.subgrp.row, lab.subgrp.col )

  ################################################ 2. produce a graph  #################################################################

  grid.newpage()

  ##########  middle-top cell (set title)
  vp <- viewport(x = 0.1 + 0.02, y = 0.91, width = 0.85, height = 0.05, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(title, gp = gpar(fontsize = font.size[1], fontface = 2))
  upViewport()
  vp <- viewport(x = 0.1 + 0.02, y = 0.86, width = 0.85, height = 0.05, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(lab.vars[2], gp = gpar(fontsize = font.size[2], fontface = 2))
  upViewport()

  ##   left-middle gap (set axis)
  vp <- viewport(x = 0.1, y = 0.02, width = 0.02, height = 0.8, just = c("left", "bottom"))
  pushViewport(vp)

  vp <- viewport(x = 0, y = 0.2, width = 1, height = (1 - 0.2), just = c("left", "bottom"))
  pushViewport(vp)

  breaks <- seq(min(c(treatment.mean), na.rm = T) - 1e-8,
                max(c(treatment.mean), na.rm = T) + 1e-8,
                length.out = 100)
  if (sign(breaks[1]) == sign(breaks[100])){
    if (sign(breaks[1]) >0){

      # when all subgroups have positive effect sizes
      if(outcome.type != "survival"){
        axis.min = 0; axis.max = breaks[100] + max(treatment.std, na.rm = T)
      }else{
        axis.min = 0; axis.max = breaks[100]
      }
    }else{

      # when all subgroups have negative effect sizes
      # axis.min = breaks[1] - max(treatment.std, na.rm = T); axis.max = 0

      if(outcome.type != "survival"){
        axis.min = breaks[1] - max(treatment.std, na.rm = T); axis.max = 0
      }else{
        axis.min = breaks[1]; axis.max = 0
      }
    }
  }else{
    treatment.std[is.na(treatment.std)] = 0
    if(outcome.type != "survival"){
      axis.min = min(treatment.mean - treatment.std, na.rm = T) - 1e-8
      axis.max = max(treatment.mean + treatment.std, na.rm = T) + 1e-8
      axis.max = ceiling(max(axis.max, abs(axis.min))*(10^sig.dig))/(10^sig.dig)
      axis.min = -axis.max
    }else{

      axis.min = min(treatment.mean, na.rm = T) - 1e-8
      axis.max = max(treatment.mean, na.rm = T) + 1e-8
      axis.max = ceiling(max(axis.max, abs(axis.min))*(10^sig.dig))/(10^sig.dig)
      axis.min = -axis.max
    }
  }

  grid.yaxis(seq(0, 1,  by = 0.1),
             vp    = viewport(y=0.5),
             label = round(seq(axis.min, axis.max, len = 11), 2),
             gp    = gpar(cex = font.size[4]),
             edits = gEdit(gPath ="labels", just = "center", rot=90))

  upViewport(2)

  ## vertical line (Left-side, set labels)

  vp <- viewport(x = 0, y = 0.02, width = 0.1, height = 0.8, just = c("left", "bottom"))
  pushViewport(vp)
  vp <- viewport(x = 0, y = 0, width = 1, height = 0.2, just = c("left", "bottom"))
  pushViewport(vp)

  grid.text(lab.vars[1], gp = gpar(fontsize = font.size[2], fontface = 2), rot = 90)            # place the label of the first covariate
  upViewport(2)

  vp <- viewport(x = 0, y = 0.02 + 0.8*0.2, width = 0.1, height = 0.8*(1 - 0.2), just = c("left", "bottom"))
  pushViewport(vp)
  vp <- viewport(x = 0, y = 0, width = 1, height= 1, just = c("left", "bottom"))
  pushViewport(vp)

  grid.text(lab.y, gp = gpar(fontsize = font.size[2], fontface = 2), vjust = 0, rot = 90)                  # place the label of the y-axis

  y.pos.zero = 0.8 / (axis.max - axis.min) * (0 - axis.min)                                     # set the y-coordinate of the buttom or the top of the bars
  if (sign(breaks[1]) != sign(breaks[100])){
    grid.lines(c(0.91,1), c(y.pos.zero/0.8, y.pos.zero/0.8), gp = gpar(col = "black"))
    vp <- viewport(x = 0.73, y = y.pos.zero/0.8 - 0.01, width = 0.2, height = 0.02, just = c("left", "bottom"))
    pushViewport(vp)
    # grid.text("0", just = "bottom", gp = gpar(cex = font.size[4]), rot = 90)                    # place the label of the second covariate
    upViewport()
  }
  upViewport(2)

  ############ middle area (middle)

  vp <- viewport(x=0.1 + 0.02, y =0.02 + 0.8 *0.2, width=0.85, height=0.8 * (1 - 0.2), just = c("left", "bottom"))
  pushViewport(vp)
  # grid.rect(gp = gpar(fill= "gray89", col = NA))

  length.ann.y = length(seq(axis.min, axis.max, len = 6))
  for (i in 1 : (length.ann.y - 2)){
    grid.lines(c(0,1), c(i * 1/(6-1), i * 1/(6-1)), gp=gpar(col = "gray95", lty = "solid", lwd = 2.5))   # set lightgray lines
  }
  for (i in 1 : (length.ann.y - 1)){
    st = 1/(2 * (6-1)) + (i - 1) * 1/(6-1)
    grid.lines(c(0,1), c(st, st), gp=gpar(col = "gray95"))                                               # set lightgray lines
  }

  ## calculate the width of the bars

  width.bar = ss.subgrp/sum(ss.subgrp) * 0.85 #sqrt(ss.subgrp)/sum(sqrt(ss.subgrp)) * 0.85
  width.between.bar = (1 - 0.85)/(n.subgrp.tol - 1)

  y.pos.zero = 0.8 / (axis.max - axis.min) * (0 - axis.min)                                              # set the y-coordinate of the buttom or the top of the bars
  grid.lines(c(0,1), c(y.pos.zero/0.8, y.pos.zero/0.8), gp = gpar(col = "black"))
  width.sum = c(0, c(t(width.bar)) )                                                                     # set the summuration of the bar widths, which is for calculting the x-coordinate of each bar

  ## draw bars

  col.bar = rep("gray25", 10)
  col.bar = c("gray25", "gray50", "gray75")
  for (i in 1 : n.subgrp.tol){
    y.pos.bar = 0.8 / (axis.max - axis.min) * abs(t(treatment.mean)[i])
    if (t(treatment.mean)[i] > 0){
      just.setting =  c("left", "bottom")
    }else{
      just.setting =  c("left", "top")
    }
    vp <- viewport(x= sum(width.sum[1:i]) + (i - 1) * width.between.bar, y = y.pos.zero/0.8, width = t(width.bar)[i],
                   height= y.pos.bar/0.8, just = just.setting)
    pushViewport(vp)
    bar.col = vector()
    if (i %% n.subgrp.var1 == 0){
      bar.col[i] = col.bar[n.subgrp.var1]}
    else{
      bar.col[i] = col.bar[i%% n.subgrp.var1]
    }
    grid.rect(gp = gpar(fill= bar.col[i]))
    upViewport()                                                                                # back to the second viewport (the middle area)
  }

  ## draw error bars-----
  mat.fa = matrix(0, nrow = n.subgrp.tol, ncol = n.subgrp.tol)
  mat.fa[lower.tri(mat.fa)] = 1
  diag(mat.fa) = 1/2
  x.mid.pos=  mat.fa %*% matrix(c(t(width.bar)), ncol = 1)                                      # for calculating the x-coordinate of the error bars
  for (i in 1 : n.subgrp.tol){
    y.pos.bar = 0.8 / (axis.max - axis.min) * abs(t(treatment.mean)[i])
    y.pos.std.bar = 0.8 / (axis.max - axis.min) * abs(t(treatment.std)[i])

    if (t(width.bar)[i] == 0) {
      std.bar.width = 0
    }else{
      std.bar.width = 0.01
    }
    if(outcome.type != "survival"){
    if (t(treatment.mean)[i] > 0){
      grid.lines(c(x.mid.pos[i] + (i - 1) * width.between.bar, x.mid.pos[i] + (i - 1) * width.between.bar),
                 c(y.pos.zero/0.8 + y.pos.bar/0.8, y.pos.zero/0.8 + y.pos.bar/0.8+ y.pos.std.bar/0.8 ))                        # draw an error bar's stem
      grid.lines(c(x.mid.pos[i] + (i - 1) * width.between.bar - std.bar.width, x.mid.pos[i] + (i - 1) * width.between.bar + std.bar.width),
                 c(y.pos.zero/0.8 + y.pos.bar/0.8+ y.pos.std.bar/0.8, y.pos.zero/0.8 + y.pos.bar/0.8+ y.pos.std.bar/0.8 ))     # draw an error bar's top or bottom
    }else{
      grid.lines(c(x.mid.pos[i] + (i - 1) * width.between.bar, x.mid.pos[i] + (i - 1) * width.between.bar),
                 c(y.pos.zero/0.8 - y.pos.bar/0.8, y.pos.zero/0.8 - y.pos.bar/0.8 - y.pos.std.bar/0.8 ))                       # draw an error bar's stem
      grid.lines(c(x.mid.pos[i] + (i - 1) * width.between.bar - std.bar.width, x.mid.pos[i] + (i - 1) * width.between.bar + std.bar.width),
                 c(y.pos.zero/0.8 - y.pos.bar/0.8 - y.pos.std.bar/0.8, y.pos.zero/0.8 - y.pos.bar/0.8 - y.pos.std.bar/0.8 ))   # draw an error bar's top or bottom
    }
    }
  }
  upViewport()

  # back to the initial viewport

  ############ horizontal area (middle-bottom, set labels)

  vp <- viewport(x = 0.1 + 0.02, y = 0.02, width = 0.85, height = 0.8 * 0.2, just = c("left", "bottom"))
  pushViewport(vp)

  for (i in 1 : n.subgrp.tol){
    vp <- viewport(x= sum(width.sum[1:i]) + (i - 1) * width.between.bar, y = 0, width = t(width.bar)[i], height= 1, just = c("left", "bottom"))
    pushViewport(vp)
    lab.bot = vector()
    if (i %% n.subgrp.var1 == 0){
      lab.bot[i] = lab.subgrp.col[n.subgrp.var1]}
    else{
      lab.bot[i] = lab.subgrp.col[i%% n.subgrp.var1]
    }
    grid.text(lab.bot[i], gp = gpar(fontsize = font.size[3]), rot = 90)
    upViewport()                                                                                 # back to the second viewport (the middle area)
  }

  upViewport()                                                                                   # back to the initial viewport

  ############ horizontal area (middle-top, set labels)

  vp <- viewport(x= 0.1 + 0.02, y =0.83, width=0.85, height=0.02, just = c("left", "bottom"))
  pushViewport(vp)

  x.width.pos = vector()                                                                         # calculate the width of the labels for the categories of the second covariate
  for (i in 1 : n.subgrp.var2) x.width.pos[i] = sum(t(width.bar)[1:((i-1)*n.subgrp.var1)])
  x.width.pos[1] = 0
  for (i in 1 : n.subgrp.var2){
    st = 1 + (i - 1) * n.subgrp.var1
    se = n.subgrp.var1 * i
    vp <- viewport(x = x.width.pos[i] + (i - 1) * n.subgrp.var1 * width.between.bar, y = 0,
                   width = sum(t(width.bar)[st:se]) + (n.subgrp.var1 - 1) * width.between.bar,
                   height = 1, just = c("left", "bottom"))
    pushViewport(vp)
    grid.lines(c(0,0.1), c(1/2, 1/2))                                                           #draw the left middle-hyphen (for categories of var2)
    grid.lines(c((1 - 0.1),1), c(1/2, 1/2))                                                     #draw the right middle-hyphen (for categories of var2)
    grid.lines(c(0,0), c(0, 1))                                                                 #draw the left bar (for categories of var2)
    grid.lines(c(1,1), c(0, 1))                                                                 #draw the right bar (for categories of var2)
    grid.text(lab.subgrp.row[i], gp = gpar(fontsize = font.size[3]))
    upViewport()
  }
  upViewport()
}
