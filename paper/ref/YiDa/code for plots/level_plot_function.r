#' level plot for subgroup effect size
#'
#' this function produces a level plot showing the treatment effect size of pairwise subgroups and marginal subgroups defined by the
#' categories of two covariates. Also, it prints out the minimum and maximum of the treatment effect size on the console so as to set
#' an approapriate range for effect size on the colour strip . Note that there are two types of graphical display; whether show subgroup
#' sample size by rectangles with different sizes (proportional to the ratio of sample size to the full size) or not. In addition, the
#' function uses log odd ratio and log hazard ratio for displaying subgroup effect sizes in binary and survival data, respectively.
#'
#' @param dat:          a data set
#' @param covari.sel:   a vector of indices of the two covariates
#' @param trt.sel:      a covariate index which specifies the treatment code
#' @param resp.sel:     a covariate index which specifies the response variable
#' @param outcome.type: a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#' @param ss.rect:      a logical operator displaying the rectangles for subgroup sample sizes if TRUE
#' @param range.strip:  a vector with two elements specifying the range of treatment effect size for display
#' @param n.brk:        a number specifying the number of the points dividing the range of the argument "range.strip".
#' @param font.size:    a vector specifying the size of labels and text; the first element is for the main title; the second element is
#'                for the covariates labels and the colour strip label; the third is for the category labels of the first and second
#'                covariates; the fourth is for the text in the middle cells; the fifth is for the unit label on the colour strip.
#' @param title:        a string specifying the main title.
#' @param strip:        a string specifying the title of the colour strip.
#
# eg.1          main.title = paste("Treatment effect sizes across subgroups (N = 1000)", sep = "");
#               strip.title = paste("Treatment effect size");
#               lvplt(dat = dat, covari.sel = c(4, 6), trt.sel = 2, resp.sel = 1, outcome.type = "continuous", ss.rect=TRUE, title = main.title,
#               strip = strip.title)
#
# eg.2          main.title = paste("Treatment effect sizes across subgroups (N = 2985)", sep = "");
#               strip.title = paste("Treatment effect size (log odd ratio)");
#               lvplt(dat = dat2, covari.sel = c(2, 3), trt.sel = 4, resp.sel = 5, outcome.type = "binary", ss.rect=TRUE, title = main.title,
#               strip = strip.title)
#
# eg.3          main.title = paste("Treatment effect sizes across subgroups (N = 686)", sep = "");
#               strip.title = paste("Treatment effect size (log hazard ratio)");
#               lvplt(dat = dat3, covari.sel = c(6, 7), trt.sel = 1, resp.sel = c(4,3), outcome.type = "survival", ss.rect=TRUE, title = main.title,
#               strip = strip.title)
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
lvplt <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type, ss.rect=FALSE, range.strip=c(-6, 6), n.brk = 13,
                  font.size = c(15, 12, 0.8, 15, 0.6), title = NULL, strip = NULL)
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

  if (!(is.logical(ss.rect))) stop("The arugument of displaying squares is not logical!")

  if (!(is.numeric(range.strip))) stop("The argument about the range for displaying effect sizes is not a numeric vector!")
  if (length(range.strip) > 2) stop("The range for displaying effect sizes should be specified by two values!")
  if (range.strip[1] > range.strip[2]) stop("The range for displaying effect sizes is not specified correctly (the former compoent should be
                                            smaller than the latter)!")

  if (!(is.numeric(n.brk))) stop("The argument about the break points in the range for displaying effect sizes is not numeric!")
  if (length(n.brk) > 1) stop("The number of the break points in the range for displaying effect sizes should be greater than 1!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  if (!(length(font.size) == 5)) stop("The font size setups for labels or text should have five components only!")

  ################################################ 1. create subgroup data  #################################################################

  library(grid)
  library(survival)

  lab.vars = names(dat)[covari.sel]                 # set the names of the covariates which relates to the defined subgroup; if a covariate
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
  cats.var1 = names(table(dat[,covari.sel[1]]))     # the names of categories of the selected first cavariate
  cats.var2 = names(table(dat[,covari.sel[2]]))     # the names of categories of the selected second cavariate
  n.subgrp.var1 = dim(table(dat[,covari.sel[1]]))   # the number of levels for the first covariate (placed in the bottom of the figure)
  n.subgrp.var2 = dim(table(dat[,covari.sel[2]]))   # the number of levels for the second covariate (placed in the left of the figure)
  n.subgrp.tol = n.subgrp.var1 * n.subgrp.var2      # the total number of subgroups

  idx1 = list()
  idx2 = list()
  for (i in 1 : n.subgrp.var1 ) idx1[[i]] = which((dat[, covari.sel[1]] == cats.var1[i])  == T )
  for (i in 1 : n.subgrp.var2 ) idx2[[i]] = which((dat[, covari.sel[2]] == cats.var2[i])  == T )

  idx.subgrp = list()                               # the index set of the subgroups
  data.subgrp = list()                              # the data set of the subgroups
  data.mar.subgrp1 = list()                         # the data set of the mariginal subgroups (for variance 1, listed columnwisely)
  data.mar.subgrp2 = list()                         # the data set of the mariginal subgroups (for variance 2, listed rowwisely)
  ss.subgrp = matrix(0, nrow = n.subgrp.var2, ncol =n.subgrp.var1)  # the data set of the mariginal subgroups
  for (j in 1 : n.subgrp.var1){
    for (i in 1 : n.subgrp.var2){
      k = i + (j - 1) * n.subgrp.var2
      idx.subgrp[[k]] =  intersect(idx1[[j]], idx2[[i]])
      data.subgrp[[k]] =  dat[idx.subgrp[[k]], ]
      ss.subgrp[i, j] = dim(data.subgrp[[k]])[1]

      data.mar.subgrp2[[i]] = dat[idx2[[i]], ]
    }
    data.mar.subgrp1[[j]] = dat[idx1[[j]], ]
  }

  treatment.mean = matrix(rep(0, n.subgrp.tol), nrow = n.subgrp.var2, ncol = n.subgrp.var1)
  treatment.mean.mar = matrix(rep(0, max(n.subgrp.var1, n.subgrp.var2)*2 ), nrow = max(n.subgrp.var1, n.subgrp.var2), ncol = 2)
  for (j in 1 : n.subgrp.var1){
    for (i in 1 : n.subgrp.var2){
      k = i + (j - 1) * n.subgrp.var2
      if (sum((data.subgrp[[k]]$trt == "1")) == 0 | sum((data.subgrp[[k]]$trt == "0")) == 0){
        treatment.mean[i,j] = NA
      }else{
        if (outcome.type == "continuous"){
          model.int = lm(resp ~ trt,  data = data.subgrp[[k]])
          model.sum = summary(model.int)
          treatment.mean[i,j] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "binary"){
          model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[k]])
          model.sum = summary(model.int)
          treatment.mean[i,j] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "survival"){
          model.int = coxph(Surv(time, status) ~ trt, data = data.subgrp[[k]])
          model.sum = summary(model.int)
          treatment.mean[i,j] = model.sum$coef[1, 1]
        }
      }

      if (sum((data.mar.subgrp2[[i]]$trt == "1")) == 0 | sum((data.mar.subgrp2[[i]]$trt == "0")) == 0){
        treatment.mean.mar[i,2] = NA
      }else{
        if (outcome.type == "continuous"){
          model.int = lm(resp ~ trt,  data = data.mar.subgrp2[[i]])
          model.sum = summary(model.int)
          treatment.mean.mar[i,2] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "binary"){
          model.int = glm(resp ~ trt,  family = "binomial", data = data.mar.subgrp2[[i]])
          model.sum = summary(model.int)
          treatment.mean.mar[i,2] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "survival"){
          model.int = coxph(Surv(time, status) ~ trt, data = data.mar.subgrp2[[i]])
          model.sum = summary(model.int)
          treatment.mean.mar[i,2] = model.sum$coef[1, 1]
        }
      }
    }

    if (sum((data.mar.subgrp1[[j]]$trt == "1")) == 0 | sum((data.mar.subgrp1[[j]]$trt == "0")) == 0){
      treatment.mean.mar[j,1] = NA
    }else{
      if (outcome.type == "continuous"){
        model.int = lm(resp ~ trt,  data = data.mar.subgrp1[[j]])
        model.sum = summary(model.int)
        treatment.mean.mar[j,1] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "binary"){
        model.int = glm(resp ~ trt,  family = "binomial", data = data.mar.subgrp1[[j]])
        model.sum = summary(model.int)
        treatment.mean.mar[j,1] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "survival"){
        model.int = coxph(Surv(time, status) ~ trt, data = data.mar.subgrp1[[j]])
        model.sum = summary(model.int)
        treatment.mean.mar[j,1] = model.sum$coef[1, 1]
      }
    }
  }

  lab.subgrp.col = cats.var1
  lab.subgrp.row = cats.var2
  lab.level = rep(0, max(n.subgrp.var1, n.subgrp.var2))
  for (i in 1 : max(n.subgrp.var1, n.subgrp.var2)) lab.level[i] = paste("Level", i, sep ="")

  dimnames(treatment.mean) = list(lab.subgrp.row, lab.subgrp.col )
  dimnames(treatment.mean.mar) = list(lab.level, lab.vars )
  dimnames(ss.subgrp) = list(lab.subgrp.row, lab.subgrp.col )

  cat("The minimum of treatment effect sizes is", c(min(treatment.mean, treatment.mean.mar)), "\n")
  cat("The maximum of treatment effect sizes is", c(max(treatment.mean, treatment.mean.mar)), "\n")

  ################################################ 2. produce a graph  #################################################################

  dev.new(width=10,height=10,noRStudioGD = TRUE)

  ##########  middle cell

  #####   middle-bottom gap
  vp <- viewport(x = 0.15 + 0.0375, y = 0.15 + 0.04, width = 0.675, height = 0.0375, just = c("left", "bottom"))
  pushViewport(vp)

  grid.xaxis(seq(0, 1, by = 1/n.subgrp.var1), vp = viewport(y=0.5), label = FALSE, gp = gpar(cex = 0.6))
  upViewport()

  vp <- viewport(x = 0.15 + 0.0375, y = 0.0775 + 0.075, width = 0.675, height = 0.0375, just = c("left", "bottom"))
  pushViewport(vp)
  for (i in 1:n.subgrp.var1){
    vp <- viewport(x = 0 + (i-1) * 1/n.subgrp.var1, y = 0, width = 1/n.subgrp.var1, height = 1, just = c("left", "bottom"))
    pushViewport(vp)
    grid.text(0.5, vp = viewport(y = 0.5), label = lab.subgrp.col[i], gp = gpar(cex = font.size[3]))
    upViewport()
  }
  upViewport()


  #####   left-middle gap
  vp <- viewport(x = 0.15, y = 0.19 + 0.0375, width=0.0375, height = 0.675, just = c("left", "bottom"))
  pushViewport(vp)
  grid.yaxis(seq(0, 1, by = 1/n.subgrp.var2), vp = viewport(y = 0.5), label = FALSE, gp = gpar(cex = 0.6))
  upViewport()

  vp <- viewport(x = 0.15 - 0.0375, y = 0.19 + 0.0375, width = 0.0375, height = 0.675, just = c("left", "bottom"))
  pushViewport(vp)
  for (j in 1 : n.subgrp.var2){
    vp <- viewport(x = 0, y = 0 + (j-1)*1/n.subgrp.var2, width = 1, height = 1/n.subgrp.var2, just = c("left", "bottom"))
    pushViewport(vp)
    grid.text(0.5, vp = viewport(y = 0.5), label = lab.subgrp.row[j], gp = gpar(cex = font.size[3]), rot = 90)
    upViewport()
  }
  upViewport()


  ##########   middle-middle cell

  pal.2 = colorRampPalette(c("black", "red", "yellow"), space = "rgb")
  breaks = seq(min(range.strip) - 1e-8, max(range.strip) + 1e-8, length.out = n.brk)
  col.vec = pal.2(length(breaks)-1)

  col.treat = vector()
  col.vec = pal.2(length(breaks)-1)
  treat_size = c((treatment.mean))
  for (i in 1 : (n.subgrp.tol)){
    if (is.na(treat_size[i]))
    {col.treat[i] == "white"
    }else{
      col.idx = which(treat_size[i] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }

  ##### the small rectangles representing subgroups

  vp <- viewport(x= 0.15 + 0.0375, y = 0.19 + 0.0375, width= 0.675, height= 0.675, just = c("left", "bottom"))
  pushViewport(vp)
  grid.rect()
  for (j in 1 : (n.subgrp.var1 - 1)){
    grid.lines(c(j * 1/n.subgrp.var1, j* 1/n.subgrp.var1), c(0, 1), gp = gpar(lty = 2, col = "gray"))
  }
  for (i in 1 : (n.subgrp.var2 - 1)){
    grid.lines(c(0, 1), c(i * 1/n.subgrp.var2, i * 1/n.subgrp.var2), gp = gpar(lty = 2, col = "gray"))
  }
  upViewport()

  ind = 0
  data.size = dim(dat)[1]
  for (j in 1 : n.subgrp.var1){
    for (i in 1 : n.subgrp.var2){
      ind = ind + 1
      vp <- viewport(x = 0.15 + 0.0375 + (j - 1) * (0.675/n.subgrp.var1), y = 0.19 + 0.0375 + (i - 1) * (0.675/n.subgrp.var2),
                     width=  (0.675/n.subgrp.var1), height = (0.675/n.subgrp.var2), just = c("left", "bottom"))
      pushViewport(vp)

      if (ss.rect){
        width.rect <- height.rect <- ss.subgrp[i,j]/data.size
      }else{
        width.rect <- height.rect <- 1}

      vp <- viewport(x = 0, y = 0, width = width.rect, height = height.rect, just = c("left", "bottom"))
      pushViewport(vp)

      grid.rect(gp = gpar(fill= col.treat[ind], col = "black", lty = 1))
      upViewport()
      grid.text(ss.subgrp[i,j], gp = gpar(fontsize = font.size[4], fontface = 2, col = "gray"))
      upViewport()
    }
  }

  ###### vertical mariginal effect sizes

  col.treat = vector()
  col.vec = pal.2(length(breaks)-1)
  for (i in 1 : n.subgrp.var2){
    if (is.na(treatment.mean.mar[i, 2])){
      col.treat[i] == "white"
    }else{
      col.idx = which(treatment.mean.mar[i, 2] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }

  vp <- viewport(x = 0.0375, y = 0.19 + 0.0375, width = 0.075, height = 0.675, just = c("left", "bottom"))
  pushViewport(vp)
  grid.rect(gp = gpar(fill="white"))
  for (i in 1:n.subgrp.var2){
    vp <- viewport(x = 0 , y = 0 + (i-1)*1/n.subgrp.var2, width = 1, height = 1/n.subgrp.var2, just = c("left", "bottom"))
    pushViewport(vp)
    grid.rect(gp = gpar(fill= col.treat[i]))
    upViewport()
  }
  upViewport()

  ###### horizontal mariginal effect sizes

  col.treat = vector()
  col.vec = pal.2(length(breaks)-1)
  for (i in 1 : n.subgrp.var1){
    if (is.na(treatment.mean.mar[i, 1])){
      col.treat[i] == "white"
    }else{
      col.idx = which(treatment.mean.mar[i, 1] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }

  vp <- viewport(x = 0.15 + 0.0375, y = 0.0775, width = 0.675, height = 0.075, just = c("left", "bottom"))
  pushViewport(vp)
  grid.rect(gp = gpar(fill = "white"))
  for (i in 1 : n.subgrp.var1){
    vp <- viewport(x = 0 + (i-1)*1/n.subgrp.var1, y = 0, width = 1/n.subgrp.var1, height = 1, just = c("left", "bottom"))
    pushViewport(vp)
    grid.rect(gp = gpar(fill= col.treat[i]))
    upViewport()
  }
  upViewport()


  ##########  right-middle cell

  vp <- viewport(x = 0.005 + 0.9, y = 0.19 + 0.0375, width = 0.055, height = 0.675, just = c("left", "bottom"))
  pushViewport(vp)

  col.bar.height = 1/ length(col.vec)
  for (i in 1 : length(col.vec)){
    vp <- viewport(x = 0 , y = 0 + (i-1) * col.bar.height, width=1, height=col.bar.height,  just = c("left", "bottom"))
    pushViewport(vp)
    grid.rect(gp = gpar(fill = col.vec[i], col = NA))
    upViewport()
  }
  grid.rect()

  grid.yaxis(seq(0, 1, len = n.brk), vp = viewport(y = 0.5), label = seq(min(range.strip), max(range.strip), len = n.brk),
             gp = gpar(cex = font.size[5]), edits = gEdit(gPath="labels", rot=90))
  upViewport()


  ##########  middle-top cell
  vp <- viewport(x = 0.5, y = 0.92, width = 0.75, height = 0.05, just = c("centre", "bottom"))
  pushViewport(vp)
  #main.title = paste("Treatment effect sizes across subgroups (N =", data.size,")", sep = "")
  grid.text(title, gp = gpar(fontsize = font.size[1], fontface = 2))
  upViewport()


  ##########  middle-bottom cell
  vp <- viewport(x = 0.15, y = 0.04, width = 0.75, height = 0.04, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(lab.vars[1], gp = gpar(fontsize = font.size[2], fontface = 2))
  upViewport()

  ##########  left-middle cell
  vp <- viewport(x = 0, y = 0.2, width = 0.04, height = 0.75, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(lab.vars[2], gp = gpar(fontsize = font.size[2], fontface = 2), rot = 90)
  upViewport()

  ##########  right-middle cell
  vp <- viewport(x = 0.96, y = 0.19, width = 0.04, height = 0.75, just = c("left", "bottom"))
  pushViewport(vp)
  #col.bar.title = strip #"Treatment effect size"
  grid.text(strip, gp = gpar(fontsize= font.size[2], fontface = 2), rot = 90)
  upViewport()


}
