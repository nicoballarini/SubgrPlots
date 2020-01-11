#' Level plot for subgroup effect size
#'
#' This function produces a level plot showing the treatment effect size of pairwise subgroups and marginal subgroups defined by the
#' categories of two covariates. Also, it prints out the minimum and maximum of the treatment effect size on the console so as to set
#' an approapriate range for effect size on the colour strip. Note that there are two types of graphical display; whether show subgroup
#' sample size by rectangles with different sizes (proportional to the ratio of sample size to the full size) or not. In addition, the
#' function uses log odd ratio and log hazard ratio for displaying subgroup effect sizes in binary and survival data, respectively.
#'
#' @param dat          a data set
#' @param covari.sel   a vector of indices of the two covariates
#' @param trt.sel      a covariate index which specifies the treatment code
#' @param resp.sel     a covariate index which specifies the response variable
#' @param outcome.type a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#' @param ss.rect      a logical operator displaying the rectangles for subgroup sample sizes if TRUE
#' @param range.strip  a vector with two elements specifying the range of treatment effect size for display
#' @param n.brk        a number specifying the number of the points dividing the range of the argument "range.strip".
#' @param n.brk.axis   a number specifying the number of breakpoints dividing the axis of the argument "range.strip".
#' @param font.size    a vector specifying the size of labels and text; the first element is for the main title; the second element is
#'                for the covariates labels and the colour strip label; the third is for the category labels of the first and second
#'                covariates; the fourth is for the text in the middle cells; the fifth is for the unit label on the colour strip.
#' @param title        a string specifying the main title.
#' @param strip        a string specifying the title of the colour strip.
#' @param effect           either "HR" or "RMST". only when outcome.type = "survival"
#' @param time             time for calculating the RMST
#' @param show.overall     logical. whether to show or not the overall treatment effect in the strip
#' @param palette          either "divergent" or "hcl"
#' @param col.power        to be used when palette = "hcl". see colorspace package for reference
#' @param grid.newpage     logical. If TRUE (default), the function calls grid::grid.newpage() to start from an empty page.
#'
#' @examples
#' # Load the data to be used
#' data(prca)
#' dat  <- prca
#' levels(dat$age_group)    <- c("Young", "Middle-aged", "Old")
#' levels(dat$weight_group) <- c("Low", "Mid", "High")
#' names(dat)[c(14,15)]     <- c("age", "weight")
#'
#' ## 1.a Level plot -----------------------------------------------------------
#' plot_level(dat,
#'            covari.sel = c(14,15),
#'            trt.sel = 3,
#'            resp.sel = c(1, 2),
#'            outcome.type = "survival",
#'            ss.rect = FALSE,
#'            range.strip=c(-3, 3),
#'            n.brk = 31,
#'            n.brk.axis =  7,
#'            font.size = c(14, 12, .8, 14, 0.7),
#'            title = "Total sample size = 475",
#'            strip = "Treatment effect size (log hazard ratio)",
#'            effect = "HR",
#'            show.overall = TRUE, palette = "hcl")
#'
#' ## 1.b Modified Level plot --------------------------------------------------
#' plot_level(dat,
#'            covari.sel = c(14,15),
#'            trt.sel = 3,
#'            resp.sel = c(1, 2),
#'            outcome.type = "survival",
#'            ss.rect = TRUE,
#'            range.strip=c(-3, 3),
#'            n.brk = 31,
#'            n.brk.axis =  7,
#'            font.size = c(14, 12, .8, 14, 0.7),
#'            title = paste0("Total sample size = ", nrow(dat)),
#'            strip = "Treatment effect size (log hazard ratio)",
#'            show.overall = TRUE, palette = "hcl")
#' @export
#' @import grid
plot_level <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                       ss.rect=FALSE, range.strip=c(-6, 6),
                       n.brk = 30, n.brk.axis = NULL,
                       font.size = c(15, 12, 0.8, 15, 0.6),
                       title = NULL, strip = NULL,
                       effect = c("HR","RMST"), time = NULL,
                       show.overall = FALSE,
                       palette = "divergent", col.power = 0.5,
                       grid.newpage = TRUE){
  ## 0. argument validity check  ###############################################
  effect = match.arg(effect)
  if(n.brk%%2 == 0) n.brk = n.brk+1
  if(is.null(n.brk.axis)) n.brk.axis = n.brk

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

  ## 1. create subgroup data  ##################################################
  lab.vars = names(dat)[covari.sel]                 # set the names of the covariates which relates to the defined subgroup; if a covariate
                                                    # are considered for multiple times, we make their name identical. (otherwise, the resulsting
                                                    # names are like var.1, var.2 and so on.)
  names(dat)[trt.sel] = "trt"                 # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"             # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"             # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"          # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"        # rename the response variable for survival right censoring status
  }


  ## Calculate overall Treatment effect ----------------------------------------
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
      model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = dat)
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
          if (effect == "HR"){
            model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.subgrp[[k]])
            model.sum = summary(model.int)
            treatment.mean[i,j] = model.sum$coef[1, 1]
          }
          if (effect == "RMST"){
            dat.subgr.i = data.subgrp[[k]]
            rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                  arm = dat.subgr.i$trt, tau = time)
            treatment.mean[i,j] = rmst$unadjusted.result[1,1]
          }
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
          if (effect == "HR"){
            model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.mar.subgrp2[[i]])
            model.sum = summary(model.int)
            treatment.mean.mar[i,2] = model.sum$coef[1, 1]
          }
          if (effect == "RMST"){
            dat.subgr.i = data.mar.subgrp2[[i]]
            rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                  arm = dat.subgr.i$trt, tau = time)
            treatment.mean.mar[i,2] = rmst$unadjusted.result[1,1]
          }
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
        if (effect == "HR"){
          model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.mar.subgrp1[[j]])
          model.sum = summary(model.int)
          treatment.mean.mar[j,1] = model.sum$coef[1, 1]
        }
        if (effect == "RMST"){
          dat.subgr.i = data.mar.subgrp1[[j]]
          rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                arm = dat.subgr.i$trt, tau = time)
          treatment.mean.mar[j,1] = rmst$unadjusted.result[1,1]
        }
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

  ## 2. produce a graph  #######################################################
  panel_dim = 0.65
  if (grid.newpage) grid::grid.newpage()

  ##### draw x-axis ------------------------------------------------------------
  vp <- grid::viewport(x = 0.15 + 0.0375, y = 0.15 + 0.04, width = panel_dim, height = 0.0375, just = c("left", "bottom"))
  grid::pushViewport(vp)
  grid::grid.xaxis(seq(0, 1, by = 1/n.subgrp.var1), vp = grid::viewport(y = 1), label = FALSE, gp = grid::gpar(cex = 0.6))
  grid::upViewport()

  vp <- grid::viewport(x = 0.15 + 0.0375, y = 0.0775 + 0.075, width = panel_dim, height = 0.0375, just = c("left", "bottom"))
  grid::pushViewport(vp)
  for (i in 1:n.subgrp.var1){
    vp <- grid::viewport(x = 0 + (i-1) * 1/n.subgrp.var1, y = 0, width = 1/n.subgrp.var1, height = 1, just = c("left", "bottom"))
    grid::pushViewport(vp)
    grid::grid.text(0.5, vp = grid::viewport(y = 0.75), label = lab.subgrp.col[i], gp = grid::gpar(cex = font.size[3]))
    grid::upViewport()
  }
  grid::upViewport()


  ##### draw y-axis ------------------------------------------------------------
  vp <- grid::viewport(x = 0.15 + 2*0.04/3, y = 0.19 + 0.0375, width=0.0375, height = panel_dim, just = c("left", "bottom"))
  grid::pushViewport(vp)
  grid::grid.yaxis(seq(0, 1, by = 1/n.subgrp.var2), vp = grid::viewport(y = 0.5), label = FALSE, gp = grid::gpar(cex = 0.6))
  grid::upViewport()

  vp <- grid::viewport(x = 0.15 - 0.0375, y = 0.19 + 0.0375, width = 0.0375, height = panel_dim, just = c("left", "bottom"))
  grid::pushViewport(vp)
  for (j in 1 : n.subgrp.var2){
    vp <- grid::viewport(x = 0, y = 0 + (j-1)*1/n.subgrp.var2, width = 1, height = 1/n.subgrp.var2, just = c("left", "bottom"))
    grid::pushViewport(vp)
    grid::grid.text(0.75, vp = grid::viewport(y = 0.5), label = lab.subgrp.row[j], gp = grid::gpar(cex = font.size[3]), rot = 90)
    grid::upViewport()
  }
  grid::upViewport()


  ##########   set color palette
  pal.YlRd = colorRampPalette(c("#fee090", "#d73027"), space = "rgb")
  pal.WhBl = colorRampPalette(c("#e0f3f8", "#4575b4"),  space = "rgb")

  pal.2 = colorRampPalette(c("#fc8d59", "#ffffbf", "#91bfdb"), space = "rgb")
  breaks = seq(min(range.strip) - 1e-8, max(range.strip) + 1e-8, length.out = n.brk)
  col.vec = pal.2((length(breaks)-1))
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

  col.treat = vector()
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

  ##### draw main panel --------------------------------------------------------
  vp <- grid::viewport(x= 0.15 + 0.0375, y = 0.19 + 0.0375,
                 width= panel_dim, height= panel_dim, just = c("left", "bottom"))
  grid::pushViewport(vp)
  grid::grid.rect()
  for (j in 1 : (n.subgrp.var1 - 1)){
    grid::grid.lines(c(j * 1/n.subgrp.var1, j* 1/n.subgrp.var1), c(0, 1), gp = grid::gpar(lty = 2, col = "gray"))
  }
  for (i in 1 : (n.subgrp.var2 - 1)){
    grid::grid.lines(c(0, 1), c(i * 1/n.subgrp.var2, i * 1/n.subgrp.var2), gp = grid::gpar(lty = 2, col = "gray"))
  }
  grid::upViewport()

  ind = 0
  data.size = dim(dat)[1]
  for (j in 1 : n.subgrp.var1){
    for (i in 1 : n.subgrp.var2){
      ind = ind + 1
      vp <- grid::viewport(x = 0.15 + 0.0375 + (j - 1) * (panel_dim/n.subgrp.var1), y = 0.19 + 0.0375 + (i - 1) * (panel_dim/n.subgrp.var2),
                     width=  (panel_dim/n.subgrp.var1), height = (panel_dim/n.subgrp.var2), just = c("left", "bottom"))
      grid::pushViewport(vp)

      if (ss.rect){
        width.rect <- height.rect <- sqrt(ss.subgrp[i,j]/data.size)
      }else{
        width.rect <- height.rect <- 1}

      vp <- grid::viewport(x = 0, y = 0, width = width.rect, height = height.rect, just = c("left", "bottom"))
      grid::pushViewport(vp)

      grid::grid.rect(gp = grid::gpar(fill= col.treat[ind], col = "black", lty = 1))
      grid::upViewport()
      grid::grid.text(ss.subgrp[i,j], gp = grid::gpar(fontsize = font.size[4], fontface = 1, col = "black"), hjust = 0.5, vjust = 0.5)
      grid::upViewport()
    }
  }

  ###### draw vertical marginal effect sizes ---------------------------------------
  col.treat = vector()
  # col.vec = pal.2(length(breaks)-1)
  for (i in 1 : n.subgrp.var2){
    if (is.na(treatment.mean.mar[i, 2])){
      col.treat[i] = "white"
    }else{
      col.idx = which(treatment.mean.mar[i, 2] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }

  vp <- grid::viewport(x = 0.0375, y = 0.19 + 0.0375, width = 0.075, height = panel_dim, just = c("left", "bottom"))
  grid::pushViewport(vp)
  grid.rect(gp = grid::gpar(fill="white"))
  for (i in 1:n.subgrp.var2){
    vp <- grid::viewport(x = 0 , y = 0 + (i-1)*1/n.subgrp.var2, width = 1, height = 1/n.subgrp.var2, just = c("left", "bottom"))
    grid::pushViewport(vp)
    grid.rect(gp = grid::gpar(fill= col.treat[i]))
    grid::upViewport()
  }
  grid::upViewport()

  ###### draw horizontal marginal effect sizes -------------------------------------
  col.treat = vector()
  # col.vec = pal.2(length(breaks)-1)
  for (i in 1 : n.subgrp.var1){
    if (is.na(treatment.mean.mar[i, 1])){
      col.treat[i] = "white"
    }else{
      col.idx = which(treatment.mean.mar[i, 1] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }

  vp <- grid::viewport(x = 0.15 + 0.0375, y = 0.0775, width = panel_dim, height = 0.075, just = c("left", "bottom"))
  grid::pushViewport(vp)
  grid.rect(gp = grid::gpar(fill = "white"))
  for (i in 1 : n.subgrp.var1){
    vp <- grid::viewport(x = 0 + (i-1)*1/n.subgrp.var1, y = 0, width = 1/n.subgrp.var1, height = 1, just = c("left", "bottom"))
    grid::pushViewport(vp)
    grid::grid.rect(gp = grid::gpar(fill= col.treat[i]))
    grid::upViewport()
  }
  grid::upViewport()


  ##########  draw legend ------------------------------------------------------
  vp <- grid::viewport(x = 0.005 + 0.9, y = 0.19 + 0.0375, width = 0.055, height = panel_dim, just = c("left", "bottom"))
  grid::pushViewport(vp)

  col.bar.height = 1/length(col.vec)
  for (i in 1 : length(col.vec)){
    vp <- grid::viewport(x = 0 , y = 0 + (i-1) * col.bar.height, width=1, height=col.bar.height,  just = c("left", "bottom"))
    grid::pushViewport(vp)
    grid.rect(gp = grid::gpar(fill = col.vec[i], col = NA))
    grid::upViewport()
  }
  # grid.rect()

  grid::grid.yaxis(seq(0, 1, len = n.brk.axis), vp = grid::viewport(y = 0.5),
             label = seq(min(range.strip), max(range.strip), len = n.brk.axis),
             gp = grid::gpar(cex = font.size[5]),
             edits = grid::gEdit(gPath="labels", rot=90, hjust = 0.5, vjust = 0.5))

  if(show.overall){
    cat(sprintf("Overall Treatment effect is: %.4f, with confidence interval: (%.4f;%.4f)\n",
        overall.treatment.mean, overall.treatment.lower, overall.treatment.upper))
    grid::grid.points(x = 0.5, (overall.treatment.mean / (range.strip[2]-range.strip[1])) + 0.5, pch = 20)
    grid::grid.points(x = 0.5, (overall.treatment.lower / (range.strip[2]-range.strip[1])) + 0.5, pch = "-")
    grid::grid.points(x = 0.5, (overall.treatment.upper / (range.strip[2]-range.strip[1])) + 0.5, pch = "-")
    grid::grid.segments(x0 = 0.5, x1 = 0.5,
                  y0 = (overall.treatment.lower/ (range.strip[2]-range.strip[1])) + 0.5,
                  y1 = (overall.treatment.upper/ (range.strip[2]-range.strip[1])) + 0.5)
  }
  grid::upViewport()


  ##########  draw plot title middle-top cell-----------------------------------
  vp <- grid::viewport(x = 0.5, y = 0.92, width = 0.75, height = 0.05, just = c("centre", "bottom"))
  grid::pushViewport(vp)
  #main.title = paste("Treatment effect sizes across subgroups (N =", data.size,")", sep = "")
  grid::grid.text(title, gp = grid::gpar(fontsize = font.size[1], fontface = 1))
  grid::upViewport()


  ##########  draw x-axis title middle-bottom cell -----------------------------
  vp <- grid::viewport(x = 0.15, y = 0.04, width = 0.75, height = 0.04, just = c("left", "bottom"))
  grid::pushViewport(vp)
  grid::grid.text(lab.vars[1], gp = grid::gpar(fontsize = font.size[2], fontface = 1))
  grid::upViewport()

  ##########  draw y-axis title left-middle cell   -----------------------------
  vp <- grid::viewport(x = 0, y = 0.2, width = 0.04, height = 0.75, just = c("left", "bottom"))
  grid::pushViewport(vp)
  grid::grid.text(lab.vars[2], gp = grid::gpar(fontsize = font.size[2], fontface = 1), rot = 90)
  grid::upViewport()

  ##########  draw legend title right-middle cell  -----------------------------
  vp <- grid::viewport(x = 0.96, y = 0.2, width = 0.04, height = 0.75, just = c("left", "bottom"))
  grid::pushViewport(vp)
  #col.bar.title = strip #"Treatment effect size"
  grid::grid.text(strip, gp = grid::gpar(fontsize= font.size[2], fontface = 1), rot = 90)
  grid::upViewport()
}
