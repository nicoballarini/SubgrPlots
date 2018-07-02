#' Mosaic plot
#'
#' This function produces a mosaic plot for subgroup analysis
#'
#' @param dat              a data set
#' @param covari.sel       a vector of indices of the two covariates
#' @param trt.sel          a covariate index specifying the treatment code
#' @param resp.sel         a covariate index specifying the response variable
#' @param outcome.type     a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#' @param range.v          a vector specifying the vertical range of graphical display.
#' @param adj.ann.subgrp   a parameter adjusting the distance between a point and its corresponding subgroup label. The smaller the value
#' is, the larger the distance is.
#' @param range.strip      range for the treatment effect scale
#' @param n.brk            number of breaks in the treatment effect scale
#' @param n.brk.axis        number of breaks in the axis of the treatment effect scale
#' @param font.size        a vector specifying the size of labels and text; the first element is for the main title, the second is for
#' for x-axis and y-axis labels; the thrid is for the legend text of subgroups; the fourth is for the subgroup
#' labels near points; the fifth is for the unit labels on all the axes.
#' @param title            a string specifying the main title.
#' @param lab.xy           a list of two strings specifying the labels of the x and y axes.
#' @param strip            title for the treatment effect scale
#' @param effect           either "HR" or "RMST". only when outcome.type = "survival"
#' @param lwd.             line width for the mosaics
#' @param sep.             separation for the mosaics
#' @param show.overall     logical. whether to show or not the overall treatment effect in the strip
#' @param palette          either "divergent" or "hcl"
#' @param col.power        to be used when palette = "hcl". see colorspace package for reference
#' @param print.ss         logival indicating whether to show the sample sizes of subgroups
#' @param col.line         color of the mosaics' border
#' @param time             time for calculating the RMST
#' @param show.marginal    logical indicating whether to show the marginal subgroups. only when 2 covariates are used
#' @param show.effect      logical indicating whether to show effect size using color or not. only when 3 covariates are used
#'
#' @examples
#' library(dplyr)
#' data(prca)
#' dat <- prca
#' dat %>%
#'   mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
#'          hx = factor(ifelse(hx == 0 , "No", "Yes")),
#'          Treatment = factor(ifelse(rx == 0 , "Control", "Treatment")),
#'          Survival = factor(ifelse(survtime > 24 , "Yes", "No"),
#'          levels = c("Yes", "No")))-> dat
#' levels(dat$age_group) = c("Young","Middle-aged","Old")
#' levels(dat$weight_group)  = c("Low","Mid","High")
#' # Change variable names
#' dat %>%
#'   rename(`Bone Metastasis` = bm,
#'          `Performance rating` = pf,
#'          `History of cardiovascular events` = hx,
#'          `2-year survival` = Survival,
#'          Weight = weight_group,
#'          Age = age_group) -> dat
#' ## 2.a Mosaic plot with 2 variables -----------------------------------------
#' plot_mosaic(dat = dat,
#'             covari.sel = c(14, 15),
#'             trt.sel = 3,
#'             resp.sel = c(1, 2),
#'             outcome.type =  "survival",
#'             range.v = NULL,
#'             adj.ann.subgrp = 4,
#'             range.strip=c(-3, 3),
#'             n.brk = 31,
#'             n.brk.axis = 7, sep. = 0.034,
#'             font.size = c(10, 10, 10, 10, 0.7),
#'             title = NULL, lab.xy = NULL,
#'             strip = "Treatment effect size (log-hazard ratio)",
#'             col.line = "white", lwd. = 2,
#'             effect = "HR", print.ss = FALSE, palette = "hcl")
#'
#' ## 2.b Mosaic plot with 3 variables -----------------------------------------
#' plot_mosaic(dat = dat,
#'             covari.sel = c(15, 7, 4),
#'             trt.sel = 3,
#'             resp.sel = c(1, 2),
#'             outcome.type =  "survival",
#'             range.v = NULL, adj.ann.subgrp = 4,
#'             range.strip=c(-3, 3),
#'             n.brk = 31, n.brk.axis = 7,
#'             font.size = c(10, 10, 10, 10, 0.7),
#'             title = NULL, lab.xy = NULL,
#'             strip = "Treatment effect size (log-hazard ratio)",
#'             effect = "HR", palette = "hcl")
#' @export
#' @import grid
plot_mosaic <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                        range.v = NULL, adj.ann.subgrp = 4,
                        range.strip=c(-3, 3),
                        n.brk = 30,
                        n.brk.axis = NULL,
                        font.size = c(1, 1, 0.85, 0.85, 1),
                        title = NULL, lab.xy = NULL,
                        strip = "default",
                        effect = "HR", lwd. = 2, sep. = 0.05,
                        show.overall = TRUE,
                        palette = "divergent", col.power = 0.5,
                        print.ss = FALSE, col.line = "white",
                        time = NULL, show.marginal = TRUE, show.effect = TRUE){
  if (length(covari.sel) == 2){
    if (show.marginal){
      plot_mosaic_2_marginal(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                             range.v = range.v, adj.ann.subgrp = adj.ann.subgrp,
                             range.strip = range.strip,
                             n.brk = n.brk,
                             n.brk.axis = n.brk.axis,
                             font.size = font.size,
                             title = title, lab.xy = lab.xy,
                             strip = strip,
                             effect = effect, lwd. = lwd., sep. = sep.,
                             show.overall = show.overall,
                             palette = palette, col.power = col.power,
                             print.ss = print.ss, col.line = col.line,
                             time = time)
    } else {
      plot_mosaic_2(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                    range.v = range.v, adj.ann.subgrp = adj.ann.subgrp,
                    range.strip = range.strip,
                    n.brk = n.brk,
                    n.brk.axis = n.brk.axis,
                    font.size = font.size,
                    title = title, lab.xy = lab.xy,
                    strip = strip,
                    effect = effect, lwd. = lwd., sep. = sep.,
                    show.overall = show.overall,
                    palette = palette, col.power = col.power,
                    print.ss = print.ss, col.line = col.line,
                    time = time)
    }
  } else if (length(covari.sel) == 3){
    if (show.effect){
      plot_mosaic_3(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                    range.v = range.v, adj.ann.subgrp = adj.ann.subgrp,
                    range.strip = range.strip,
                    n.brk = n.brk,
                    n.brk.axis = n.brk.axis,
                    font.size = font.size,
                    title = title, lab.xy = lab.xy,
                    strip = strip,
                    effect = effect, lwd. = lwd., sep. = sep.,
                    show.overall = show.overall,
                    palette = palette, col.power = col.power,
                    print.ss = print.ss, col.line = col.line,
                    time = time)
    } else {
      plot_mosaic_3_noeffect(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                             range.v = range.v, adj.ann.subgrp = adj.ann.subgrp,
                             range.strip = range.strip,
                             n.brk = n.brk,
                             n.brk.axis = n.brk.axis,
                             font.size = font.size,
                             title = title, lab.xy = lab.xy,
                             strip = strip,
                             effect = effect, lwd. = lwd., sep. = sep.,
                             palette = palette, col.power = col.power,
                             print.ss = print.ss, col.line = col.line)
    }
  } else {
    stop("Only 2 or 3 covariates are allowed in 'covari.sel'")
  }
}



#' @import grid
plot_mosaic_2 <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                          range.v = NULL, adj.ann.subgrp = 4,
                          range.strip=c(-3, 3),
                          n.brk = 30,
                          n.brk.axis = NULL,
                          font.size = c(1, 1, 0.85, 0.85, 1),
                          title = NULL, lab.xy = NULL,
                          strip = "Treatment effect size",
                          effect = "HR", lwd. = 2, sep. = 0.05,
                          show.overall = TRUE,
                          palette = "divergent", col.power = 0.5,
                          print.ss = FALSE, col.line = "white",
                          time = NULL){
  old.par <- par(no.readonly=T)
  if(is.null(n.brk.axis)) n.brk.axis = n.brk
  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
  }
  # Calculate overall Treatment effect -----------------------------------------
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
  labels = names(dat)[covari.sel]
  l1 = unique(dat[, covari.sel[1]])
  l2 = unique(dat[, covari.sel[2]])

  n1 = table(dat[, covari.sel[1]])
  c1 = length(n1)
  n1. = sum(n1)
  w1  = n1 / n1.
  w1  = w1 * (sum(w1)-sep.*(c1-1))
  sum(w1)
  n2 = as.vector(table(dat[, covari.sel[2:1]]))
  c2 = length(table(dat[, covari.sel[2]]))
  n2. = rep(n1,each = c2)
  w2 = n2 / n2.
  w2 = w2

  cats.var1 = names(table(dat[,covari.sel[1]]))     # the names of categories of the selected first cavariate
  cats.var2 = names(table(dat[,covari.sel[2]]))     # the names of categories of the selected second cavariate
  n.subgrp.var1 = dim(table(dat[,covari.sel[1]]))   # the number of levels for the first covariate (placed in the bottom of the figure)
  n.subgrp.var2 = dim(table(dat[,covari.sel[2]]))   # the number of levels for the second covariate (placed in the left of the figure)
  n.subgrp.tol = n.subgrp.var1 * n.subgrp.var2

  idx1 = list()
  idx2 = list()
  for (i in 1 : n.subgrp.var1 ) idx1[[i]] = which((dat[, covari.sel[1]] == cats.var1[i])  == T )
  for (i in 1 : n.subgrp.var2 ) idx2[[i]] = which((dat[, covari.sel[2]] == cats.var2[i])  == T )
  idx.subgrp = list()                               # the index set of the subgroups
  data.subgrp = list()                              # the data set of the subgroups
  ss.subgrp = list()  # the data set of the mariginal subgroups


  #####   V1.1 -------------------------------------------------------------------
  ii=0
  for (i in 1:c1){
    for (j in 1:c2){
      ii = ii + 1
      idx.subgrp[[ii]] =  intersect(idx1[[i]], idx2[[j]])
      data.subgrp[[ii]] =  dat[idx.subgrp[[ii]], ]
      ss.subgrp[[ii]] = dim(data.subgrp[[ii]])[1]
    }
  }


  treatment.mean = list()
  ii=0
  for (i in 1:c1){
    for (j in 1:c2){
        ii = ii + 1
        if (sum((data.subgrp[[ii]]$trt == "1")) == 0 | sum((data.subgrp[[ii]]$trt == "0")) == 0){
          treatment.mean[[ii]] = NA
        }else{
          if (outcome.type == "continuous"){
            model.int = lm(resp ~ trt,  data = data.subgrp[[ii]])
            model.sum = summary(model.int)
            treatment.mean[[ii]] = model.sum$coefficients[2, 1]
          }else if (outcome.type == "binary"){
            model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[ii]])
            model.sum = summary(model.int)
            treatment.mean[[ii]] = model.sum$coefficients[2, 1]
          }else if (outcome.type == "survival"){
            if (effect == "HR"){
              model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.subgrp[[ii]])
              model.sum = summary(model.int)
              treatment.mean[[ii]] = model.sum$coef[1, 1]
            }
            if (effect == "RMST"){
              dat.subgr.i = data.subgrp[[ii]]
              rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                    arm = dat.subgr.i$trt, tau = time)
              treatment.mean[[ii]] = rmst$unadjusted.result[1,1]

          }
        }
      }
    }
  }

  cat("The minimum of treatment effect sizes is", min(unlist(treatment.mean), na.rm = T), "\n")
  cat("The maximum of treatment effect sizes is", max(unlist(treatment.mean), na.rm = T), "\n")


  pal.YlRd = colorRampPalette(c("#fee090", "#d73027"), space = "rgb")
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
  col.treat = vector()
  treat_size = unlist(treatment.mean)
  for (i in 1 : (n.subgrp.tol)){
    if (is.na(treat_size[i]))
    {col.treat[i] = "white"
    }else{
      col.idx = which(treat_size[i] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }



  #####   Produce a plot -------------------------------------------------------------------
  par(mar=c(0,0,0,0), xpd = TRUE)
  grid.newpage()
  ii=0
  vp <- viewport(x = 0, width = 0.9, height = 1,just = c("left", "center"))
  pushViewport(vp)
  vp <- viewport(x = 2*sep., width = 1-3*sep., height = 1 - 4*sep.,just = c("left", "center"))
  pushViewport(vp)


  #####   V1.1 -------------------------------------------------------------------
  for (i in 1:c1){
    if (i==1) {init.i = 0 + sep.*(i-1)} else {init.i = sum(w1[(i-1):1]) + sep.*(i-1)}
    vp <- viewport(x = init.i, y = 0, width = w1[i], height = 1, just = c("left", "bottom"))
    pushViewport(vp)

    for (k in 1:c2){
        ii=ii+1
        if (k==1) {init = 0} else {init = sum(w2[ii-((k-1):1)])}
        vp <- viewport(x = 0, y = init,
                       width = 0.99,
                       height = w2[ii]*0.99,
                       just = c("left", "bottom"))
        pushViewport(vp)
        grid.rect(gp = gpar(fill = col.treat[ii], col = "white", lty = 1, lwd = lwd.))
        if(print.ss) {grid.text(ss.subgrp[[ii]], gp = gpar(fontsize = font.size[3]))}
        if (is.na(col.treat[ii])) {
          if (w2[ii] == 0) {
            grid.rect(gp = gpar(col = "black",
                                lty = 1, lwd = lwd.))
          }else{
            grid.rect(gp = gpar(fill = "black", col = "white",
                                lty = 1, lwd = lwd.))
          }
        }
        if (i == 1) {
          grid.text(cats.var2[k], x = -0.02, vjust = 0, gp = gpar(fontsize = font.size[3]), rot = 90)
        }

        if (k == 1) {
          vp <- viewport(x = 0, y = 0, height = -sep., just = c("left", "bottom"))
          pushViewport(vp)
          upViewport()
        }
      upViewport()
      grid.text(cats.var1[i], y = 1.02, vjust = 0, gp = gpar(fontsize = font.size[3]))
    }
    upViewport()
  }
  upViewport(2)
  vp <- viewport(x = 0, y=1, width = 0.9, height = sep., just = c("left", "top"))
  pushViewport(vp)
  grid.text(labels[1], vjust = 0.5,  hjust = 0.5, gp = gpar(fontsize= font.size[2], fontface = 1))
  upViewport()

  vp <- viewport(x = 0, y=0.5, width = sep., height = 1 - 4*sep., just = c("left", "center"))
  pushViewport(vp)
  grid.text(labels[2], vjust = 0.5,  hjust = 0.5, gp = gpar(fontsize= font.size[2], fontface = 1), rot = 90)
  upViewport()


  #####  produce legend --------------------------------------------------------
  vp <- viewport(x = 0.9, width = 0.1, height = 1,just = c("left", "center"))
  pushViewport(vp)
  # grid.rect()

  vp <- viewport(x = 0.4, width = 0.2, height = 1 - 4*sep., just = c("left", "center"))
  pushViewport(vp)

  col.bar.height = 1/ length(col.vec)
  for (i in 1 : length(col.vec)){
    vp <- viewport(x = 0 , y = 0 + (i-1) * col.bar.height, width=1, height=col.bar.height,  just = c("left", "bottom"))
    pushViewport(vp)
    grid.rect(gp = gpar(fill = col.vec[i], col = NA))
    upViewport()
  }

  grid.yaxis(seq(0, 1, len = n.brk.axis), vp = viewport(y = 0.5),
             label = seq(min(range.strip), max(range.strip), len = n.brk.axis),
             gp = gpar(cex = font.size[5], lwd = 0),
             edits = gEdit(gPath="labels", rot=90, hjust = 0.5, vjust = 0.5))
  if(show.overall){
    cat("Overall Treatment effect is:",
        overall.treatment.mean, ", with confidence interval: (",
        overall.treatment.lower,";",overall.treatment.upper,")")
    grid.points(x = 0.5, (overall.treatment.mean / (range.strip[2]-range.strip[1])) + 0.5, pch = 20)
    grid.points(x = 0.5, (overall.treatment.lower / (range.strip[2]-range.strip[1])) + 0.5, pch = "-")
    grid.points(x = 0.5, (overall.treatment.upper / (range.strip[2]-range.strip[1])) + 0.5, pch = "-")
    grid.segments(x0 = 0.5, x1 = 0.5,
                  y0 = (overall.treatment.lower/ (range.strip[2]-range.strip[1])) + 0.5,
                  y1 = (overall.treatment.upper/ (range.strip[2]-range.strip[1])) + 0.5)
  }
  upViewport()


  vp <- viewport(x = 0.6, width = 0.4, height = 1 - 4*sep., just = c("left", "center"))
  pushViewport(vp)
  #col.bar.title = strip #"Treatment effect size"
  grid.text(strip, gp = gpar(fontsize= font.size[2], fontface = 1), rot = 90)
  upViewport()
  upViewport()
  par(old.par)
}



#' @import grid
plot_mosaic_2_marginal <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                                   range.v = NULL, adj.ann.subgrp = 4,
                                   range.strip=c(-3, 3),
                                   n.brk = 30,
                                   n.brk.axis = NULL,
                                   font.size = c(1, 1, 0.85, 0.85, 1),
                                   title = NULL, lab.xy = NULL,
                                   strip = "Treatment effect size",
                                   effect = "HR", lwd. = 2, sep. = 0.05,
                                   show.overall = TRUE,
                                   palette = "divergent", col.power = 0.5,
                                   print.ss = FALSE, col.line = "white",
                                   time = NULL){
  old.par <- par(no.readonly=T)
  if(is.null(n.brk.axis)) n.brk.axis = n.brk
  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
  }

  # Calculate overall Treatment effect -----------------------------------------
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
  labels = names(dat)[covari.sel]
  l1 = unique(dat[, covari.sel[1]])
  l2 = unique(dat[, covari.sel[2]])

  n1 = table(dat[, covari.sel[1]])
  c1 = length(n1)
  n1. = sum(n1)
  w1  = n1 / n1.
  w1  = w1 * (sum(w1)-sep.*(c1-1))
  sum(w1)

  n2 = as.vector(table(dat[, covari.sel[2:1]]))
  c2 = length(table(dat[, covari.sel[2]]))
  n2. = rep(n1,each = c2)
  w2 = n2 / n2.
  w2 = w2

  n2.mar = as.vector(table(dat[, covari.sel[2]]))
  n2.mar. = sum(n2.mar)
  w2.mar = n2.mar / n2.mar.

  cats.var1 = names(table(dat[,covari.sel[1]]))     # the names of categories of the selected first cavariate
  cats.var2 = names(table(dat[,covari.sel[2]]))     # the names of categories of the selected second cavariate
  n.subgrp.var1 = dim(table(dat[,covari.sel[1]]))   # the number of levels for the first covariate (placed in the bottom of the figure)
  n.subgrp.var2 = dim(table(dat[,covari.sel[2]]))   # the number of levels for the second covariate (placed in the left of the figure)
  n.subgrp.tol = n.subgrp.var1 * n.subgrp.var2
  data.mar.subgrp1 = list()                         # the data set of the mariginal subgroups (for variance 1, listed columnwisely)
  data.mar.subgrp2 = list()                         # the data set of the mariginal subgroups (for variance 2, listed rowwisely)

  idx1 = list()
  idx2 = list()
  for (i in 1 : n.subgrp.var1 ) idx1[[i]] = which((dat[, covari.sel[1]] == cats.var1[i])  == T )
  for (i in 1 : n.subgrp.var2 ) idx2[[i]] = which((dat[, covari.sel[2]] == cats.var2[i])  == T )
  idx.subgrp = list()                               # the index set of the subgroups
  data.subgrp = list()                              # the data set of the subgroups
  ss.subgrp = list()  # the data set of the mariginal subgroups


  #####   V1.1 -------------------------------------------------------------------
  ii=0
  for (i in 1:c1){
    for (j in 1:c2){
      ii = ii + 1
      # k = i + (j - 1) * n.subgrp.var2
      idx.subgrp[[ii]] =  intersect(idx1[[i]], idx2[[j]])
      data.subgrp[[ii]] =  dat[idx.subgrp[[ii]], ]
      ss.subgrp[[ii]] = dim(data.subgrp[[ii]])[1]


      data.mar.subgrp2[[j]] = dat[idx2[[j]], ]
    }

    data.mar.subgrp1[[i]] = dat[idx1[[i]], ]
  }


  treatment.mean = list()
  treatment.mean.mar = matrix(rep(0, max(n.subgrp.var1, n.subgrp.var2)*2 ),
                              nrow = max(n.subgrp.var1, n.subgrp.var2), ncol = 2)

  ii=0
  for (i in 1:c1){
    for (j in 1:c2){
      ii = ii + 1
      if (sum((data.subgrp[[ii]]$trt == "1")) == 0 | sum((data.subgrp[[ii]]$trt == "0")) == 0){
        treatment.mean[[ii]] = NA
      }else{
        if (outcome.type == "continuous"){
          model.int = lm(resp ~ trt,  data = data.subgrp[[ii]])
          model.sum = summary(model.int)
          treatment.mean[[ii]] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "binary"){
          model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[ii]])
          model.sum = summary(model.int)
          treatment.mean[[ii]] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "survival"){
          if (effect == "HR"){
            model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.subgrp[[ii]])
            model.sum = summary(model.int)
            treatment.mean[[ii]] = model.sum$coef[1, 1]
          }
          if (effect == "RMST"){
            dat.subgr.i = data.subgrp[[ii]]
            rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                  arm = dat.subgr.i$trt, tau = time)
            treatment.mean[[ii]] = rmst$unadjusted.result[1,1]

          }
        }
      }

      if (sum((data.mar.subgrp2[[j]]$trt == "1")) == 0 | sum((data.mar.subgrp2[[j]]$trt == "0")) == 0){
        treatment.mean.mar[j,2] = NA
      }else{
        if (outcome.type == "continuous"){
          model.int = lm(resp ~ trt,  data = data.mar.subgrp2[[j]])
          model.sum = summary(model.int)
          treatment.mean.mar[j,2] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "binary"){
          model.int = glm(resp ~ trt,  family = "binomial", data = data.mar.subgrp2[[j]])
          model.sum = summary(model.int)
          treatment.mean.mar[j,2] = model.sum$coefficients[2, 1]
        }else if (outcome.type == "survival"){
          if (effect == "HR"){
            model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.mar.subgrp2[[j]])
            model.sum = summary(model.int)
            treatment.mean.mar[j,2] = model.sum$coef[1, 1]
          }
          if (effect == "RMST"){
            dat.subgr.i = data.mar.subgrp2[[j]]
            rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                  arm = dat.subgr.i$trt, tau = time)
            treatment.mean.mar[j,2] = rmst$unadjusted.result[1,1]
          }
        }
      }

    }
    if (sum((data.mar.subgrp1[[i]]$trt == "1")) == 0 | sum((data.mar.subgrp1[[i]]$trt == "0")) == 0){
      treatment.mean.mar[i,1] = NA
    }else{
      if (outcome.type == "continuous"){
        model.int = lm(resp ~ trt,  data = data.mar.subgrp1[[i]])
        model.sum = summary(model.int)
        treatment.mean.mar[i,1] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "binary"){
        model.int = glm(resp ~ trt,  family = "binomial", data = data.mar.subgrp1[[i]])
        model.sum = summary(model.int)
        treatment.mean.mar[i,1] = model.sum$coefficients[2, 1]
      }else if (outcome.type == "survival"){
        if (effect == "HR"){
          model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.mar.subgrp1[[i]])
          model.sum = summary(model.int)
          treatment.mean.mar[i,1] = model.sum$coef[1, 1]
        }
        if (effect == "RMST"){
          dat.subgr.i = data.mar.subgrp1[[i]]
          rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                arm = dat.subgr.i$trt, tau = time)
          treatment.mean.mar[i,1] = rmst$unadjusted.result[1,1]
        }
      }
    }

  }

  cat("The minimum of treatment effect sizes is", min(unlist(treatment.mean), na.rm = T), "\n")
  cat("The maximum of treatment effect sizes is", max(unlist(treatment.mean), na.rm = T), "\n")
  # warning("The minimum of treatment effect sizes is ", min(unlist(treatment.mean), na.rm = T), "\n")
  # warning("The maximum of treatment effect sizes is ", max(unlist(treatment.mean), na.rm = T), "\n")


  pal.YlRd = colorRampPalette(c("#fee090", "#d73027"), space = "rgb")
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
  col.treat = vector()
  treat_size = unlist(treatment.mean)
  for (i in 1 : (n.subgrp.tol)){
    if (is.na(treat_size[i]))
    {col.treat[i] = "white"
    }else{
      col.idx = which(treat_size[i] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }



  #####   Produce a plot -------------------------------------------------------------------
  par(mar=c(0,0,0,0), xpd = TRUE)
  grid.newpage()
  ii=0
  vp <- viewport(x = 0.1, y = 0.1, width = 0.7, height = 0.9,just = c("left", "bottom"))
  pushViewport(vp)
  vp <- viewport(x = 2*sep., width = 1-3*sep., height = 1 - 4*sep.,just = c("left", "center"))
  pushViewport(vp)

  # i=2
  #####   V1.1 -------------------------------------------------------------------
  for (i in 1:c1){
    if (i==1) {
      init.i = 0 + sep.*(i-1)
    } else {
        init.i = sum(w1[(i-1):1]) + sep.*(i-1)
    }

    vp <- viewport(x = init.i, y = 0, width = w1[i], height = 1, just = c("left", "bottom"))
    pushViewport(vp)
    # k=1
    k=2
    for (k in 1:c2){
      ii=ii+1
      if (k==1) {init = 0} else {init = sum(w2[ii-((k-1):1)])}
      vp <- viewport(x = 0, y = init,
                     width = 0.99,
                     height = w2[ii]*0.99,
                     just = c("left", "bottom"))
      pushViewport(vp)
      grid.rect(gp = gpar(fill = col.treat[ii], col = col.line, lty = 1, lwd = lwd.))
      if(print.ss) {grid.text(ss.subgrp[[ii]], gp = gpar(fontsize = font.size[3]))}
      if (is.na(col.treat[ii])) {
        if (w2[ii] == 0) {
          grid.rect(gp = gpar(col = "black",
                              lty = 1, lwd = lwd.))
        }else{
          grid.rect(gp = gpar(fill = "black", col = "white",
                              lty = 1, lwd = lwd.))
        }
      }
      if (i == 1) {
        grid.text(cats.var2[k], x = -0.125, vjust = 0, gp = gpar(fontsize = font.size[3]), rot = 90)
      }

      if (k == 1) {
        vp <- viewport(x = 0, y = 0, height = -sep., just = c("left", "bottom"))
        pushViewport(vp)
        upViewport()
      }
      upViewport()
    }
    grid.text(cats.var1[i], y = -0.015, vjust = 1, gp = gpar(fontsize = font.size[3]))
    upViewport()
  }
  upViewport(2)

  # grid.rect()
  mar.width = 0.1
  ## Marginal cells through y axis ---------------------------------------------
  vp <- viewport(x = mar.width/2, y = mar.width, width = mar.width/2,
                 height = 0.9,just = c("left", "bottom"))
  pushViewport(vp)
  vp <- viewport(x = 0, height = 1 - 4*sep.,just = c("left", "center"))
  pushViewport(vp)
  col.treat = vector()
  # col.vec = pal.2(length(breaks)-1)
  for (i in 1 : c2){
    if (is.na(treatment.mean.mar[i, 2])){
      col.treat[i] = "white"
    }else{
      col.idx = which(treatment.mean.mar[i, 2] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }
  k=3
  for (k in 1:c2){
    if (k==1) {
      init = 0
    } else {
      init = sum(w2.mar[(k-1):1])
    }
    vp <- viewport(x = 0, y = init,
                   width = 0.99,
                   height = w2.mar[k],
                   just = c("left", "bottom"))
    pushViewport(vp)
    grid.rect(gp = gpar(fill = col.treat[k], col = col.line, lty = 1, lwd = lwd.))
    # if(print.ss) {grid.text(ss.subgrp[[k]], gp = gpar(fontsize = font.size[3]))}
    if (is.na(col.treat[k])) {
      if (w2[k] == 0) {
        grid.rect(gp = gpar(col = "black",
                            lty = 1, lwd = lwd.))
      }else{
        grid.rect(gp = gpar(fill = "black", col = "white",
                            lty = 1, lwd = lwd.))
      }
    }
    upViewport()
  }
  upViewport(2)

  ## Marginal cells through x axis ---------------------------------------------


  vp <- viewport(x = mar.width, y = mar.width/2, width = 0.7, height = 0.9*mar.width/2,just = c("left", "bottom"))
  pushViewport(vp)
  vp <- viewport(x = 2*sep., width = 1-3*sep., height = 1,  just = c("left", "center"))
  pushViewport(vp)
  col.treat = vector()
  # col.vec = pal.2(length(breaks)-1)
  for (i in 1 : c1){
    if (is.na(treatment.mean.mar[i, 1])){
      col.treat[i] = "white"
    }else{
      col.idx = which(treatment.mean.mar[i, 1] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }
  k=1
  for (k in 1:c1){
    # if (k==1) {
    #   init = 0
    # } else {
    #   init = sum(w1[(k-1):1])
    # }
    if (k==1) {
      init = 0 + sep.*(k-1)
    } else {
      init = sum(w1[(k-1):1]) + sep.*(k-1)
    }

    vp <- viewport(x = init, y = 0,
                   width = w1[k],
                   height = 1,
                   just = c("left", "bottom"))
    pushViewport(vp)
    grid.rect(gp = gpar(fill = col.treat[k], col = col.line, lty = 1, lwd = lwd.))
    # if(print.ss) {grid.text(ss.subgrp[[k]], gp = gpar(fontsize = font.size[3]))}
    if (is.na(col.treat[k])) {
      if (w1[k] == 0) {
        grid.rect(gp = gpar(col = "black",
                            lty = 1, lwd = lwd.))
      }else{
        grid.rect(gp = gpar(fill = "black", col = "white",
                            lty = 1, lwd = lwd.))
      }
    }
    upViewport()
  }
  upViewport(2)

  vp <- viewport(x = mar.width, y = 0,
                 width = 0.7, height = mar.width/2, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(labels[1], vjust = 0.5,  hjust = 0.5, gp = gpar(fontsize= font.size[2], fontface = 1))
  upViewport()

  vp <- viewport(x = 0, y=mar.width,
                 width = mar.width/2, height = 1 - 4*sep., just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(labels[2], vjust = 0.5,  hjust = 0.5, gp = gpar(fontsize= font.size[2], fontface = 1), rot = 90)
  upViewport()







  ##########  produce legend -------------------------------------------------------------
  # vp <- viewport(x = mar.width/2, y = mar.width, width = mar.width/2,  height = 0.9,just = c("left", "bottom"))
  # pushViewport(vp)
  # vp <- viewport(x = 0, height = 1 - 4*sep.,just = c("left", "center"))
  # pushViewport(vp)


  vp <- viewport(x = 0.75, width = 0.25, height = 0.9, y = mar.width, just = c("left", "bottom"))
  pushViewport(vp)
  # grid.rect()

  vp <- viewport(x = 0.4,  width = 0.20, height = 1 - 4*sep., just = c("left", "center"))
  pushViewport(vp)

  col.bar.height = 1/ length(col.vec)
  for (i in 1 : length(col.vec)){
    vp <- viewport(x = 0 , y = 0 + (i-1) * col.bar.height, width=1, height=col.bar.height,  just = c("left", "bottom"))
    pushViewport(vp)
    grid.rect(gp = gpar(fill = col.vec[i], col = NA))
    upViewport()
  }
  # grid.rect()

  grid.yaxis(seq(0, 1, len = n.brk.axis), vp = viewport(y = 0.5),
             label = seq(min(range.strip), max(range.strip), len = n.brk.axis),
             gp = gpar(cex = font.size[5], lwd = 0),
             edits = gEdit(gPath="labels", rot=90, hjust = 0.5, vjust = 0.5))

  if(show.overall){
    cat("Overall Treatment effect is:",
        overall.treatment.mean, ", with confidence interval: (",
        overall.treatment.lower,";",overall.treatment.upper,")\n")
    grid.points(x = 0.5, (overall.treatment.mean / (range.strip[2]-range.strip[1])) + 0.5, pch = 20)
    grid.points(x = 0.5, (overall.treatment.lower / (range.strip[2]-range.strip[1])) + 0.5, pch = "-")
    grid.points(x = 0.5, (overall.treatment.upper / (range.strip[2]-range.strip[1])) + 0.5, pch = "-")
    grid.segments(x0 = 0.5, x1 = 0.5,
                  y0 = (overall.treatment.lower/ (range.strip[2]-range.strip[1])) + 0.5,
                  y1 = (overall.treatment.upper/ (range.strip[2]-range.strip[1])) + 0.5)
  }

  upViewport()


  vp <- viewport(x = 0.6, width = 0.4, height = 1 - 4*sep., just = c("left", "center"))
  pushViewport(vp)
  #col.bar.title = strip #"Treatment effect size"
  grid.text(strip, gp = gpar(fontsize= font.size[2], fontface = 1), rot = 90)
  upViewport()
  upViewport()
  par(old.par)
}






#' @import grid
plot_mosaic_3_noeffect <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                          range.v = NULL, adj.ann.subgrp = 4,
                          range.strip=c(-3, 3),
                          n.brk = 30,
                          n.brk.axis = NULL,
                          font.size = c(1, 1, 0.85, 0.85, 1),
                          title = NULL, lab.xy = NULL,
                          strip = "Treatment effect size",
                          effect = "HR", lwd. = 2, sep. = 0.05,
                          palette = "divergent", col.power = 0.5,
                          print.ss = FALSE, col.line = "white"){
  old.par <- par(no.readonly=T)
  if(n.brk%%2 == 0) n.brk = n.brk+1
  if(is.null(n.brk.axis)) n.brk.axis = n.brk
  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
  }

  labels = names(dat)[covari.sel]
  l1 = unique(dat[, covari.sel[1]])
  l2 = unique(dat[, covari.sel[2]])
  l3 = unique(dat[, covari.sel[3]])

  n1 = table(dat[, covari.sel[1]])
  c1 = length(n1)
  n1. = sum(n1)
  w1  = n1 / n1.
  w1  = w1 * (sum(w1)-sep.*(c1-1))
  sum(w1)
  n2 = as.vector(table(dat[, covari.sel[2:1]]))
  c2 = length(table(dat[, covari.sel[2]]))
  n2. = rep(n1,each = c2)
  w2 = n2 / n2.
  w2 = w2
  n3 = as.vector(table(dat[, covari.sel[3:1]]))
  c3 = length(table(dat[, covari.sel[3]]))
  n3. = rep(n2,each = c3)
  w3 = n3 / n3.
  w3 = w3

  cats.var1 = names(table(dat[,covari.sel[1]]))     # the names of categories of the selected first cavariate
  cats.var2 = names(table(dat[,covari.sel[2]]))     # the names of categories of the selected second cavariate
  cats.var3 = names(table(dat[,covari.sel[3]]))     # the names of categories of the selected second cavariate
  n.subgrp.var1 = dim(table(dat[,covari.sel[1]]))   # the number of levels for the first covariate (placed in the bottom of the figure)
  n.subgrp.var2 = dim(table(dat[,covari.sel[2]]))   # the number of levels for the second covariate (placed in the left of the figure)
  n.subgrp.var3 = dim(table(dat[,covari.sel[3]]))   # the number of levels for the second covariate (placed in the left of the figure)
  n.subgrp.tol = n.subgrp.var1 * n.subgrp.var2 *  n.subgrp.var3   # the total number of subgroups

  idx1 = list()
  idx2 = list()
  idx3 = list()
  for (i in 1 : n.subgrp.var1 ) idx1[[i]] = which((dat[, covari.sel[1]] == cats.var1[i])  == T )
  for (i in 1 : n.subgrp.var2 ) idx2[[i]] = which((dat[, covari.sel[2]] == cats.var2[i])  == T )
  for (i in 1 : n.subgrp.var3 ) idx3[[i]] = which((dat[, covari.sel[3]] == cats.var3[i])  == T )
  idx.subgrp = list()                               # the index set of the subgroups
  data.subgrp = list()                              # the data set of the subgroups
  ss.subgrp = list()  # the data set of the mariginal subgroups




  #####   Produce a plot -------------------------------------------------------------------
  col =c("#80b1d3", "#fccde5")
  col.treat = rep(col,10)
  par(mar=c(0,0,0,0), xpd = TRUE)
  grid.newpage()
  ii=0
  vp <- viewport(x = sep., width = 1 - 2*sep., height = 1-4*sep.,just = c("left", "center"))
  pushViewport(vp)
  vp <- viewport(x = 2*sep., width = 1-2*sep., height = 1 - 4*sep.,just = c("left", "center"))
  pushViewport(vp)
  i=2
  #####   V1.1 -------------------------------------------------------------------
  for (i in 1:c1){
    if (i==1) {init.i = 0 + sep.*(i-1)} else {init.i = sum(w1[(i-1):1]) + sep.*(i-1)}
    vp <- viewport(x = init.i, y = 0, width = w1[i], height = 1, just = c("left", "bottom"))
    pushViewport(vp)
    # grid.rect()
    #####   V2.1.1 -------------------------------------------------------------------
    # j=2
    # f
    for (j in 1:c2){
      if (j==1) {
        init.j = 0
      } else {
        init.j = sum(w2[(i-1)*c2+(j-1):1])
      }
      vp <- viewport(x = init.j, y = 0,
                     width = w2[(i-1)*c2+j]*0.99, height = 1,
                     just = c("left", "bottom"))
      # vp <- viewport(x = c(0,1)[j], y = 0,
      #                width = w2[(i-1)*c2+j]*0.98, height = 1,
      #                just = c(c("left","right")[j], "bottom"))
      pushViewport(vp)
      # grid.rect()
      # upViewport()
      if (w2[(i-1)*c2+j] == 0) grid.rect(gp = gpar(col = "black", lty = 1, lwd = lwd.))
      k=2
      for (k in 1:c3){
        ii=ii+1
        if (k==1) {init = 0} else {init = w3[(i-1)*c2*c3+c3*(j-1)+k-1]}
        vp <- viewport(x = 0, y = c(0,1)[k],
                       width = 0.99, height = w3[(i-1)*c2*c3+c3*(j-1)+k]*0.99,
                       just = c("left", c("bottom", "top")[k]))
        pushViewport(vp)
        grid.rect(gp = gpar(fill = col.treat[ii], col = "white", lty = 1, lwd = lwd.))
        if (col.treat[ii]=="white" | is.na(col.treat[ii])) {
          if (w3[(i-1)*c2*c3+c3*(j-1)+k] == 0) {
            grid.rect(gp = gpar(col = "black",
                                lty = 1, lwd = lwd.))
          }else{
            grid.rect(gp = gpar(fill = "black", col = "white",
                                lty = 1, lwd = lwd.))
          }
        }
        if (i == 1 & j == 1) {
          grid.text(cats.var3[k], x = -0.02, vjust = 0,
                    gp = gpar(fontsize = font.size[4]), rot = 90)
        }
        upViewport()
        if (k == 1) {
          vp <- viewport(x = 0, y = 0, height = -sep., just = c("left", "bottom"))
          pushViewport(vp)
          grid.text(cats.var2[j], gp = gpar(fontsize = font.size[3]))
          # grid.text(cats.var2[j], y = -0.01, vjust = 1, gp = gpar(fontsize = font.size[3]))
          upViewport()
        }
      }
      upViewport()
      grid.text(cats.var1[i], y = 1.02, vjust = 0, gp = gpar(fontsize = font.size[3]))
    }
    upViewport()
  }
  upViewport(2)
  vp <- viewport(x = 0, y=1, width = 0.9, height = sep., just = c("left", "top"))
  pushViewport(vp)
  grid.text(labels[1], vjust = 1,  hjust = 0.5, gp = gpar(fontsize= font.size[2], fontface = 1))
  upViewport()

  vp <- viewport(x = 0, y=0, width = 0.9, height = sep., just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(labels[2], vjust = 0,  hjust = 0.5,  gp = gpar(fontsize= font.size[2], fontface = 1))
  upViewport()

  vp <- viewport(x = 0, y=0.5, width = sep., height = 1 - 4*sep., just = c("left", "center"))
  pushViewport(vp)
  grid.text(labels[3], vjust = 1,  hjust = 0.5, gp = gpar(fontsize= font.size[2], fontface = 1), rot = 90)
  upViewport()

  par(old.par)

}


#' @import grid
plot_mosaic_3 <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                          range.v = NULL, adj.ann.subgrp = 4,
                          range.strip=c(-3, 3),
                          n.brk = 30,
                          n.brk.axis = NULL,
                          font.size = c(1, 1, 0.85, 0.85, 1),
                          title = NULL, lab.xy = NULL,
                          strip = "Treatment effect size",
                          effect = "HR", lwd. = 2, sep. = 0.05,
                          show.overall = TRUE,
                          palette = "divergent", col.power = 0.5,
                          print.ss = FALSE, col.line = "white",
                          time = NULL){
  old.par <- par(no.readonly=T)
  if(n.brk%%2 == 0) n.brk = n.brk+1
  if(is.null(n.brk.axis)) n.brk.axis = n.brk
  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
  }

  # Calculate overall Treatment effect -----------------------------------------
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
  labels = names(dat)[covari.sel]
  l1 = unique(dat[, covari.sel[1]])
  l2 = unique(dat[, covari.sel[2]])
  l3 = unique(dat[, covari.sel[3]])

  n1 = table(dat[, covari.sel[1]])
  c1 = length(n1)
  n1. = sum(n1)
  w1  = n1 / n1.
  w1  = w1 * (sum(w1)-sep.*(c1-1))
  sum(w1)
  n2 = as.vector(table(dat[, covari.sel[2:1]]))
  c2 = length(table(dat[, covari.sel[2]]))
  n2. = rep(n1,each = c2)
  w2 = n2 / n2.
  w2 = w2
  n3 = as.vector(table(dat[, covari.sel[3:1]]))
  c3 = length(table(dat[, covari.sel[3]]))
  n3. = rep(n2,each = c3)
  w3 = n3 / n3.
  w3 = w3

  cats.var1 = names(table(dat[,covari.sel[1]]))     # the names of categories of the selected first cavariate
  cats.var2 = names(table(dat[,covari.sel[2]]))     # the names of categories of the selected second cavariate
  cats.var3 = names(table(dat[,covari.sel[3]]))     # the names of categories of the selected second cavariate
  n.subgrp.var1 = dim(table(dat[,covari.sel[1]]))   # the number of levels for the first covariate (placed in the bottom of the figure)
  n.subgrp.var2 = dim(table(dat[,covari.sel[2]]))   # the number of levels for the second covariate (placed in the left of the figure)
  n.subgrp.var3 = dim(table(dat[,covari.sel[3]]))   # the number of levels for the second covariate (placed in the left of the figure)
  n.subgrp.tol = n.subgrp.var1 * n.subgrp.var2 *  n.subgrp.var3   # the total number of subgroups

  idx1 = list()
  idx2 = list()
  idx3 = list()
  for (i in 1 : n.subgrp.var1 ) idx1[[i]] = which((dat[, covari.sel[1]] == cats.var1[i])  == T )
  for (i in 1 : n.subgrp.var2 ) idx2[[i]] = which((dat[, covari.sel[2]] == cats.var2[i])  == T )
  for (i in 1 : n.subgrp.var3 ) idx3[[i]] = which((dat[, covari.sel[3]] == cats.var3[i])  == T )
  idx.subgrp = list()                               # the index set of the subgroups
  data.subgrp = list()                              # the data set of the subgroups
  ss.subgrp = list()  # the data set of the mariginal subgroups


  #####   V1.1 -------------------------------------------------------------------
  ii=0
  for (i in 1:c1){
    for (j in 1:c2){
      for (k in 1:c3){
        ii = ii + 1
        # k = i + (j - 1) * n.subgrp.var2
        idx.subgrp[[ii]] =  intersect(intersect(idx1[[i]], idx2[[j]]), idx3[[k]])
        data.subgrp[[ii]] =  dat[idx.subgrp[[ii]], ]
        ss.subgrp[[ii]] = dim(data.subgrp[[ii]])[1]
      }
    }
  }


  treatment.mean = list()
  ii=0
  for (i in 1:c1){
    for (j in 1:c2){
      for (k in 1:c3){
        ii = ii + 1
        if (sum((data.subgrp[[ii]]$trt == "1")) == 0 | sum((data.subgrp[[ii]]$trt == "0")) == 0){
          treatment.mean[[ii]] = NA
        }else{
          if (outcome.type == "continuous"){
            model.int = lm(resp ~ trt,  data = data.subgrp[[ii]])
            model.sum = summary(model.int)
            treatment.mean[[ii]] = model.sum$coefficients[2, 1]
          }else if (outcome.type == "binary"){
            model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[ii]])
            model.sum = summary(model.int)
            treatment.mean[[ii]] = model.sum$coefficients[2, 1]
          }else if (outcome.type == "survival"){
            if (effect == "HR"){
              model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.subgrp[[ii]])
              model.sum = summary(model.int)
              treatment.mean[[ii]] = model.sum$coef[1, 1]
            }
            if (effect == "RMST"){
              dat.subgr.i = data.subgrp[[ii]]
              rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                    arm = dat.subgr.i$trt, tau = time)
              treatment.mean[[ii]] = rmst$unadjusted.result[1,1]
            }
          }
        }
      }
    }
  }

  pal.YlRd = colorRampPalette(c("#fee090", "#d73027"), space = "rgb")
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
  col.treat = vector()
  treat_size = unlist(treatment.mean)
  for (i in 1 : (n.subgrp.tol)){
    if (is.na(treat_size[i]))
    {
      col.treat[i] = "white"
    } else {
      col.idx = which(treat_size[i] >= breaks[-length(breaks)])
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]
    }
  }
  cat("The minimum of treatment effect sizes is", min(unlist(treatment.mean), na.rm = T), "\n")
  cat("The maximum of treatment effect sizes is", max(unlist(treatment.mean), na.rm = T), "\n")

  if(any(is.na(col.treat))) {
    warning("Check strip range")
    warning("The minimum of treatment effect sizes is ", min(unlist(treatment.mean), na.rm = T), "\n")
    warning("The maximum of treatment effect sizes is ", max(unlist(treatment.mean), na.rm = T), "\n")
  }

  #####   Produce a plot -------------------------------------------------------------------
  par(mar=c(0,0,0,0), xpd = TRUE)
  grid.newpage()
  ii=0
  vp <- viewport(x = 0, width = 0.8, height = 1,just = c("left", "center"))
  pushViewport(vp)
  vp <- viewport(x = 2*sep., width = 1-3*sep., height = 1 - 4*sep.,just = c("left", "center"))
  pushViewport(vp)
  i=2
  #####   V1.1 -----------------------------------------------------------------
  for (i in 1:c1){
    if (i==1) {init.i = 0 + sep.*(i-1)} else {init.i = sum(w1[(i-1):1]) + sep.*(i-1)}
    vp <- viewport(x = init.i, y = 0, width = w1[i], height = 1, just = c("left", "bottom"))
    pushViewport(vp)
    #####   V2.1.1 -------------------------------------------------------------
    for (j in 1:c2){
      if (j==1) {
        init.j = 0
      } else {
        init.j = sum(w2[(i-1)*c2+(j-1):1])
      }
      vp <- viewport(x = init.j, y = 0,
                     width = w2[(i-1)*c2+j]*0.99, height = 1,
                     just = c("left", "bottom"))
      pushViewport(vp)
      if (w2[(i-1)*c2+j] == 0) grid.rect(gp = gpar(col = "black", lty = 1, lwd = lwd.))
      k=2
      for (k in 1:c3){
        ii=ii+1
        if (k==1) {init = 0} else {init = w3[(i-1)*c2*c3+c3*(j-1)+k-1]}
        vp <- viewport(x = 0, y = c(0,1)[k],
                       width = 0.99, height = w3[(i-1)*c2*c3+c3*(j-1)+k]*0.99,
                       just = c("left", c("bottom", "top")[k]))
        pushViewport(vp)
        grid.rect(gp = gpar(fill = col.treat[ii], col = "white", lty = 1, lwd = lwd.))
        if (col.treat[ii]=="white" | is.na(col.treat[ii])) {
          if (w3[(i-1)*c2*c3+c3*(j-1)+k] == 0) {
            grid.rect(gp = gpar(col = "black",
                                lty = 1, lwd = lwd.))
          }else{
            grid.rect(gp = gpar(fill = "black", col = "white",
                                lty = 1, lwd = lwd.))
          }
        }
        if (i == 1 & j == 1) {
          grid.text(cats.var3[k], x = -0.02, vjust = 0,
                    gp = gpar(fontsize = font.size[3]), rot = 90)
        }
        upViewport()
        if (k == 1) {
          vp <- viewport(x = 0, y = 0, height = -sep., just = c("left", "bottom"))
          pushViewport(vp)
          grid.text(cats.var2[j], gp = gpar(fontsize = font.size[3]))
          upViewport()
        }
      }
      upViewport()
      grid.text(cats.var1[i], y = 1.02, vjust = 0, gp = gpar(fontsize = font.size[3]))
    }
    upViewport()
  }
  upViewport(2)
  vp <- viewport(x = 0, y=1, width = 0.9, height = sep., just = c("left", "top"))
  pushViewport(vp)
  grid.text(labels[1], vjust = 0.5,  hjust = 0.5, gp = gpar(fontsize= font.size[2], fontface = 1))
  upViewport()

  vp <- viewport(x = 0, y=0, width = 0.9, height = sep., just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(labels[2], vjust = 0.5,  hjust = 0.5,  gp = gpar(fontsize= font.size[2], fontface = 1))
  upViewport()

  vp <- viewport(x = 0, y=0.5, width = sep., height = 1 - 4*sep., just = c("left", "center"))
  pushViewport(vp)
  grid.text(labels[3], vjust = 0.5,  hjust = 0.5, gp = gpar(fontsize= font.size[2], fontface = 1), rot = 90)
  upViewport()


  #####  produce legend --------------------------------------------------------
  vp <- viewport(x = 0.75, width = 0.25, height = 1,just = c("left", "center"))
  pushViewport(vp)

  vp <- viewport(x = 0.4, width = 0.2, height = 1 - 4*sep., just = c("left", "center"))
  pushViewport(vp)

  col.bar.height = 1/ length(col.vec)
  for (i in 1 : length(col.vec)){
    vp <- viewport(x = 0 , y = 0 + (i-1) * col.bar.height, width=1, height=col.bar.height,  just = c("left", "bottom"))
    pushViewport(vp)
    grid.rect(gp = gpar(fill = col.vec[i], col = NA))
    upViewport()
  }

  grid.yaxis(seq(0, 1, len = n.brk.axis), vp = viewport(y = 0.5),
             label = seq(min(range.strip), max(range.strip), len = n.brk.axis),
             gp = gpar(cex = font.size[5], lwd = 0),
             edits = gEdit(gPath="labels", rot=90, hjust = 0.5, vjust = 0.5))
  if(show.overall){
    cat("Overall Treatment effect is:",
        overall.treatment.mean, ", with confidence interval: (",
        overall.treatment.lower,";",overall.treatment.upper,")")
    grid.points(x = 0.5, (overall.treatment.mean / (range.strip[2]-range.strip[1])) + 0.5, pch = 20)
    grid.points(x = 0.5, (overall.treatment.lower / (range.strip[2]-range.strip[1])) + 0.5, pch = "-")
    grid.points(x = 0.5, (overall.treatment.upper / (range.strip[2]-range.strip[1])) + 0.5, pch = "-")
    grid.segments(x0 = 0.5, x1 = 0.5,
                  y0 = (overall.treatment.lower/ (range.strip[2]-range.strip[1])) + 0.5,
                  y1 = (overall.treatment.upper/ (range.strip[2]-range.strip[1])) + 0.5)
  }
  upViewport()
  vp <- viewport(x = 0.6, width = 0.4, height = 1 - 4*sep., just = c("left", "center"))
  pushViewport(vp)
  grid.text(strip, gp = gpar(fontsize= font.size[2], fontface = 1), rot = 90)
  upViewport()
  upViewport()
  par(old.par)
}
