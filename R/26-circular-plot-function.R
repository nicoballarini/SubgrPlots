#' Circular plot using circlize package
#'
#' This function produces a circular plot for subgroup analysis
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
# created by Nico, 12/03/18
#' @export
plot_circle <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                        range.v = NULL, adj.ann.subgrp = 4,
                        range.strip=c(-3, 3),
                        n.brk = 31,
                        n.brk.axis = NULL,
                        font.size = c(1, 1, 0.85, 0.85, 1),
                        title = NULL, lab.xy = NULL,
                        strip = "Treatment effect size",
                        effect = "HR",
                        equal.width = TRUE,
                        show.KM = FALSE,
                        show.effect = TRUE,
                        conf.int = TRUE, show.overall = TRUE, palette = "divergent", col.power = 0.5)
{
  ################################################ 1. create subgroup data  #################################################################
  library(survival)
  library(grid)
  library(circlize)
  if(n.brk%%2 == 0) n.brk = n.brk+1
  if(is.null(n.brk.axis)) n.brk.axis = n.brk
  n.covari = length(covari.sel)
  lab.vars = names(dat)[covari.sel]                          # set the names of the covariates which relates to the defined subgroup; if a covariate
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
  for (i in 1: length(covari.sel)){
    cond = covari.sel == covari.sel[[i]]
    lab.vars[cond] = rep(lab.vars[i], length(which(cond == TRUE)))
  }
  cats.var = list()
  n.subgrp.tol = 0
  for (i in 1 : length(covari.sel)){
    cats.var[[i]] = names(table(dat[,covari.sel[i]]))
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])      # the number of the subgroups (excluding the complement)
  }

  cond = list()
  data.subgrp = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)
  ss.subgrp.T = matrix(0, nrow = n.subgrp.tol)
  ss.subgrp.C = matrix(0, nrow = n.subgrp.tol)
  k = 0
  for (i in 1 : length(covari.sel)) {
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      cond[[k]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
      ss.subgrp[k, k] = length(cond[[k]])
      data.subgrp[[k]] = dat[cond[[k]], ]
      ss.subgrp.T[k] = length(which(data.subgrp[[k]]$trt == 1))
      ss.subgrp.C[k] = length(which(data.subgrp[[k]]$trt == 0))
    }
  }

  k = n.subgrp.tol
  r.prop = diag(n.subgrp.tol)
  for (i in 1 : (n.subgrp.tol - 1) ){
    for (j in (i + 1) : (n.subgrp.tol) ){
      k = k + 1
      cond[[k]] = intersect(cond[[i]], cond[[j]])
      ss.subgrp[i, j] = length(cond[[k]])
      ss.subgrp[j, i] = length(cond[[k]])
      r.prop[i, j] = ss.subgrp[i, j] / ss.subgrp[i, i]
      r.prop[j, i] = ss.subgrp[j, i] / ss.subgrp[j, j]
    }
  }

  # create matrices for treatment size and standard error of MLE
  # set estimate (mean, 95% C.I. bounds) of the control group and the treatment group for each subgroup

  plot.data = list()
  treatment.mean = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1 , ncol = 1)
  treatment.upper = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.lower = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.C.mean = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol+ 1, ncol = 1)
  treatment.C.upper = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol+ 1, ncol = 1)
  treatment.C.lower = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.mean = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1 , ncol = 1)
  treatment.T.upper = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.lower = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  for (i in 1 : n.subgrp.tol){

    if (sum((data.subgrp[[i]]$trt == "1")) == 0 | sum((data.subgrp[[i]]$trt == "0")) == 0){
      treatment.mean[i] = NA
      treatment.upper[i] = NA
      treatment.lower[i] = NA
    }else{

      if (outcome.type == "continuous"){

        model.int = lm(resp ~ trt,  data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.upper[i] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
        treatment.lower[i] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
          treatment.C.upper[i] = NA
          treatment.C.lower[i] = NA
        }else{
          model.int = lm(resp ~ 1,  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = model.sum$coefficients[1, 1]
          treatment.C.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.C.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA
          treatment.T.upper[i] = NA
          treatment.T.lower[i] = NA
        }else{
          model.int = lm(resp ~ 1,  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] = model.sum$coefficients[1, 1]
          treatment.T.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.T.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

      }else if (outcome.type == "binary"){

        model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.upper[i] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
        treatment.lower[i] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
          treatment.C.upper[i] = NA
          treatment.C.lower[i] = NA
        }else{
          model.int = glm(resp ~ 1, family = "binomial",  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = model.sum$coefficients[1, 1]
          treatment.C.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.C.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA
          treatment.T.upper[i] = NA
          treatment.T.lower[i] = NA
        }else{
          model.int = glm(resp ~ 1, family = "binomial",  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] = model.sum$coefficients[1, 1]
          treatment.T.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.T.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

      }else if (outcome.type == "survival"){

        model.int = coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.upper[i] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
        treatment.lower[i] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

        surv.fit = survfit(Surv(time, status) ~ trt, data = data.subgrp[[i]])

        plot.data[[i]] = surv.fit
      }
    }
  }

  # set estimate (mean, 95% C.I. bounds) of the control group and the treatment group for the full population

  if (sum((dat$trt == "1")) == 0 | sum((dat$trt == "0")) == 0){
    treatment.mean[n.subgrp.tol + 1] = NA
    treatment.upper[n.subgrp.tol + 1] = NA
    treatment.lower[n.subgrp.tol + 1] = NA
  }else{

    if (outcome.type == "continuous"){

      model.int = lm(resp ~ trt,  data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coefficients[2, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
        treatment.C.upper[n.subgrp.tol + 1] = NA
        treatment.C.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = lm(resp ~ 1,  data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.C.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.C.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
        treatment.T.upper[n.subgrp.tol + 1] = NA
        treatment.T.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = lm(resp ~ 1,  data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.T.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.T.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

    }else if (outcome.type == "binary"){

      model.int = glm(resp ~ trt, family = "binomial", data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coefficients[2, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
        treatment.C.upper[n.subgrp.tol + 1] = NA
        treatment.C.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = glm(resp ~ 1, family = "binomial",  data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.C.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.C.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
        treatment.T.upper[n.subgrp.tol + 1] = NA
        treatment.T.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = glm(resp ~ 1, family = "binomial",  data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.T.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.T.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

    }else if (outcome.type == "survival"){

      model.int = coxph(Surv(time, status) ~ trt, data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coef[1, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]
    }
  }

  lab.subgrp = vector()
  k = 0
  for (i in 1: length(covari.sel)){
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      # lab.subgrp[k] = paste0("(", LETTERS[i], j, ") ", lab.vars[i], "=", cats.var[[i]][j], sep = "")
      lab.subgrp[k] = paste0(lab.vars[i], "=", cats.var[[i]][j], sep = "")
    }
  }
  lab.subgrp[n.subgrp.tol + 1] = "Full"

  est.range = cbind.data.frame(treatment.mean, treatment.lower, treatment.upper )
  # est.C.range = cbind.data.frame(treatment.C.mean, treatment.C.lower, treatment.C.upper )
  # est.T.range = cbind.data.frame(treatment.T.mean, treatment.T.lower, treatment.T.upper )
  dimnames(est.range) = list(c(lab.subgrp), c("mean", "lower","upper") )

  ################################################ 2. create plots #################################################################

  layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(7,1))
  # layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(1,1))
  par(mar=c(1,1,3,1), xpd = TRUE)
  ##########   middle-middle cell
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
  col.treat = col.upper = col.lower = vector()
  treat_size = c((treatment.mean))

  for (i in 1 : (n.subgrp.tol)){
    if (is.na(treat_size[i]))
    {
      col.treat[i] = "white"
      col.upper[i] = col.lower[i] ="white"
    }else{
      col.idx = which(treat_size[i] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]

      col.idx = which(c(treatment.upper)[i] >= breaks)
      col.idx = max(col.idx)
      col.upper[i] = col.vec[col.idx]

      col.idx = which(c(treatment.lower)[i] >= breaks)
      col.idx = max(col.idx)
      col.lower[i] = col.vec[col.idx]
    }
  }


  max.time = max(dat$time)
  circos.par(cell.padding = c(0,0,0,0), track.margin = c(0, 0))
  if (equal.width){
    circos.initialize(lab.subgrp[1:n.subgrp.tol],
                      xlim = cbind(rep(0, n.subgrp.tol), rep(max.time, n.subgrp.tol)))
  } else {
    circos.initialize(lab.subgrp[1:n.subgrp.tol],
                      xlim = cbind(rep(0, n.subgrp.tol), rep(max.time, n.subgrp.tol)),
                      sector.width = diag(ss.subgrp))
  }


  if (show.KM){
    circos.track(ylim = c(0, 1), track.height = uh(15, "mm"),
                 panel.fun = function(x, y) {
                   circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(3, "mm"),
                               CELL_META$sector.index)
                   # circos.lines(c(0, 0 + ux(5, "mm")), c(0.5, 0.5), col = "blue")
                 })
    for (i in 1:(length(lab.subgrp)-1)){
      factor = lab.subgrp[i]
      n.strata = plot.data[[i]]$strata
      time. = plot.data[[i]]$time
      surv. = plot.data[[i]]$surv
      surv.0 = c(1, 1, rep(surv.[1:n.strata[1]], each=2))
      time.0 = c(0, rep(time.[1:n.strata[1]], each=2), time.[n.strata[1]])
      surv.1 = c(1, 1, rep(surv.[(n.strata[1]+1):(n.strata[1]+n.strata[2])], each=2))
      time.1 = c(0, rep(time.[(n.strata[1]+1):(n.strata[1]+n.strata[2])], each=2), time.[(n.strata[1]+n.strata[2])])
      # plot(range(time.), c(0,1), type = "n", xaxs="i", yaxs="i")
      # lines(time.0, surv.0)
      # lines(time.1, surv.1)
      circos.lines(time.0, surv.0, sector.index = factor, col = col.line[1])#, track.index = 1)
      circos.lines(time.1, surv.1, sector.index = factor, col = col.line[2])#, track.index = 1)
    }
  }



  if (show.effect & conf.int == FALSE){
    if(show.KM){
      circos.track(ylim = c(0, 1), track.height = uh(5, "mm"),
                   panel.fun = function(x, y) {})
    } else {
      circos.track(ylim = c(0, 1), track.height = uh(5, "mm"),
                   panel.fun = function(x, y) {
                     circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(3, "mm"),
                                 CELL_META$sector.index)
                   })

    }
    for (i in 1:(length(lab.subgrp)-1)){
      factor = lab.subgrp[i]
      circos.update(sector.index = factor, #track.index = 2,
                    bg.col = col.treat[i], bg.border = "black")
    }
  }


  if (!show.KM & show.effect & conf.int){
    circos.track(ylim = c(0, 1), track.height = uh(2.5, "mm"),
                 panel.fun = function(x, y) {
                   circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(3, "mm"),
                               CELL_META$sector.index)
                 })
    circos.track(ylim = c(0, 1), track.height = uh(5, "mm"),
                 panel.fun = function(x, y) {
                 })
    circos.track(ylim = c(0, 1), track.height = uh(2.5, "mm"),
                 panel.fun = function(x, y) {
                 })
    for (i in 1:(length(lab.subgrp)-1)){
      factor = lab.subgrp[i]
      circos.update(sector.index = factor, track.index = 1,
                    bg.col = col.upper[i], bg.border = "black")
      circos.update(sector.index = factor, track.index = 2,
                    bg.col = col.treat[i], bg.border = "black")
      circos.update(sector.index = factor, track.index = 3,
                    bg.col = col.lower[i], bg.border = "black")
    }
  }

  rownames(ss.subgrp) = lab.subgrp[1:n.subgrp.tol]
  colnames(ss.subgrp) = lab.subgrp[1:n.subgrp.tol]
  rownames(r.prop) = lab.subgrp[1:n.subgrp.tol]
  colnames(r.prop) = lab.subgrp[1:n.subgrp.tol]
  mat2 = r.prop
  mat = ss.subgrp
  mat[lower.tri(mat)] <- 0
  diag(mat2) <- 0
  factor1 = lab.subgrp[1]
  factor2 = lab.subgrp[2]
  # circos.link(factor1,c(1,1),factor2,c(1, 1))
  # circos.link(factor1,max.time,factor2,max.time)

  rowSums(mat2)
  colSums(mat2)
  mat2
  i=1
  j=3
  # col_mat = rand_color(length(mat), hue = "yellow", luminosity = "bright",
  #                      transparency = 0.5)
  # col_mat = rand_color(4, hue = "yellow", luminosity = "bright",
  #                      transparency = 0.5)
  # cols = c('#e41a1c','#984ea3','#984ea3','#a6d854')
  # cols = col_mat[1:4]
  col_mat = rep(add_transparency(c('#fee391','#fec44f','#fe9929'), 0.5),
                (dim(mat)[1]*dim(mat)[2])/3)
  dim(col_mat) = dim(mat)  # to make sure it is a matrix

  mat3 = cbind(rep(0, n.subgrp.tol), mat2)
  i=1
  j=4
  for (i in 1:(n.subgrp.tol-1)){
    for (j in (i+1):n.subgrp.tol){
      # print(mat2[i, (j)])

      if(mat2[i, (j)] == 0) next()
      begin.1 = sum(mat2[i, 1:(j-1)])/(n.covari-1)*max.time
      end.1   = sum(mat2[i, 1:j])/(n.covari-1)*max.time
      begin.2 = sum(mat3[j, 1:i])/(n.covari-1)*max.time
      end.2   = sum(mat3[j, 1:(i+1)])/(n.covari-1)*max.time

      circos.link(lab.subgrp[i],
                  c(begin.1, end.1),
                  lab.subgrp[j],
                  c(begin.2, end.2),w = 2,
                  col = col_mat[i, i])
    }
  }
  par(mar=c(1,2, 2, 2))
  par(mar=c(3,1, 5, 1), xpd = FALSE)
  SubgrPlots:::image.scale(treatmeant.mean,
                           col= col.vec,
                           breaks = breaks,
                           axis.pos = 4, add.axis = FALSE)

  axis(2,
       at = seq(min(range.strip), max(range.strip), len = n.brk.axis),
       labels = seq(min(range.strip), max(range.strip), len = n.brk.axis),
       las = 0, cex.axis = font.size[4])
  # box()
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

  # abline(h=breaks)
  title(ylab = strip, cex.lab = font.size[4])

  mtext(side = 4, text = strip, cex.lab = font.size[4], las = 3)
}














plot_circle_std <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                        range.v = NULL, adj.ann.subgrp = 4,
                        range.strip=c(-3, 3),
                        n.brk = 31,
                        n.brk.axis = NULL,
                        font.size = c(1, 1, 0.85, 0.85, 1),
                        title = NULL, lab.xy = NULL,
                        strip = "Treatment effect size",
                        effect = "HR",
                        equal.width = TRUE,
                        show.KM = FALSE,
                        show.effect = TRUE,
                        conf.int = TRUE, palette = "divergent", col.power = 0.5)
{
  ################################################ 1. create subgroup data  #################################################################
  library(survival)
  library(grid)
  library(circlize)
  if(n.brk%%2 == 0) n.brk = n.brk+1
  if(is.null(n.brk.axis)) n.brk.axis = n.brk
  n.covari = length(covari.sel)
  lab.vars = names(dat)[covari.sel]                          # set the names of the covariates which relates to the defined subgroup; if a covariate
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
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])      # the number of the subgroups (excluding the complement)
  }

  cond = list()
  data.subgrp = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)
  ss.subgrp.T = matrix(0, nrow = n.subgrp.tol)
  ss.subgrp.C = matrix(0, nrow = n.subgrp.tol)
  k = 0
  for (i in 1 : length(covari.sel)) {
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      cond[[k]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
      ss.subgrp[k, k] = length(cond[[k]])
      data.subgrp[[k]] = dat[cond[[k]], ]
      ss.subgrp.T[k] = length(which(data.subgrp[[k]]$trt == 1))
      ss.subgrp.C[k] = length(which(data.subgrp[[k]]$trt == 0))
    }
  }

  k = n.subgrp.tol
  r.prop = diag(n.subgrp.tol)
  for (i in 1 : (n.subgrp.tol - 1) ){
    for (j in (i + 1) : (n.subgrp.tol) ){
      k = k + 1
      cond[[k]] = intersect(cond[[i]], cond[[j]])
      ss.subgrp[i, j] = length(cond[[k]])
      ss.subgrp[j, i] = length(cond[[k]])
      r.prop[i, j] = ss.subgrp[i, j] / ss.subgrp[i, i]
      r.prop[j, i] = ss.subgrp[j, i] / ss.subgrp[j, j]
    }
  }

  # create matrices for treatment size and standard error of MLE
  # set estimate (mean, 95% C.I. bounds) of the control group and the treatment group for each subgroup

  plot.data = list()
  treatment.mean = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1 , ncol = 1)
  treatment.upper = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.lower = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.C.mean = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol+ 1, ncol = 1)
  treatment.C.upper = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol+ 1, ncol = 1)
  treatment.C.lower = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.mean = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1 , ncol = 1)
  treatment.T.upper = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.lower = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  for (i in 1 : n.subgrp.tol){

    if (sum((data.subgrp[[i]]$trt == "1")) == 0 | sum((data.subgrp[[i]]$trt == "0")) == 0){
      treatment.mean[i] = NA
      treatment.upper[i] = NA
      treatment.lower[i] = NA
    }else{

      if (outcome.type == "continuous"){

        model.int = lm(resp ~ trt,  data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.upper[i] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
        treatment.lower[i] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
          treatment.C.upper[i] = NA
          treatment.C.lower[i] = NA
        }else{
          model.int = lm(resp ~ 1,  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = model.sum$coefficients[1, 1]
          treatment.C.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.C.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA
          treatment.T.upper[i] = NA
          treatment.T.lower[i] = NA
        }else{
          model.int = lm(resp ~ 1,  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] = model.sum$coefficients[1, 1]
          treatment.T.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.T.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

      }else if (outcome.type == "binary"){

        model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.upper[i] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
        treatment.lower[i] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
          treatment.C.upper[i] = NA
          treatment.C.lower[i] = NA
        }else{
          model.int = glm(resp ~ 1, family = "binomial",  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = model.sum$coefficients[1, 1]
          treatment.C.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.C.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA
          treatment.T.upper[i] = NA
          treatment.T.lower[i] = NA
        }else{
          model.int = glm(resp ~ 1, family = "binomial",  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] = model.sum$coefficients[1, 1]
          treatment.T.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.T.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

      }else if (outcome.type == "survival"){

        model.int = coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.upper[i] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
        treatment.lower[i] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

        surv.fit = survfit(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        difference <- summary(surv.fit, time=time)

        plot.data[[i]] = surv.fit
        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
          treatment.C.upper[i] = NA
          treatment.C.lower[i] = NA
        }else{
          model.int = coxph(Surv(time, status) ~ 1, data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = difference$surv[1]
          treatment.C.upper[i] = difference$upper[1]
          treatment.C.lower[i] = difference$lower[1]
        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA
          treatment.T.upper[i] = NA
          treatment.T.lower[i] = NA
        }else{
          model.int = coxph(Surv(time, status) ~ 1, data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] =  difference$surv[2]
          treatment.T.upper[i] = difference$upper[2]
          treatment.T.lower[i] = difference$lower[2]
        }
      }
    }
  }

  # set estimate (mean, 95% C.I. bounds) of the control group and the treatment group for the full population

  if (sum((dat$trt == "1")) == 0 | sum((dat$trt == "0")) == 0){
    treatment.mean[n.subgrp.tol + 1] = NA
    treatment.upper[n.subgrp.tol + 1] = NA
    treatment.lower[n.subgrp.tol + 1] = NA
  }else{

    if (outcome.type == "continuous"){

      model.int = lm(resp ~ trt,  data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coefficients[2, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
        treatment.C.upper[n.subgrp.tol + 1] = NA
        treatment.C.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = lm(resp ~ 1,  data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.C.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.C.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
        treatment.T.upper[n.subgrp.tol + 1] = NA
        treatment.T.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = lm(resp ~ 1,  data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.T.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.T.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

    }else if (outcome.type == "binary"){

      model.int = glm(resp ~ trt, family = "binomial", data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coefficients[2, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
        treatment.C.upper[n.subgrp.tol + 1] = NA
        treatment.C.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = glm(resp ~ 1, family = "binomial",  data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.C.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.C.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
        treatment.T.upper[n.subgrp.tol + 1] = NA
        treatment.T.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = glm(resp ~ 1, family = "binomial",  data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.T.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.T.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

    }else if (outcome.type == "survival"){

      model.int = coxph(Surv(time, status) ~ trt, data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coef[1, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

      surv.fit = survfit(Surv(time, status) ~ trt, data = dat)
      difference <- summary(surv.fit, time=time)
      plot.data[[n.subgrp.tol + 1]] = surv.fit
      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
        treatment.C.upper[n.subgrp.tol + 1] = NA
        treatment.C.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = coxph(Surv(time, status) ~ 1, data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1]  = difference$surv[1]
        treatment.C.upper[n.subgrp.tol + 1] = difference$upper[1]
        treatment.C.lower[n.subgrp.tol + 1] = difference$lower[1]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
        treatment.T.upper[n.subgrp.tol + 1] = NA
        treatment.T.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = coxph(Surv(time, status) ~ 1, data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1]  = difference$surv[2]
        treatment.T.upper[n.subgrp.tol + 1] = difference$upper[2]
        treatment.T.lower[n.subgrp.tol + 1] = difference$lower[2]
      }
    }
  }

  lab.subgrp = vector()
  k = 0
  for (i in 1: length(covari.sel)){
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      # lab.subgrp[k] = paste0("(", LETTERS[i], j, ") ", lab.vars[i], "=", cats.var[[i]][j], sep = "")
      lab.subgrp[k] = paste0(lab.vars[i], "=", cats.var[[i]][j], sep = "")
    }
  }
  lab.subgrp[n.subgrp.tol + 1] = "Full"

  est.range = cbind.data.frame(treatment.mean, treatment.lower, treatment.upper )
  est.C.range = cbind.data.frame(treatment.C.mean, treatment.C.lower, treatment.C.upper )
  est.T.range = cbind.data.frame(treatment.T.mean, treatment.T.lower, treatment.T.upper )
  dimnames(est.range) = list(c(lab.subgrp), c("mean", "lower","upper") )

  ################################################ 2. create plots #################################################################

  layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(7,1))
  # layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(1,1))
  par(mar=c(1,1,3,1), xpd = TRUE)
  ##########   middle-middle cell
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
  col.treat = col.upper = col.lower = vector()
  treat_size = c((treatment.mean))

  for (i in 1 : (n.subgrp.tol)){
    if (is.na(treat_size[i]))
    {
      col.treat[i] = "white"
      col.upper[i] = col.lower[i] ="white"
    }else{
      col.idx = which(treat_size[i] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]

      col.idx = which(c(treatment.upper)[i] >= breaks)
      col.idx = max(col.idx)
      col.upper[i] = col.vec[col.idx]

      col.idx = which(c(treatment.lower)[i] >= breaks)
      col.idx = max(col.idx)
      col.lower[i] = col.vec[col.idx]
    }
  }


  circos.clear()
  circos.par(cell.padding = c(0,0,0,0), track.margin = c(0, 0))
  if (equal.width){
    circos.initialize(lab.subgrp[1:n.subgrp.tol],
                      xlim = cbind(rep(0, n.subgrp.tol), rep(1, n.subgrp.tol)))
  } else {
    circos.initialize(lab.subgrp[1:n.subgrp.tol],
                      xlim = cbind(rep(0, n.subgrp.tol), rep(1, n.subgrp.tol)),
                      sector.width = diag(ss.subgrp))
  }





  if (show.effect & conf.int == FALSE){
      circos.track(ylim = c(0, 1), track.height = uh(5, "mm"),
                   panel.fun = function(x, y) {
                     circos.text(CELL_META$xcenter,
                                 CELL_META$cell.ylim[2] + uy(3, "mm"),
                                 CELL_META$sector.index)
                   })
    for (i in 1:(length(lab.subgrp)-1)){
      factor = lab.subgrp[i]
      circos.update(sector.index = factor, #track.index = 2,
                    bg.col = col.treat[i], bg.border = "black")
    }
  }


  if (!show.KM & show.effect & conf.int){
    circos.track(ylim = c(0, 1), track.height = uh(2.5, "mm"),
                 panel.fun = function(x, y) {
                   circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(3, "mm"),
                               CELL_META$sector.index)
                 })
    circos.track(ylim = c(0, 1), track.height = uh(5, "mm"),
                 panel.fun = function(x, y) {
                 })
    circos.track(ylim = c(0, 1), track.height = uh(2.5, "mm"),
                 panel.fun = function(x, y) {
                 })
    for (i in 1:(length(lab.subgrp)-1)){
      factor = lab.subgrp[i]
      circos.update(sector.index = factor, track.index = 1,
                    bg.col = col.upper[i], bg.border = "black")
      circos.update(sector.index = factor, track.index = 2,
                    bg.col = col.treat[i], bg.border = "black")
      circos.update(sector.index = factor, track.index = 3,
                    bg.col = col.lower[i], bg.border = "black")
    }
  }

  rownames(ss.subgrp) = lab.subgrp[1:n.subgrp.tol]
  colnames(ss.subgrp) = lab.subgrp[1:n.subgrp.tol]
  rownames(r.prop) = lab.subgrp[1:n.subgrp.tol]
  colnames(r.prop) = lab.subgrp[1:n.subgrp.tol]
  mat2 = r.prop
  mat = ss.subgrp
  mat[lower.tri(mat)] <- 0
  diag(mat2) <- 0
  factor1 = lab.subgrp[1]
  factor2 = lab.subgrp[2]
  # circos.link(factor1,c(1,1),factor2,c(1, 1))
  # circos.link(factor1,max.time,factor2,max.time)

  rowSums(mat2)
  colSums(mat2)
  mat2
  i=1
  j=3
  col_mat = rand_color(length(mat), transparency = 0.5)
  dim(col_mat) = dim(mat)  # to make sure it is a matrix

  mat3 = cbind(rep(0, n.subgrp.tol), mat2)
  # for (i in 1:(n.subgrp.tol-1)){
  #   for (j in (i+1):n.subgrp.tol){
  #     # print(mat2[i, (j)])
  #     if(mat2[i, (j)] == 0) next()
  #     circos.link(lab.subgrp[i],
  #                 c(sum(mat2[i, 1:(j-1)]), sum(mat2[i, 1:j]))/(n.covari-1)*max.time,
  #                 lab.subgrp[j],
  #                 c(sum(mat3[j, 1:i]) + sum(),   sum(mat3[j, 1:(i+1)]))/(n.covari-1)*max.time,
  #                 # col = "#1A030780")
  #                 col = col_mat[i, i])
  #   }
  # }
  # i=1
  # j=3
  # for (i in 1:(n.subgrp.tol-1)){
  #   for (j in (i+1):n.subgrp.tol){
  #     # print(mat2[i, (j)])
  #
  #     if(mat2[i, (j)] == 0) next()
  #     begin.1 = sum(mat2[i, 1:(j-1)])
  #     end.1   = sum(mat2[i, 1:j])
  #     begin.2 = sum(mat3[j, 1:i])
  #     end.2   = sum(mat3[j, 1:(i+1)])
  #
  #     circos.link(lab.subgrp[i],
  #                 c(begin.1, end.1),
  #                 lab.subgrp[j],
  #                 c(begin.2, end.2),
  #                 col = col_mat[i, i])
  #   }
  # }

  ii = 0
  var.ind = unlist(lapply(1:n.covari, function(x) rep(x, each = length(cats.var[[x]]))))
  var.change = var.ind - dplyr::lag(var.ind)
  # var.change[1] = 1
  var.change[1] = 0
  for (i in 1:n.covari){
    for (j in 1:length(cats.var[[i]])){
  # i=1
  # j=1
  # k=4
  # ss.subgrp
  # for (i in 1:1){
  #   for (j in 1:1){
          ii = ii + 1
      for (k in (ii):(n.subgrp.tol)){
        if(mat2[ii, k] == 0) next()
        pos.i = ii
        pos.j = k

        var.change[k]

        begin.1 = sum(mat2[pos.i, 1:(pos.j-1)]) - sum(var.change[k:1]) + 1
        end.1   = sum(mat2[pos.i, 1:pos.j]) - sum(var.change[k:1])+ 1
        begin.2 = sum(mat3[pos.j, 1:pos.i]) - sum(var.change[ii:1])
        end.2   = sum(mat3[pos.j, 1:(pos.i+1)]) - sum(var.change[ii:1])

      circos.link(lab.subgrp[ii],
                  c(begin.1, end.1),
                  lab.subgrp[k],
                  c(begin.2, end.2),
                  col = col_mat[ii, k])
      }
    }
  }




  par(mar=c(1,2, 2, 2))
  par(mar=c(5,1, 5, 1), xpd = FALSE)
  SubgrPlots:::image.scale(treatmeant.mean, col= col.vec, breaks = breaks, axis.pos = 4, add.axis = FALSE)
  axis(2, at = breaks, labels = round(breaks, 3), las = 0, cex.axis = font.size[4])
  box()

  abline(h=breaks)
  title(ylab = strip, cex.lab = font.size[4])

  mtext(side = 4, text = strip, cex.lab = font.size[4], las = 3)
}

plot_circle_std_by <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                            range.v = NULL, adj.ann.subgrp = 4,
                            range.strip=c(-3, 3),
                            n.brk = 31,
                            n.brk.axis = NULL,
                            font.size = c(1, 1, 0.85, 0.85, 1),
                            title = NULL, lab.xy = NULL,
                            strip = "Treatment effect size",
                            effect = "HR",
                            equal.width = TRUE,
                            show.KM = FALSE,
                            show.effect = TRUE,
                            conf.int = TRUE, palette = "divergent", col.power = 0.5)
{
  ################################################ 1. create subgroup data  #################################################################
  library(survival)
  library(grid)
  library(circlize)
  if(n.brk%%2 == 0) n.brk = n.brk+1
  if(is.null(n.brk.axis)) n.brk.axis = n.brk
  n.covari = length(covari.sel)
  lab.vars = names(dat)[covari.sel]                          # set the names of the covariates which relates to the defined subgroup; if a covariate
  # are considered for multiple times, we make their name identical. (otherwise, the resulsting
  # names are like var, var.1, var.2 and so on.)
time = 50
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
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])      # the number of the subgroups (excluding the complement)
  }

  cond = list()
  data.subgrp = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)
  ss.subgrp.T = matrix(0, nrow = n.subgrp.tol)
  ss.subgrp.C = matrix(0, nrow = n.subgrp.tol)
  k = 0
  for (i in 1 : length(covari.sel)) {
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      cond[[k]] = which((dat[, covari.sel[i]] == cats.var[[i]][j])  == T )
      ss.subgrp[k, k] = length(cond[[k]])
      data.subgrp[[k]] = dat[cond[[k]], ]
      ss.subgrp.T[k] = length(which(data.subgrp[[k]]$trt == 1))
      ss.subgrp.C[k] = length(which(data.subgrp[[k]]$trt == 0))
    }
  }

  k = n.subgrp.tol
  r.prop = diag(n.subgrp.tol)
  for (i in 1 : (n.subgrp.tol - 1) ){
    for (j in (i + 1) : (n.subgrp.tol) ){
      k = k + 1
      cond[[k]] = intersect(cond[[i]], cond[[j]])
      ss.subgrp[i, j] = length(cond[[k]])
      ss.subgrp[j, i] = length(cond[[k]])
      r.prop[i, j] = ss.subgrp[i, j] / ss.subgrp[i, i]
      r.prop[j, i] = ss.subgrp[j, i] / ss.subgrp[j, j]
    }
  }

  # create matrices for treatment size and standard error of MLE
  # set estimate (mean, 95% C.I. bounds) of the control group and the treatment group for each subgroup

  plot.data = list()
  treatment.mean = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1 , ncol = 1)
  treatment.upper = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.lower = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.C.mean = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol+ 1, ncol = 1)
  treatment.C.upper = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol+ 1, ncol = 1)
  treatment.C.lower = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.mean = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1 , ncol = 1)
  treatment.T.upper = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.lower = matrix(rep(0, n.subgrp.tol+ 1), nrow = n.subgrp.tol + 1, ncol = 1)
  for (i in 1 : n.subgrp.tol){

    if (sum((data.subgrp[[i]]$trt == "1")) == 0 | sum((data.subgrp[[i]]$trt == "0")) == 0){
      treatment.mean[i] = NA
      treatment.upper[i] = NA
      treatment.lower[i] = NA
    }else{

      if (outcome.type == "continuous"){

        model.int = lm(resp ~ trt,  data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.upper[i] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
        treatment.lower[i] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
          treatment.C.upper[i] = NA
          treatment.C.lower[i] = NA
        }else{
          model.int = lm(resp ~ 1,  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = model.sum$coefficients[1, 1]
          treatment.C.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.C.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA
          treatment.T.upper[i] = NA
          treatment.T.lower[i] = NA
        }else{
          model.int = lm(resp ~ 1,  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] = model.sum$coefficients[1, 1]
          treatment.T.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.T.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

      }else if (outcome.type == "binary"){

        model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.upper[i] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
        treatment.lower[i] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
          treatment.C.upper[i] = NA
          treatment.C.lower[i] = NA
        }else{
          model.int = glm(resp ~ 1, family = "binomial",  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = model.sum$coefficients[1, 1]
          treatment.C.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.C.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA
          treatment.T.upper[i] = NA
          treatment.T.lower[i] = NA
        }else{
          model.int = glm(resp ~ 1, family = "binomial",  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] = model.sum$coefficients[1, 1]
          treatment.T.upper[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
          treatment.T.lower[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
        }

      }else if (outcome.type == "survival"){

        model.int = coxph(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.upper[i] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
        treatment.lower[i] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

        surv.fit = survfit(Surv(time, status) ~ trt, data = data.subgrp[[i]])
        difference <- summary(surv.fit, time=time)

        plot.data[[i]] = surv.fit
        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
          treatment.C.upper[i] = NA
          treatment.C.lower[i] = NA
        }else{
          model.int = coxph(Surv(time, status) ~ 1, data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = difference$surv[1]
          treatment.C.upper[i] = difference$upper[1]
          treatment.C.lower[i] = difference$lower[1]
        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA
          treatment.T.upper[i] = NA
          treatment.T.lower[i] = NA
        }else{
          model.int = coxph(Surv(time, status) ~ 1, data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] =  difference$surv[2]
          treatment.T.upper[i] = difference$upper[2]
          treatment.T.lower[i] = difference$lower[2]
        }
      }
    }
  }

  # set estimate (mean, 95% C.I. bounds) of the control group and the treatment group for the full population

  if (sum((dat$trt == "1")) == 0 | sum((dat$trt == "0")) == 0){
    treatment.mean[n.subgrp.tol + 1] = NA
    treatment.upper[n.subgrp.tol + 1] = NA
    treatment.lower[n.subgrp.tol + 1] = NA
  }else{

    if (outcome.type == "continuous"){

      model.int = lm(resp ~ trt,  data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coefficients[2, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
        treatment.C.upper[n.subgrp.tol + 1] = NA
        treatment.C.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = lm(resp ~ 1,  data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.C.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.C.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
        treatment.T.upper[n.subgrp.tol + 1] = NA
        treatment.T.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = lm(resp ~ 1,  data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.T.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.T.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

    }else if (outcome.type == "binary"){

      model.int = glm(resp ~ trt, family = "binomial", data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coefficients[2, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
        treatment.C.upper[n.subgrp.tol + 1] = NA
        treatment.C.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = glm(resp ~ 1, family = "binomial",  data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.C.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.C.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
        treatment.T.upper[n.subgrp.tol + 1] = NA
        treatment.T.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = glm(resp ~ 1, family = "binomial",  data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
        treatment.T.upper[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coefficients[1, 2]
        treatment.T.lower[n.subgrp.tol + 1] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coefficients[1, 2]
      }

    }else if (outcome.type == "survival"){

      model.int = coxph(Surv(time, status) ~ trt, data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coef[1, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

      surv.fit = survfit(Surv(time, status) ~ trt, data = dat)
      difference <- summary(surv.fit, time=time)
      plot.data[[n.subgrp.tol + 1]] = surv.fit
      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
        treatment.C.upper[n.subgrp.tol + 1] = NA
        treatment.C.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = coxph(Surv(time, status) ~ 1, data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1]  = difference$surv[1]
        treatment.C.upper[n.subgrp.tol + 1] = difference$upper[1]
        treatment.C.lower[n.subgrp.tol + 1] = difference$lower[1]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
        treatment.T.upper[n.subgrp.tol + 1] = NA
        treatment.T.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = coxph(Surv(time, status) ~ 1, data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1]  = difference$surv[2]
        treatment.T.upper[n.subgrp.tol + 1] = difference$upper[2]
        treatment.T.lower[n.subgrp.tol + 1] = difference$lower[2]
      }
    }
  }

  lab.subgrp = vector()
  k = 0
  for (i in 1: length(covari.sel)){
    for (j in 1 : length(cats.var[[i]])){
      k = k + 1
      # lab.subgrp[k] = paste0("(", LETTERS[i], j, ") ", lab.vars[i], "=", cats.var[[i]][j], sep = "")
      lab.subgrp[k] = paste0(lab.vars[i], "=", cats.var[[i]][j], sep = "")
    }
  }
  lab.subgrp[n.subgrp.tol + 1] = "Full"

  est.range = cbind.data.frame(treatment.mean, treatment.lower, treatment.upper )
  est.C.range = cbind.data.frame(treatment.C.mean, treatment.C.lower, treatment.C.upper )
  est.T.range = cbind.data.frame(treatment.T.mean, treatment.T.lower, treatment.T.upper )
  dimnames(est.range) = list(c(lab.subgrp), c("mean", "lower","upper") )

  ################################################ 2. create plots #################################################################

  layout(matrix(c(1:n.subgrp.tol), nrow=2, ncol=4))
  # layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(1,1))
  par(mar=c(1,1,3,1), xpd = TRUE)
  ##########   middle-middle cell
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
  col.treat = col.upper = col.lower = vector()
  treat_size = c((treatment.mean))

  for (i in 1 : (n.subgrp.tol)){
    if (is.na(treat_size[i]))
    {
      col.treat[i] = "white"
      col.upper[i] = col.lower[i] ="white"
    }else{
      col.idx = which(treat_size[i] >= breaks)
      col.idx = max(col.idx)
      col.treat[i] = col.vec[col.idx]

      col.idx = which(c(treatment.upper)[i] >= breaks)
      col.idx = max(col.idx)
      col.upper[i] = col.vec[col.idx]

      col.idx = which(c(treatment.lower)[i] >= breaks)
      col.idx = max(col.idx)
      col.lower[i] = col.vec[col.idx]
    }
  }


  circos.clear()
  circos.par(cell.padding = c(0,0,0,0), track.margin = c(0, 0))

  rownames(ss.subgrp) = lab.subgrp[1:n.subgrp.tol]
  colnames(ss.subgrp) = lab.subgrp[1:n.subgrp.tol]
  rownames(r.prop) = lab.subgrp[1:n.subgrp.tol]
  colnames(r.prop) = lab.subgrp[1:n.subgrp.tol]
  mat2 = r.prop
  mat = ss.subgrp
  mat[lower.tri(mat)] <- 0
  diag(mat2) <- 0
  factor1 = lab.subgrp[1]
  factor2 = lab.subgrp[2]

  col_mat = rand_color(length(mat),
                       luminosity = "bright",
                       transparency = 0.45)
  dim(col_mat) = dim(mat)  # to make sure it is a matrix

  mat3 = cbind(rep(0, n.subgrp.tol), mat2)
  ## Create Circos function -------------
  create_circos <- function(){
    circos.par(cell.padding = c(0,0,0,0), track.margin = c(0, 0))
    if (equal.width){
      circos.initialize(lab.subgrp[1:n.subgrp.tol],
                        xlim = cbind(rep(0, n.subgrp.tol), rep(1, n.subgrp.tol)))
    } else {
      circos.initialize(lab.subgrp[1:n.subgrp.tol],
                        xlim = cbind(rep(0, n.subgrp.tol), rep(1, n.subgrp.tol)),
                        sector.width = diag(ss.subgrp))
    }
    circos.track(ylim = c(0, 1), track.height = uh(2, "mm"),
                 panel.fun = function(x, y) {
                   circos.text(CELL_META$xcenter,
                               CELL_META$cell.ylim[2] + uy(3, "mm"),
                               CELL_META$sector.index)
                 })
    for (i in 1:(length(lab.subgrp)-1)){
      factor = lab.subgrp[i]
      circos.update(sector.index = factor, #track.index = 2,
                    bg.col = col.treat[i], bg.border = "black")
    }
  }


  ii = 0
  var.ind = unlist(lapply(1:n.covari, function(x) rep(x, each = length(cats.var[[x]]))))
  var.change = var.ind - dplyr::lag(var.ind)
  # var.change[1] = 1
  var.change[1] = 0
  i=1
  for (i in 1:n.covari){
    for (j in 1:length(cats.var[[i]])){
      # i=1
      # j=1
      # k=3
      # ss.subgrp
      # for (i in 1:1){
      #   for (j in 1:1){
      suppressMessages(
      create_circos()) ## Create base plot without links
      ii = ii + 1
      for (k in (ii):(n.subgrp.tol)){
        if(mat2[ii, k] == 0) next()
        pos.i = ii
        pos.j = k

        var.change[k]

        begin.1 = sum(mat2[pos.i, 1:(pos.j-1)]) - sum(var.change[k:1]) + 1
        end.1   = sum(mat2[pos.i, 1:pos.j]) - sum(var.change[k:1]) + 1
        begin.2 = sum(mat3[pos.j, 1:pos.i]) - sum(var.change[ii:1])
        end.2   = sum(mat3[pos.j, 1:(pos.i+1)]) - sum(var.change[ii:1])
        # cat(begin.1, end.1, begin.2, end.2)
        circos.link(lab.subgrp[ii],
                    c(begin.1, end.1),
                    lab.subgrp[k],
                    c(begin.2, end.2),
                    col = col_mat[1, k]) # Add the links
      }
      for (k in (ii):(1)){
        if(mat2[ii, k] == 0) next()
        pos.i = ii
        pos.j = k

        var.change[k]

        begin.1 = sum(mat2[pos.i, (pos.j+1):n.subgrp.tol]) - sum(rev(var.change)[k:n.subgrp.tol]) +1
        end.1   = sum(mat2[pos.i, (pos.j):n.subgrp.tol]) - sum(rev(var.change)[k:n.subgrp.tol]) + 1
        begin.2 = sum(mat3[pos.j, 1:pos.i]) - sum(var.change[ii:1]) + 1
        end.2   = sum(mat3[pos.j, 1:(pos.i+1)]) - sum(var.change[ii:1]) + 1
        # cat(begin.1, begin.2, end.1, end.2)
        circos.link(lab.subgrp[ii],
                    c(begin.1, end.1),
                    lab.subgrp[k],
                    c(begin.2, end.2),
                    col = col_mat[1, k])
      }
    }
  }




  # par(mar=c(1,2, 2, 2))
  # par(mar=c(5,1, 5, 1), xpd = FALSE)
  # SubgrPlots:::image.scale(treatmeant.mean, col= col.vec, breaks = breaks, axis.pos = 4, add.axis = FALSE)
  # axis(2, at = breaks, labels = round(breaks, 3), las = 0, cex.axis = font.size[4])
  # box()
  #
  # abline(h=breaks)
  # title(ylab = strip, cex.lab = font.size[4])
  #
  # mtext(side = 4, text = strip, cex.lab = font.size[4], las = 3)
}

