#' L'Abbe plot for subgroup effect size
#'
#' this function produces a L'Abbe plot showing the treatment effect size of subgroups defined by the categories of covariates. The
#' x-axis and y-axis represent the treatment effect estimate from the control and treatment group, respectively. The dashed dignal
#' indicates no effect, and the solid diagnal line corresponds to the full population effect estimate. The squares represent subgroups,
#' and the red or blue dashed lines have a length which show the magnitude of the lower or upper bound of the 95% C.I for subgroup
#' treatment effect estimates. If the solid diagonal line cross all the blue or red lines, it may indicate homogeneity across subgroup
#' with repective to the full population effect estimate. Note that the overall size of squares which represent subgroups can be
#' adjusted by setting different values on the associated input argument. In addition, the function uses log odd ratio for displaying
#' subgroup effect sizes in binary and survival data, respectively.
#'
#'
#' @param dat              a data set
#' @param covari.sel       a vector of indices of the two covariates
#' @param trt.sel          a covariate index specifying the treatment code
#' @param resp.sel         a covariate index specifying the response variable
#' @param outcome.type     a string specifying the type of the response variable, it can be "continuous", or "binary".
#' @param adj.ann.subgrp   a parameter controlling the distance between a square and its corresponding subgroup label; also, the line
#'  gap between the legend text for subgroups.
#' @param size.shape       a parameter controlling the height and width of the squares and the size is proportional to the ratio of subgroup sample sizes
#'  over the full population size.
#' @param font.size        a vector specifying the size of labels and text; the first element is for the main title, the second is for
#'  for x-axis and y-axis labels; the thrid is for the legend text of subgroups; the fourth is for the subgroup
#'  labels near the corresponding squares.
#' @param title            a string specifying the main title.
#' @param lab.xy           a list of two strings specifying the labels of the x and y axes.
#' @param time              time for calculating the survival in each subgroup
#' @param show.ci A logical indicating whether to show an additional line for confidence intervals
#' @param effect Either "HR" of "RMST". Only when outcome.type = "survival"
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
#' @import grid
#' @import graphics
plot_labbe <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                       effect = c("HR","RMST"), size.shape = 1/18,
                       adj.ann.subgrp = 1/30, font.size = c(1, 1, 0.85, 0.85),
                       title = NULL, lab.xy = NULL,
                       time = mean(dat[,resp.sel[1]]), show.ci = TRUE)
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

  type.all = c("continuous", "binary", "survival")
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
  }

  if (!(is.numeric(size.shape))) stop("The argument about the shape setting of diamonds is not numeric!")
  if (!(length(size.shape) == 1)) stop("The shape set-up of diamonds is not specified correctly!!")

  if (!(is.numeric(adj.ann.subgrp))){
    stop("The argument adjusting the distance between a square and its corresponding subgroup label, or adjusting line gap between the
         legend text is not numeric!")
  }
  if (adj.ann.subgrp < 0){
    stop("The argument adjusting the distance between a square and its corresponding subgroup label, or adjusting line gap between the
         legend text is not positive!")
  }

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of labels and text is not numeric!")
  if (!(length(font.size) == 4)) stop("The font size setups for labels or text should have four components only!")

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
    n.subgrp.tol = n.subgrp.tol + length(cats.var[[i]])                            # the number of the subgroups (excluding the complement)
  }

  cond = list()
  data.subgrp = list()
  ss.subgrp = matrix(rep(0, n.subgrp.tol * n.subgrp.tol), nrow = n.subgrp.tol)
  ss.subgrp.T = matrix(0, nrow = n.subgrp.tol + 1)
  ss.subgrp.C = matrix(0, nrow = n.subgrp.tol + 1)
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
  ss.subgrp.T[n.subgrp.tol + 1] = length(which(dat$trt == 1))
  ss.subgrp.C[n.subgrp.tol + 1] = length(which(dat$trt == 0))

  k = n.subgrp.tol
  r.prop = diag(n.subgrp.tol)
  for (i in 1 : (n.subgrp.tol - 1) ){
    for (j in (i + 1) : (n.subgrp.tol) ){
      k = k + 1
      cond[[k]] = intersect(cond[[i]], cond[[j]])
      ss.subgrp[i, j] = length(cond[[k]])
      ss.subgrp[j, i] = length(cond[[k]])
    }
  }


  # create matrices for treatment size and standard error of MLE

  treatment.C.mean = matrix(0, nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.mean = matrix(0, nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.mean  = matrix(0, nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.upper = matrix(0, nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.lower = matrix(0, nrow = n.subgrp.tol + 1, ncol = 1)
  for (i in 1:n.subgrp.tol){

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
        }else{
          model.int = lm(resp ~ 1,  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = model.sum$coefficients[1, 1]
        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA
        }else{
          model.int = lm(resp ~ 1,  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] = model.sum$coefficients[1, 1]
        }

      }else if (outcome.type == "binary"){

        model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.upper[i] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
        treatment.lower[i] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
        }else{
          model.int = glm(resp ~ 1, family = "binomial",  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
          model.sum = summary(model.int)
          treatment.C.mean[i] = model.sum$coefficients[1, 1]

        }

        if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
          treatment.T.mean[i] = NA

        }else{
          model.int = glm(resp ~ 1, family = "binomial",  data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
          model.sum = summary(model.int)
          treatment.T.mean[i] = model.sum$coefficients[1, 1]

        }

      }else if (outcome.type == "survival"){
        if (effect == "RMST"){
          dat.subgr.i = data.subgrp[[i]]
          rmst = survRM2::rmst2(time = dat.subgr.i$time, status = dat.subgr.i$status,
                                arm = dat.subgr.i$trt, tau = time)


          treatment.mean[i]  = rmst$unadjusted.result[1,1]
          treatment.lower[i] = rmst$unadjusted.result[1,2]
          treatment.upper[i] = rmst$unadjusted.result[1,3]

          if (length(which(dat$trt == 0)) == 0){
            treatment.C.mean[i] = NA
          }else{
            treatment.C.mean[i] = rmst$RMST.arm0$rmst[1]
          }

          if (length(which(dat$trt == 1)) == 0){
            treatment.T.mean[i] = NA
          }else{
            treatment.T.mean[i] = rmst$RMST.arm1$rmst[1]
          }
        }
        if (effect == "HR"){
          model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.subgrp[[i]])
          model.sum = summary(model.int)
          treatment.mean[i] = model.sum$coef[1, 1]
          treatment.upper[i] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
          treatment.lower[i] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

          surv.fit = survival::survfit(survival::Surv(time, status) ~ trt, data = data.subgrp[[i]])
          difference <- summary(surv.fit, time=time)
          if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
            treatment.C.mean[i] = NA
          }else{
            treatment.C.mean[i] = difference$surv[1]
          }

          if (length(which(data.subgrp[[i]]$trt == 1)) == 0){
            treatment.T.mean[i] = NA
          }else{
            treatment.T.mean[i] = difference$surv[2]
          }
        }
      }
    }
  }


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
      }else{
        model.int = lm(resp ~ 1,  data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
      }else{
        model.int = lm(resp ~ 1,  data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
      }

    }else if (outcome.type == "binary"){

      model.int = glm(resp ~ trt, family = "binomial", data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coefficients[2, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
      }else{
        model.int = glm(resp ~ 1, family = "binomial",  data = dat[which(dat$trt == 0), ])
        model.sum = summary(model.int)
        treatment.C.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
      }

      if (length(which(dat$trt == 1)) == 0){
        treatment.T.mean[n.subgrp.tol + 1] = NA
      }else{
        model.int = glm(resp ~ 1, family = "binomial",  data = dat[which(dat$trt == 1), ])
        model.sum = summary(model.int)
        treatment.T.mean[n.subgrp.tol + 1] = model.sum$coefficients[1, 1]
      }

    }else if (outcome.type == "survival"){
      if (effect == "RMST"){
        rmst = survRM2::rmst2(time = dat$time, status = dat$status,
                              arm = dat$trt, tau = time)
        treatment.mean[n.subgrp.tol + 1]  = rmst$unadjusted.result[1,1]
        treatment.lower[n.subgrp.tol + 1] = rmst$unadjusted.result[1,2]
        treatment.upper[n.subgrp.tol + 1] = rmst$unadjusted.result[1,3]

        if (length(which(dat$trt == 0)) == 0){
          treatment.C.mean[n.subgrp.tol + 1] = NA
        }else{
          treatment.C.mean[n.subgrp.tol + 1] = rmst$RMST.arm0$rmst[1]
        }

        if (length(which(dat$trt == 1)) == 0){
          treatment.T.mean[n.subgrp.tol + 1] = NA
        }else{
          treatment.T.mean[n.subgrp.tol + 1] = rmst$RMST.arm1$rmst[1]
        }
      }

      if (effect == "HR"){
        model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = dat)
        model.sum = summary(model.int)
        treatment.mean[n.subgrp.tol + 1] = model.sum$coef[1, 1]
        treatment.upper[n.subgrp.tol + 1] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
        treatment.lower[n.subgrp.tol + 1] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

        surv.fit = survival::survfit(survival::Surv(time, status) ~ trt, data = dat)
        difference <- summary(surv.fit, time=time)

        if (length(which(dat$trt == 0)) == 0){
          treatment.C.mean[n.subgrp.tol + 1] = NA
        }else{
          treatment.C.mean[n.subgrp.tol + 1] = difference$surv[1]
        }

        if (length(which(dat$trt == 1)) == 0){
          treatment.T.mean[n.subgrp.tol + 1] = NA
        }else{
          treatment.T.mean[n.subgrp.tol + 1] = difference$surv[2]
        }
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
  lab.subgrp[n.subgrp.tol + 1] = "Full"
  lab.subgrp2[n.subgrp.tol + 1] = "Full"

  dimnames(treatment.C.mean) = list(c(lab.subgrp), c("C.mean"))
  dimnames(treatment.T.mean) = list(c(lab.subgrp), c("T.mean"))
  dimnames(treatment.mean)  = list(c(lab.subgrp), c("mean diff"))
  dimnames(treatment.upper) = list(c(lab.subgrp), c("upper"))
  dimnames(treatment.lower) = list(c(lab.subgrp), c("lower"))

  ################################################ 2. produce a graph  #################################################################

  par(mar = c(4,4,2,0.5))
  x.lim.min = min(min(treatment.T.mean, na.rm = TRUE),  min(treatment.C.mean, na.rm = TRUE)) - 0.2
  x.lim.max = max(max(treatment.T.mean, na.rm = TRUE),  max(treatment.C.mean, na.rm = TRUE)) + 0.2
  eff.col = c("#91bfdb","#fc8d59")
  if (treatment.mean[n.subgrp.tol + 1] > 0 ){
    CI.bar.y.pos = treatment.C.mean + abs(treatment.upper)
  }else{
    CI.bar.y.pos = treatment.C.mean - abs(treatment.lower)
  }

  if ((outcome.type == "survival" & effect == "HR")){
    lab.x=paste0(lab.xy[[1]], " (Survival probability at time=", time,")")
    lab.y=paste0(lab.xy[[2]], " (Survival probability at time=", time,")")
  } else if ((outcome.type == "survival" & effect == "RMST")){
    lab.x=paste0(lab.xy[[1]], " (RMST for tau=", time,")")
    lab.y=paste0(lab.xy[[2]], " (RMST for tau=", time,")")
  } else {
    lab.x=paste0(lab.xy[[1]])
    lab.y=paste0(lab.xy[[2]])
  }

  plot(treatment.C.mean, treatment.T.mean, type='n',
       xlab = lab.x,
       ylab = lab.y,
       main= title,
       ylim = c(x.lim.min, x.lim.max), xlim = c(x.lim.min, x.lim.max), cex.main = font.size[1], cex.lab = font.size[2], bty = "o")
  abline(a=0, b = 1, lty = 2)
  text( max(max(treatment.T.mean, na.rm = TRUE),  max(treatment.C.mean, na.rm = TRUE)),
        max(max(treatment.T.mean, na.rm = TRUE),  max(treatment.C.mean, na.rm = TRUE)),
        labels = "x = y")

  if (!(outcome.type == "survival" & effect == "HR")){
    eff.col = rev(eff.col)
    abline(a = treatment.mean[n.subgrp.tol + 1] , b = 1, lty = 1)
  }

  rectangle = list()
  w = vector()
  h = vector()
  x.range = (max(treatment.C.mean, na.rm = TRUE)+ 0.2) - (min(treatment.C.mean, na.rm = TRUE)-0.2)
  y.range = (max(treatment.T.mean, na.rm = TRUE)+ 0.2) - (min(treatment.T.mean, na.rm = TRUE)-0.2)
  data.C.size = length(which(dat$trt == 0))
  data.T.size = length(which(dat$trt == 1))

  col.seg = rep(eff.col[2], n.subgrp.tol + 1)
  col.seg[which(treatment.mean < 0 )] = rep(eff.col[1], length(which(treatment.mean < 0 )))
  prop.w = (x.lim.max - x.lim.min) * size.shape  #0.8   # the shrinkage in width
  prop.h = (x.lim.max - x.lim.min) * size.shape  #0.8   # the shrinkage in height
  for (i in 1: (n.subgrp.tol + 1)){
    w[i] = ss.subgrp.C[i]/data.C.size * prop.w
    h[i] = ss.subgrp.T[i]/data.T.size * prop.h

    rectangle[[i]] = rectangleSP_test(treatment.C.mean[i], treatment.T.mean[i], w[i], h[i])
    sp::plot(rectangle[[i]], add=TRUE, col= adjustcolor("white", alpha.f = 0))

    segments(treatment.C.mean[i], treatment.C.mean[i], treatment.C.mean[i], treatment.T.mean[i], col = col.seg[i], lwd = 2)
    if (show.ci){
    segments(treatment.C.mean[i], treatment.C.mean[i], treatment.C.mean[i], CI.bar.y.pos[i], col = "violet", lwd = 3, lty = 2)
    }
  }
  text(treatment.C.mean, treatment.T.mean - (x.lim.max - x.lim.min) * adj.ann.subgrp, labels = lab.subgrp2, pch = 4, cex = font.size[4])

  x.pos = vector(); y.pos = vector()
  xy.current.pos = par("usr")
  lab.pos.adj = (x.lim.max - x.lim.min) * adj.ann.subgrp
  for (i in 1 : (n.subgrp.tol + 1)){
    x.pos[i] = xy.current.pos[1] + lab.pos.adj
    y.pos[i] = xy.current.pos[4] - (i - 1) * lab.pos.adj - lab.pos.adj * 2
  }
  text(x.pos, y.pos, labels = lab.subgrp, adj = c(0, 1),  cex = font.size[3])
  text(x.pos + lab.pos.adj * 9,
       y.pos,
       labels = round(treatment.mean[1 : (n.subgrp.tol + 1)], 2),
       col = col.seg[1 : (n.subgrp.tol + 1)],
       adj = c(1, 1),
       cex = font.size[3])

  text(x.pos[n.subgrp.tol] + lab.pos.adj * 9, y.pos[1] + lab.pos.adj,
       labels = "Effect size",
       adj = c(1, 1),
       cex = font.size[3], font=4)

}
