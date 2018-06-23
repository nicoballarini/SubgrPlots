#' STEPP for subgroup effect size
#'
#' this function produces a plot of using the approach "Subpopulation Treatment Effect Pattern Plot". It shows the treatment effect
#' size of subgroups, where subgruops are defined by certain ranges of a continuous covariate; each subgroup has a sample size close
#' to a pre-specified value (N2) and any neighboring subgroups have an overlap size near another pre-specified value (N1). The plot
#' shows the 95% mariginal C.I. for each subgroup, the 95% simultaneous C.I. for all subgroups and the overall effect (represented by
#' a horizontal line). The y-coordinate of a point indicates the effect size within the corresponding subgroup; the x-coordinate shows
#' the lower bound of the range which defines the subgroup. If part of the horizontal line is out of the simultaneous C.I., it may
#' reveal hetergeneity across subgroup effects with repective to the overall effect. In addition, one can control the width of the C.I.
#' by controlling Type I error rate in one of the function arguments. Note that the function uses log odd ratio and log hazard ratio
#' for displaying subgroup effect sizes in binary and survival data, respectively. The actual subgroup sample sizes over the covariate
#' are shown on the console window as well.
#'
#'@param dat:            a data set
#'@param covari.sel:     a vector of indices of the two covariates
#'@param trt.sel:        a variate index specifying the treatment code
#'@param resp.sel:       a variate index specifying the response variable
#'@param outcome.type:   a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#'@param setup.ss:       a vector specifying the approximate overlap size (N2) and subgroup sample size (N1).
#'@param alpha:          the type I error rate
#'@param font.size:      a vector specifying the size of labels and text; the first element is for main titles, the second is for
#'               for x-axis and y-axis labels; the thrid is for the text in the legend; the fourth is for the subtitle.
#'@param title:          a string specifying the main title.
#'@param lab.y:          a string specifying the labels of the y-axis.
#'@param subtitle:       strings specifying the subtitle
#'
# eg.1            var.sel = 3;
#                 main.title = paste("STEPP for treatment effect size of overlapping subgroups defined by", names(dat)[var.sel]);
#                 lab.y.title = paste("Treatment effect diffence");
#                 setup.ss = c(35, 40);
#                 sub.title = paste("(Subgroup sample sizes are set to", setup.ss[2], "; overlap sizes are set to", setup.ss[1], ")" )
#                 stepp.plt(dat, covari.sel = 3, trt.sel = 2, resp.sel = 1, outcome.type = "continuous", setup.ss = c(35,40), alpha = 0.05,
#                 title = main.title, lab.y = lab.y.title, subtitle = sub.title)
#
# eg.2            var.sel = 2;
#                 main.title = paste("STEPP for overlapping subgroups defined by", names(dat3)[var.sel]);
#                 lab.y.title = paste("Treatment effect diffence (log odds ratio)");
#                 setup.ss = c(120, 130);
#                 sub.title = paste("(Subgroup sample sizes are set to", setup.ss[2], "; overlap sizes are set to", setup.ss[1], ")" )
#                 stepp.plt(dat3, covari.sel = 2, trt.sel = 1, resp.sel = 3, outcome.type = "binary", setup.ss = c(120, 130), alpha = 0.05,
#                 title = main.title, lab.y = lab.y.title, subtitle = sub.title)
#
#                 var.sel = 2;
#                 main.title = paste("STEPP for overlapping subgroups defined by", names(dat3)[var.sel]);
#                 lab.y.title = paste("Treatment effect diffence (log hazard ratio)");
#                 setup.ss = c(120, 130);
#                 sub.title = paste("(Subgroup sample sizes are set to", setup.ss[2], "; overlap sizes are set to", setup.ss[1], ")" )
#                 stepp.plt(dat3, covari.sel = 2, trt.sel = 1, resp.sel = c(4,3), outcome.type = "survival", setup.ss = c(120, 130), alpha = 0.05,
#                 title = main.title, lab.y = lab.y.title, subtitle = sub.title)
#
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
#' @import grid
#' @import graphics
plot_stepp <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type, setup.ss, alpha, font.size = c(1.2,1,1,0.85), title = NULL, lab.y = NULL,
                      subtitle = NULL)
{

  ################################################ 0. argument validity check  #################################################################

  if (missing(dat)) stop("Data have not been inputed!")
  if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")

  if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  if (!(length(covari.sel) == 1)) stop("The variables for defining subgroups can not have more than one component!")

  if (missing(trt.sel)) stop("The variable specifying the treatment code (for treatment / control groups) has not been inputed!")
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

  if (missing(setup.ss)) stop("The setup for the subgroup smaple size and overlap size has not been specified!")
  if (!(is.numeric(setup.ss))) stop("The setup for the subgroup smaple size and overlap size is not numeric!")
  if (length(setup.ss) !=  2) stop("The setup for the subgroup smaple size and overlap size should have two elements only!")
  if (setup.ss[1] > setup.ss[2]){
    stop("subgroup overlap sample sizes should not be larger than the subgroup sample size!")
  }

  if (missing(alpha)) stop("The type I error rate has not been specified!")
  if (!(length(alpha) == 1)) stop("The error rate can not have more than one value!")
  if (!(is.numeric(alpha))) stop("The error rate is not numeric!")

  #mode.all = c("slide window", "trail oriented")
  #if (is.null(plot.mode)) stop("The mode of the stepp plot has not been specified!")
  #if (!(is.element(plot.mode, model.all)) == TRUE) stop("A unrecognized plot mode has been inputed!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of labels and text is not numeric!")
  if (!(length(font.size) == 4)) stop("The font size setups for labels or text should have four components only!")

  ################################################ 1. create subgroup data  #################################################################

  ## slide-oriented window

  lab.var = names(dat)[covari.sel]
  covari1.table = round(sort(dat[, covari.sel[1]]), 4)

  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                   # rename the response variable for survival right censoring status
  }


  N1 = setup.ss[1]                                               # the approximate subgroup overlap sample size for any two neighboring subgroups
  N2 = setup.ss[2]                                               # the approximate subgroup sample size

  cutpoint.covar1 = list()
  cutpoint.covar1[[1]] = vector()
  cutpoint.covar1[[2]] = vector()

  low.bd.covar1.idx = 1
  upp.bd.covar1.idx = N2
  ss.full = dim(dat)[1]
  i = 0
  while (upp.bd.covar1.idx < ss.full){
    i = i + 1
    low.bd.covar1.idx = 1 + (i-1) * (N2 - N1)
    upp.bd.covar1.idx = N2+ (i-1) * (N2 - N1)
    cutpoint.covar1[[1]][i] = covari1.table[low.bd.covar1.idx]
    cutpoint.covar1[[2]][i] = covari1.table[upp.bd.covar1.idx]
  }


  idx.covar1 = list()                                            # the index set of subgroups over the first covariate
  n.subgrp.covar1 = length(cutpoint.covar1[[1]])                 # the number of subgroups over the first covariate
  ss.subgrp.covar1 = vector()
  data.subgrp.covar1 = list()
  for (i in 1 : n.subgrp.covar1){
    idx.covar1[[i]] = which((dat[, covari.sel[1]] >= cutpoint.covar1[[1]][i] &
                               dat[, covari.sel[1]] <= cutpoint.covar1[[2]][i] ) == T  )
    data.subgrp.covar1[[i]] =  dat[idx.covar1[[i]], ]
    ss.subgrp.covar1[i] = length(idx.covar1[[i]])
  }

  subgrp.lab = rep(0, n.subgrp.covar1)
  for (i in 1 : n.subgrp.covar1){
    subgrp.lab[i] = paste(cutpoint.covar1[[1]][i],"--",cutpoint.covar1[[2]][i])
  }

  ### adjust factor for simultaneous confidence interval

  alpha.adj = 1 - (1 - alpha)^(1/n.subgrp.covar1)
  gamma = qnorm(1 - alpha.adj/2 ) / qnorm(1 - alpha/2 )

  treatment.mean =matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  treatment.upper.idl =matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  treatment.lower.idl =matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  treatment.upper =matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  treatment.lower =matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  for (i in 1 : n.subgrp.covar1){
    if ((sum((data.subgrp.covar1[[i]])$trt == "1")) == 0 | (sum((data.subgrp.covar1[[i]])$trt == "0")) == 0 ){
      treatment.mean[i] = NA
      treatment.upper[i] = NA
      treatment.lower[i] = NA
      treatment.upper.idl[i] = NA
      treatment.lower.idl[i] = NA
    }else{

      if (outcome.type == "continuous"){
        model.int = lm(resp ~ trt,  data = data.subgrp.covar1[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.lower[i] = model.sum$coefficients[2, 1] - gamma * 1.96 * model.sum$coefficients[2, 2]
        treatment.upper[i] = model.sum$coefficients[2, 1] + gamma * 1.96 * model.sum$coefficients[2, 2]
        treatment.upper.idl[i] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
        treatment.lower.idl[i] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

      }else if (outcome.type == "binary"){
        model.int = glm(resp ~ trt, family = "binomial", data = data.subgrp.covar1[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coefficients[2, 1]
        treatment.lower[i] = model.sum$coefficients[2, 1] - gamma * 1.96 * model.sum$coefficients[2, 2]
        treatment.upper[i] = model.sum$coefficients[2, 1] + gamma * 1.96 * model.sum$coefficients[2, 2]
        treatment.upper.idl[i] = model.sum$coefficients[2, 1] + 1.96 * model.sum$coefficients[2, 2]
        treatment.lower.idl[i] = model.sum$coefficients[2, 1] - 1.96 * model.sum$coefficients[2, 2]

      }else if (outcome.type == "survival"){
        model.int = survival::coxph(Surv(time, status) ~ trt, data = data.subgrp.covar1[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.lower[i] = model.sum$coefficients[1, 1] - gamma * 1.96 * model.sum$coef[1, 3]
        treatment.upper[i] = model.sum$coefficients[1, 1] + gamma * 1.96 * model.sum$coef[1, 3]
        treatment.lower.idl[i] = model.sum$coefficients[1, 1] - 1.96 * model.sum$coef[1, 3]
        treatment.upper.idl[i] = model.sum$coefficients[1, 1] + 1.96 * model.sum$coef[1, 3]
      }
    }
  }


  if ( sum((dat$trt == "1")) == 0 | sum((dat$trt == "0")) == 0 ){
    treatment.mean.overall = NA
  }else{

    if (outcome.type == "continuous"){
      model.int = lm(resp ~ trt,  data = dat)
      model.sum = summary(model.int)
      treatment.mean.overall = model.sum$coefficients[2, 1]

    }else if (outcome.type == "binary"){
      model.int = glm(resp ~ trt, family = "binomial", data = dat)
      model.sum = summary(model.int)
      treatment.mean.overall = model.sum$coefficients[2, 1]

    }else if (outcome.type == "survival"){
      model.int = survival::coxph(Surv(time, status) ~ trt, data = dat)
      model.sum = summary(model.int)
      treatment.mean.overall = model.sum$coef[1, 1]
    }
  }

  cat("The subgroup sample sizes are actually", ss.subgrp.covar1, "\n")

  ################################################ 2. produce a graph  #################################################################
  if (is.null(title)){
    par(mar=c(4,4,2,1))
  } else{
    par(mar=c(4,4,4,1))
  }

  color = c("green", "red", "blue", "aquamarine2", "brown3", "blueviolet" )
  linetype = c(3:5, 6:8)
  plotchar = c(2, 3, 6, 1, 4, 5)

  covari.subgrp.mid = vector()
  for (i in 1:n.subgrp.covar1){
    covari.subgrp.mid[i]= (cutpoint.covar1[[2]][i] + cutpoint.covar1[[1]][i]) /2
  }

  cutpoint.all = vector()
  cutpoint.all = c(cutpoint.covar1[[1]], cutpoint.covar1[[2]])

  plot(c(1 : n.subgrp.covar1),
       treatment.mean,
       type = "o", lwd = 1.5, pch = 16, lty = 1,  col = "red", xaxt = "n",
       ylim = c(min(treatment.lower, na.rm = TRUE), max(treatment.upper, na.rm = TRUE)),
       xlim = c(1, n.subgrp.covar1),
       xlab = lab.var,
       ylab = lab.y,
       main = title,
       # sub = subtitle,
       cex.main = font.size[1],
       cex.lab =  font.size[2],
       cex.sub =  font.size[3])
  mtext(subtitle, cex = font.size[3], line = .1)
  by. = ceiling(diff((range(cutpoint.covar1[[1]])))/10)
  axis(side = 1, at = seq(1, n.subgrp.covar1, by=by.),
       cex.axis = font.size[2], #tck = -0.01,
       labels = round(cutpoint.covar1[[1]],1)[seq(1, n.subgrp.covar1, by=by.)])
  lines(c(1 : n.subgrp.covar1), treatment.upper, lty = 2, lwd = 1.5, col = "blue" )
  lines(c(1 : n.subgrp.covar1), treatment.lower, lty = 2, lwd = 1.5, col = "blue" )
  lines(c(1 : n.subgrp.covar1), treatment.upper.idl, lty = 2, lwd = 1.5, col = "orange" )
  lines(c(1 : n.subgrp.covar1), treatment.lower.idl, lty = 2, lwd = 1.5, col = "orange" )
  abline(h = 0, col = "black", lty = 2)
  abline(h = treatment.mean.overall, col = "green", lty = 1)

  xy.current.pos = par("usr")
  lab.bd.ic = paste("Boundaries for", (1-alpha) * 100, "% C.I.")
  lab.bd.sic = paste("Boundaries for", (1-alpha) * 100, "% S.C.I.")
  legend(xy.current.pos[1], xy.current.pos[4], c("Overlapping Subgroup Mean", "Overall Mean ", lab.bd.sic, lab.bd.ic), lty = c(1, 1, 2, 2),
         col = c("red", "green", "blue", "orange" ), bty = "n",
         cex = font.size[4])
}
