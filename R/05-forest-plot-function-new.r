#' Forest plot for subgroup effect size
#'
#' This function produces a forest plot showing the treatment effect size of subgroups defined by the categories of covariates. The
#' first sub-figure provides a table of treatment effect estimate and sample size (for treatment / control group within each subgroup)
#' ; the second sub-figure shows forest plots for subgroups and full population; the third displays forest plots of treatment and
#' control group for each population. The dashed vertical line indicates no effect Note that the overall size of diamonds which
#' represent subgroups can be adjusted by setting different values on the associated input argument. In addition, the function uses
#' log odd ratio and log hazard ratio for displaying subgroup effect sizes in binary and survival data, respectively.
#'
#'
#'@param dat          a data set
#'@param covari.sel   a vector of indices of the two covariates
#'@param trt.sel      a covariate index specifying the treatment code
#'@param resp.sel     a covariate index specifying the response variable
#'@param outcome.type a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#'@param size.shape   a vector specifying the height and width of the diamonds displaying sample sizes.
#'@param font.size    a vector specifying the size of labels and text; the first element is for the main titles, the second is for
#'               for the x-axis labels; the thrid is for the text in the first sub-figure; the fourth is for the unit labels of
#'               the x-axis.
#'@param title        a list of three strings specifying the main titles of the three sub-figures.
#'@param lab.x        a list of three strings specifying the x-axis labels of the three sub-figures.
#' @param time             time for calculating the RMST
#' @param KM A logical indicating whether to show the Kaplan-meier curves

# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 30/08/17
#' @export
#' @import grid
#' @import graphics
plot_forest <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                      size.shape = c(0.25, 0.12), font.size = c(1.3, 1, 0.85, 0.9),
                      title = NULL, lab.x = NULL, time = mean(dat[,resp.sel[1]]),
                      KM = FALSE)
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

  if (!(is.numeric(size.shape))) stop("The argument about the shape setting of diamonds is not numeric!")
  if (!(length(size.shape) == 2)) stop("The shape set-up of diamonds has two components only!!")

  if (!(is.numeric(font.size))) stop("The argument about the font sizes of labels and text is not numeric!")
  if (!(length(font.size) == 4)) stop("The font size setups for labels or text should have four components only!")

  ################################################ 1. create subgroup data  #################################################################

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
    }
  }

  # create matrices for treatment size and standard error of MLE
  # set estimate (mean, 95% C.I. bounds) of the control group and the treatment group for each subgroup


  treatment.mean  = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.upper = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.lower = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.C.mean  = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.C.upper = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.C.lower = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.mean  = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.upper = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1, ncol = 1)
  treatment.T.lower = matrix(rep(0, n.subgrp.tol + 1), nrow = n.subgrp.tol + 1, ncol = 1)
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

        model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.upper[i] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
        treatment.lower[i] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

        surv.fit = survival::survfit(survival::Surv(time, status) ~ trt, data = data.subgrp[[i]])
        difference <- summary(surv.fit, time=time)

        if (length(which(data.subgrp[[i]]$trt == 0)) == 0){
          treatment.C.mean[i] = NA
          treatment.C.upper[i] = NA
          treatment.C.lower[i] = NA
        }else{
          model.int = survival::coxph(survival::Surv(time, status) ~ 1, data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 0), ])
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
          model.int = survival::coxph(survival::Surv(time, status) ~ 1, data = data.subgrp[[i]][which(data.subgrp[[i]]$trt == 1), ])
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

      model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = dat)
      model.sum = summary(model.int)
      treatment.mean[n.subgrp.tol + 1] = model.sum$coef[1, 1]
      treatment.upper[n.subgrp.tol + 1] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
      treatment.lower[n.subgrp.tol + 1] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

      surv.fit = survival::survfit(survival::Surv(time, status) ~ trt, data = dat)
      difference <- summary(surv.fit, time=time)

      if (length(which(dat$trt == 0)) == 0){
        treatment.C.mean[n.subgrp.tol + 1] = NA
        treatment.C.upper[n.subgrp.tol + 1] = NA
        treatment.C.lower[n.subgrp.tol + 1] = NA
      }else{
        model.int = survival::coxph(survival::Surv(time, status) ~ 1, data = dat[which(dat$trt == 0), ])
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
        model.int = survival::coxph(survival::Surv(time, status) ~ 1, data = dat[which(dat$trt == 1), ])
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
      lab.subgrp[k] = paste0("(", LETTERS[i], j, ") ", lab.vars[i], "=", cats.var[[i]][j], sep = "")
    }
  }
  lab.subgrp[n.subgrp.tol + 1] = "Full"

  est.range = cbind.data.frame(treatment.mean, treatment.lower, treatment.upper )
  est.C.range = cbind.data.frame(treatment.C.mean, treatment.C.lower, treatment.C.upper )
  est.T.range = cbind.data.frame(treatment.T.mean, treatment.T.lower, treatment.T.upper )
  dimnames(est.range) = list(c(lab.subgrp), c("mean", "lower","upper") )

  ################################################ 2. create plots #################################################################

  layout(matrix(c(1,2,3), byrow = TRUE, nrow=1, ncol=3), widths=c(1, 1, 1), heights=c(1, 1, 1))

  col.line = c("blue", "red", "forestgreen", "orange", "darkorchid1", "darkgoldenrod3", "darkseagreen3", "chartreuse3", "cyan1", "deeppink1")

  # forest plot (1)
  par(mar = c(4.1,1,2.5,0))
  line.x.range = est.range[, 2:3]
  line.y.range = matrix(rep(seq(0.1, 0.1+ (n.subgrp.tol) * 0.3, 0.3),2), ncol = 2)

  line.comp.y.range = matrix(rep(seq(0.1 + 0.15, 0.1 + 0.15+ (n.subgrp.tol) * 0.3, 0.3),2), ncol = 2)
  x.lim.max = max(abs(est.range[, 2:3]))
  y.lim.max = 0.1+ (n.subgrp.tol + 1) * 0.3
  x.lim.adj = 3 + (max(nchar(lab.subgrp, type = "chars")) - 19) * ((4-3)/(26 - 19))
  plot(0, 0,
       type='n',
       xlab = lab.x[[1]], ylab = "",
       ylim = c(0, y.lim.max),
       xlim = c(-x.lim.max - 4,
                -x.lim.max + 0.6),
       xaxt="n", yaxt="n",
       main = title[[1]])

  data.size = dim(dat)[1]
  ss.subgrp.list = vector()
  for (i in 1 : n.subgrp.tol){
    ss.subgrp.list[i] = paste(diag(ss.subgrp)[i], " (", ss.subgrp.T[i], "|", ss.subgrp.C[i], ")",  sep = "")
  }
  ss.subgrp.list[n.subgrp.tol + 1] = paste(data.size, " (", length(which(dat$trt == 1)), "|", length(which(dat$trt == 0)), ")",  sep = "")

  x.lim.adj = 2.8 + (max(nchar(lab.subgrp, type = "chars")) - 19) * ((3.2-2.8)/(26 - 19))
  for (i in 1: (n.subgrp.tol + 1)){
    text(-x.lim.max - 4, line.y.range[i,1], labels = lab.subgrp[i], cex = font.size[3], adj = c(0,1))
    text(-x.lim.max - 2.2, line.y.range[i,1], labels = round(treatment.mean[i],2), cex = font.size[3], adj = c(1,1))
    text(-x.lim.max - 1.55, line.y.range[i,1], labels = round(treatment.lower[i],2), cex = font.size[3], adj = c(1,1))
    text(-x.lim.max - 0.90, line.y.range[i,1], labels = round(treatment.upper[i],2), cex = font.size[3], adj = c(1,1))
    text(-x.lim.max + 0.6, line.y.range[i,1], labels = ss.subgrp.list[i], cex = font.size[3], adj = c(1,1))
  }

  sub.title.xpos = c(-x.lim.max-2.2, -x.lim.max-1.55, -x.lim.max-0.90, -x.lim.max + 0.6)
  sub.title.ypos = rep(line.y.range[n.subgrp.tol + 1, 1] + 0.1, 4)

  sub.title = c("Eff.size", "Low.", "Upp.", "S.Size(T|C)")
  text(sub.title.xpos, sub.title.ypos, labels = sub.title, cex = font.size[3], font=4, adj = c(1,0))

  # forest plot (2)
  par(mar = c(4.1, 0, 2.5, 0))
  line.x.range = est.range[, 2:3]
  line.y.range = matrix(rep(seq(0.1, 0.1+ (n.subgrp.tol) * 0.3, 0.3),2), ncol = 2)


  x.lim.min = round(est.range[dim(est.range)[1],1],2) - 1.5
  x.lim.min = round(x.lim.min/0.5) * 0.5
  x.lim.max = x.lim.min + 3

  y.lim.max = 0.1 + (n.subgrp.tol + 1) * 0.3
  plot(0, 0, type='n',  xlab = lab.x[[2]], ylab = "", ylim = c(0, y.lim.max), xlim = c(x.lim.min, x.lim.max),
       xaxt="n", yaxt="n", cex.lab = font.size[2])
  axis(1, at = seq(x.lim.min, x.lim.max, 0.5), labels = seq(x.lim.min, x.lim.max, 0.5), cex.axis = font.size[4])
  title(main= title[[2]], cex.main = font.size[1])

  line.centre = est.range[, 1]
  w = size.shape[1]; h = size.shape[2]
  h1 = y.lim.max/(x.lim.max - x.lim.min)/h
  w2 = w*h1/3
  for (i in 1: (n.subgrp.tol)){
    lines(line.x.range[i, ], line.y.range[i, ], col = "black")
    x0 = line.centre[i]; y0 = line.y.range[i, 1]
    r =  dim(data.subgrp[[i]])[1] / data.size
    x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
    y = c(y0 - w2* r, y0 - w2* r, y0 + w2* r, y0 + w2 * r)
    polygon(x, y,col = "black", border = "black")
  }

  lines(line.x.range[n.subgrp.tol + 1, ], line.y.range[n.subgrp.tol + 1, ], col = "black")
  x0 = line.centre[n.subgrp.tol + 1]; y0 = line.y.range[n.subgrp.tol + 1, 1]
  x = c(x0 - w, x0 + w, x0 + w, x0 - w)
  y = c(y0 - w2, y0 - w2, y0 + w2, y0 + w2)
  polygon(x, y,col = "black", border = "black")
  abline(v= 0, col="gray", lty=2)
  abline(v= treatment.mean[n.subgrp.tol+1], col="gray", lty=1)


  # forest plot (3)

  par(mar = c(4.1,0,2.5,1))

  x.lim.max = max(max(est.C.range[, 2:3]), max(est.T.range[, 2:3]))
  x.lim.min = min(min(est.C.range[, 2:3]), min(est.T.range[, 2:3]))

  y.lim.max = 0.1 + (n.subgrp.tol + 1) * 0.3
  plot(0, 0, type='n',  xlab = lab.x[[3]], ylab = "", ylim = c(0, y.lim.max), xlim = c(x.lim.min, x.lim.max),
       xaxt="n", yaxt="n", cex.lab = font.size[2])

  if (KM==FALSE){
  axis(1, at = round(seq(x.lim.min, x.lim.max, len = 10 + 1), 2), labels = round(seq(x.lim.min, x.lim.max, len = 10 + 1), 2), cex.axis = font.size[4])
  title(main= title[[3]], cex.main = font.size[1])

  line.C.x.range = est.C.range[, 2:3]
  line.T.x.range = est.T.range[, 2:3]
  line.C.y.range = line.y.range  - 0.03
  line.T.y.range = line.y.range  + 0.03

  line.C.centre = est.C.range[, 1]
  line.T.centre = est.T.range[, 1]

  w = 0.25 * ((x.lim.max - x.lim.min) / 6 ); t = 0.15
  h1 = y.lim.max/(x.lim.max - x.lim.min)/h
  w2 = w*h1/3
  for (i in 1: (n.subgrp.tol)){
    lines(line.C.x.range[i, ], line.C.y.range[i, ], col = col.line[1])
    x0 = line.C.centre[i]; y0 = line.C.y.range[i, 1]
    ss.C = length(which(data.subgrp[[i]]$trt == 0))
    r =  ss.C / data.size
    x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
    y = c(y0 - w2 * r, y0 - w2 * r, y0 + w2 * r, y0 + w2 * r)
    polygon(x, y,col = col.line[1], border = col.line[1])

    lines(line.T.x.range[i, ], line.T.y.range[i, ], col = col.line[2])
    x0 = line.T.centre[i]; y0 = line.T.y.range[i, 1]
    ss.T = length(which(data.subgrp[[i]]$trt == 1))
    r =  ss.T / data.size
    x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
    y = c(y0 - w2 * r, y0 - w2 * r, y0 + w2 * r, y0 + w2 * r)
    polygon(x, y,col = col.line[2], border = col.line[2])
  }
  lines(line.C.x.range[n.subgrp.tol + 1, ], line.C.y.range[n.subgrp.tol + 1, ], col = col.line[1])
  x0 = line.C.centre[n.subgrp.tol + 1]; y0 = line.C.y.range[n.subgrp.tol + 1, 1]
  ss.C = length(which(dat$trt == 0))
  r =  ss.C / data.size
  x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
  y = c(y0 - w2 * r, y0 - w2 * r, y0 + w2 * r, y0 + w2 * r)
  polygon(x, y,col = col.line[1], border = col.line[1])

  lines(line.T.x.range[n.subgrp.tol + 1, ], line.T.y.range[n.subgrp.tol + 1, ], col = col.line[2])
  x0 = line.T.centre[n.subgrp.tol + 1]; y0 = line.T.y.range[n.subgrp.tol + 1, 1]
  ss.T = length(which(dat$trt == 1))
  r =  ss.T / data.size
  x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
  y = c(y0 - w2 * r, y0 - w2 * r, y0 + w2 * r, y0 + w2 * r)
  polygon(x, y,col = col.line[2], border = col.line[2])

  abline(v= 0, col="gray", lty=2)
  }
}
