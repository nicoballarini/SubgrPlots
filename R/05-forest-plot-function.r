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
#' @param dat a data set
#' @param covari.sel a vector of indices of the two covariates
#' @param trt.sel a covariate index specifying the treatment code
#' @param resp.sel a covariate index specifying the response variable
#' @param outcome.type a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#' @param size.shape a vector specifying the height and width of the diamonds displaying sample sizes.
#' @param font.size a vector specifying the size of labels and text; the first element is for the main titles, the second is for
#'               for the x-axis labels; the thrid is for the text in the first sub-figure; the fourth is for the unit labels of
#'               the x-axis.
#' @param title a list of three strings specifying the main titles of the three sub-figures.
#' @param lab.x a list of three strings specifying the x-axis labels of the three sub-figures.
#' @param time time for calculating the RMST
#' @param pdf a logical indicating whether the plot is inside a pdf() device
#' @param KM a logical indicating whether to show the Kaplan-Meier curves in the third panel
#' @param show.km.axis a logical indicating whether to show the axes in the Kaplan-Meier curves
#' @param widths a vector of length 3 indicating the widths of the panels
#' @param max.time a numeric input indicating the maximum time for x-axis in the the Kaplan-Meier curves. If NULL, the maximum is taken from the dataset.
#' @param n.brk number of breaks in the Kaplan-Meier curves
#'
#' @examples
#' # Load the data to be used
#' library(dplyr)
#' data(prca)
#' dat <- prca
#' dat %>%
#'   mutate(bm = factor(ifelse(bm == 0 , "No", "Yes")),
#'          hx = factor(ifelse(hx == 0 , "No", "Yes"))) -> dat
#'
#' ## 5. Forest Plot -----------------------------------------------------------
#' main.title = list("", "Forest plot of subgroups",
#'                   "Kaplan-Meier curves\n by treatment group")
#' label.x = list("", "Log hazard ratio",
#'                "Time (days)")
#' plot_forest(dat,
#'             covari.sel = c(4,5,6,7),#vars
#'             trt.sel = 3,
#'             resp.sel = c(1, 2),
#'             outcome.type = "survival",
#'             size.shape = c(0.3, 6.5/4),
#'             font.size = c(0.6, 0.5, 0.4, 0.6),
#'             title = main.title,
#'             lab.x = label.x, time = 50, KM = TRUE, pdf = TRUE,
#'             show.km.axis = 2, n.brk = 12, max.time = 77,
#'             widths = c(1,1,0.6))
#'
#'
#' @export
#' @import grid
#' @import graphics
plot_forest <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                      size.shape = c(0.25, 0.12), font.size = c(1.3, 1, 0.85, 0.9),
                      title = NULL, lab.x = NULL, time = mean(dat[,resp.sel[1]]),
                      pdf = FALSE, KM = FALSE, show.km.axis = TRUE,
                      widths = c(1,1,1), max.time = NULL, n.brk = 10)
{
  old.par <- par(no.readonly=T)

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

  if (!(length(widths) == 3)) stop("The 'widths' should have three components only!")
  widhts = widths/(sum(widths))
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

        model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.subgrp[[i]])
        model.sum = summary(model.int)
        treatment.mean[i] = model.sum$coef[1, 1]
        treatment.upper[i] = model.sum$coef[1, 1] + 1.96 * model.sum$coef[1, 3]
        treatment.lower[i] = model.sum$coef[1, 1] - 1.96 * model.sum$coef[1, 3]

        surv.fit = survival::survfit(survival::Surv(time, status) ~ trt, data = data.subgrp[[i]])
        difference <- summary(surv.fit, time=time)

        plot.data[[i]] = surv.fit
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
      plot.data[[n.subgrp.tol + 1]] = surv.fit
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
      # lab.subgrp[k] = paste0("(", LETTERS[i], j, ") ", lab.vars[i], "=", cats.var[[i]][j], sep = "")
      lab.subgrp[k] = paste0(lab.vars[i], " = ", cats.var[[i]][j], sep = "")
    }
  }
  lab.subgrp[n.subgrp.tol + 1] = "Full"

  est.range = cbind.data.frame(treatment.mean, treatment.lower, treatment.upper )
  est.C.range = cbind.data.frame(treatment.C.mean, treatment.C.lower, treatment.C.upper )
  est.T.range = cbind.data.frame(treatment.T.mean, treatment.T.lower, treatment.T.upper )
  dimnames(est.range) = list(c(lab.subgrp), c("mean", "lower","upper") )

  ################################################ 2. create plots #################################################################

  if (pdf) plot.new() # Needed for par(new=TRUE) when outputting pdfs
  grid.newpage()
  col.line = c("blue", "red", "forestgreen", "orange", "darkorchid1", "darkgoldenrod3", "darkseagreen3", "chartreuse3", "cyan1", "deeppink1")

  col.line = c("#a6cee3", "#1f78b4")
  data.size = dim(dat)[1]

  ## 2.1 First panel: Table -----
  vp <- viewport(x = 0.5, width=0.99, height=1)
  pushViewport(vp)
  vp <- viewport(x = 0, y = 0.10, width=widhts[1], height=0.83, just = c("left", "bottom"))
  pushViewport(vp)

  ss.subgrp.list = vector()
  for (i in 1 : n.subgrp.tol){
    ss.subgrp.list[i] = paste(diag(ss.subgrp)[i], " (", ss.subgrp.T[i], "|", ss.subgrp.C[i], ")",  sep = "")
  }
  ss.subgrp.list[n.subgrp.tol + 1] = paste(data.size, " (", length(which(dat$trt == 1)), "|", length(which(dat$trt == 0)), ")",  sep = "")

  # grid.rect()
  vertical_width = 1/(n.subgrp.tol + 1.5)
  sub.title = c("Eff.size", "Low.", "Upp.", "S.Size(T|C)")
  grid.text(x = c(0.4,0.55,0.7,0.98), y = 1-(vertical_width/2)/2,
            label = sub.title, gp = gpar(cex = font.size[3], font=4, adj = c(1,0)), hjust = 1)
  data.size = dim(dat)[1]
  for (i in 1:(n.subgrp.tol)){
    y_p = 1 - (vertical_width/2 + vertical_width*(i) + vertical_width/2)
    grid.text(x=0.05, y=y_p, label = lab.subgrp[i],               gp = gpar(cex = font.size[3], adj = c(0,1)), hjust = 0)
    grid.text(x=0.40, y=y_p, label = round(treatment.mean[i],2),  gp = gpar(cex = font.size[3], adj = c(1,1)), hjust = 1)
    grid.text(x=0.55, y=y_p, label = round(treatment.lower[i],2), gp = gpar(cex = font.size[3], adj = c(1,1)), hjust = 1)
    grid.text(x=0.70, y=y_p, label = round(treatment.upper[i],2), gp = gpar(cex = font.size[3], adj = c(1,1)), hjust = 1)
    grid.text(x=0.98, y=y_p, label = ss.subgrp.list[i],           gp = gpar(cex = font.size[3], adj = c(1,1)), hjust = 1)
  }
  y_p = 1 - vertical_width
  grid.text(x=0.05, y=y_p, label = lab.subgrp[n.subgrp.tol+1],               gp = gpar(cex = font.size[3], adj = c(0,1)), hjust = 0)
  grid.text(x=0.40, y=y_p, label = round(treatment.mean[n.subgrp.tol+1],2),  gp = gpar(cex = font.size[3], adj = c(1,1)), hjust = 1)
  grid.text(x=0.55, y=y_p, label = round(treatment.lower[n.subgrp.tol+1],2), gp = gpar(cex = font.size[3], adj = c(1,1)), hjust = 1)
  grid.text(x=0.70, y=y_p, label = round(treatment.upper[n.subgrp.tol+1],2), gp = gpar(cex = font.size[3], adj = c(1,1)), hjust = 1)
  grid.text(x=0.98, y=y_p, label = ss.subgrp.list[n.subgrp.tol+1],           gp = gpar(cex = font.size[3], adj = c(1,1)), hjust = 1)

  upViewport()

  ## 2.2 Second panel: Forest plot -----
  vp <- viewport(x = widhts[1], y = 0.83+0.10, width=widhts[2], height=0.07, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(title[[2]], gp = gpar(cex = font.size[1]))
  upViewport()
  vp <- viewport(x = widhts[1], y = 0, width=widhts[2], height=0.04, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(lab.x[[2]], gp = gpar(cex = font.size[2]))
  upViewport()


  vp <- viewport(x = widhts[1], y = 0.10, width=widhts[2], height=0.83, just = c("left", "bottom"))
  pushViewport(vp)
  vp <- viewport(x = 0.05, y = 0, width=0.9, height=1, just = c("left", "bottom"))
  pushViewport(vp)
  line.centre  = est.range[, 1]
  line.x.range = est.range[, 2:3]
  i = 1:(n.subgrp.tol + 1)
  y_p_vec = 1 - (vertical_width/2 + vertical_width*(i) + vertical_width/2)
  line.y.range = matrix(rep(y_p_vec,2), ncol = 2)

  x.lim.min = round(est.range[dim(est.range)[1],1],2) - 1.5
  x.lim.min = round(x.lim.min/0.5) * 0.5
  x.lim.max = x.lim.min + 3
  x.lim.diff = x.lim.max-x.lim.min

  grid.xaxis(at = seq(0,1, len = 9),
             label = round(seq(x.lim.min, x.lim.max, len =9), 2),
             gp = gpar(cex = font.size[3]),
             edits = gEdit(gPath="labels", rot=0))

  w = size.shape[1]
  h = size.shape[2]
  ww = range((line.x.range[1, ]-x.lim.min)/x.lim.diff)
  w = (ww[2]-ww[1])/2*w
  w2 = 0.33/0.83*w*h
  i=1
  for (i in 1:(n.subgrp.tol)){
    grid.lines(x = (line.x.range[i, ]-x.lim.min)/x.lim.diff,
               y = line.y.range[i, ])
    x0 = (line.centre[i]-x.lim.min)/x.lim.diff
    y0 = line.y.range[i, 1]
    r =  dim(data.subgrp[[i]])[1] / data.size
    x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
    y = c(y0 - w2* r, y0 - w2* r, y0 + w2* r, y0 + w2 * r)
    grid.polygon(x,y, gp = gpar(fill="black"))
  }

  grid.lines((line.x.range[n.subgrp.tol + 1, ]-x.lim.min)/x.lim.diff,
             1 - (vertical_width/2 +  vertical_width/2),
             gp = gpar(col = "black"))
  x0 = (line.centre[n.subgrp.tol + 1]-x.lim.min)/x.lim.diff
  y0 = 1 - (vertical_width/2 +  vertical_width/2)
  x = c(x0 - w, x0 + w, x0 + w, x0 - w)
  y = c(y0 - w2, y0 - w2, y0 + w2, y0 + w2)
  grid.polygon(x, y, gp = gpar(fill = "black"))
  grid.lines(x = (0-x.lim.min)/x.lim.diff, y=c(0,1),gp = gpar(col="gray", lty=2))
  grid.lines(x = (treatment.mean[n.subgrp.tol+1]-x.lim.min)/x.lim.diff, y=c(0,1),gp = gpar(col="gray", lty=1))
  upViewport()
  upViewport()


  ## 2.3 Third panel: Forest plot -----
  vp <- viewport(x = widhts[1]+widhts[2], y = 0.83+0.10, width=widhts[3], height=0.07, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(title[[3]], gp = gpar(cex = font.size[1]))
  upViewport()
  vp <- viewport(x =  widhts[1]+widhts[2], y = 0, width=widhts[3], height=0.04, just = c("left", "bottom"))
  pushViewport(vp)
  grid.text(lab.x[[3]], gp = gpar(cex = font.size[2]))
  upViewport()


  if (KM==FALSE){
    vp <- viewport(x =  widhts[1]+widhts[2], y = 0.10, width=widhts[3], height=0.83, just = c("left", "bottom"))
    pushViewport(vp)
    # grid.rect()
    vp <- viewport(x = 0.3, y = 1-vertical_width/2, width=0.4, height=vertical_width/2, just = c("left", "bottom"))
    pushViewport(vp)
    grid.legend(labels = c("Treatment", "Control"), nrow = 1, do.lines = TRUE,
                gp=gpar(col = c("blue", "red"), cex = font.size[2]*0.6))
    upViewport()

    vp <- viewport(x = 0.05, y = 0, width=0.9, height=1, just = c("left", "bottom"))
    pushViewport(vp)
    x.lim.max = max(max(est.C.range[, 2:3]), max(est.T.range[, 2:3]))
    x.lim.min = min(min(est.C.range[, 2:3]), min(est.T.range[, 2:3]))
    x.lim.diff = x.lim.max-x.lim.min

    grid.xaxis(at = seq(0,1, len = 9),
               label = round(seq(x.lim.min, x.lim.max, len =9), 2),
               gp = gpar(cex = font.size[3]),
               edits = gEdit(gPath="labels", rot=0))

    i = 1:(n.subgrp.tol + 1)
    y_p_vec = 1 - (vertical_width/2 + vertical_width*(i) + vertical_width/2)
    line.y.range = matrix(rep(y_p_vec,2), ncol = 2)

    line.C.x.range = est.C.range[, 2:3]/x.lim.diff
    line.T.x.range = est.T.range[, 2:3]/x.lim.diff
    line.C.y.range = line.y.range  - 0.01
    line.T.y.range = line.y.range  + 0.01

    line.C.centre = est.C.range[, 1]/x.lim.diff
    line.T.centre = est.T.range[, 1]/x.lim.diff

    for (i in 1: (n.subgrp.tol)){
      grid.lines(line.C.x.range[i, ], line.C.y.range[i, ], gp = gpar(col = col.line[1]))
      x0 = line.C.centre[i]; y0 = line.C.y.range[i, 1]
      ss.C = length(which(data.subgrp[[i]]$trt == 0))
      r =  ss.C / data.size
      x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
      y = c(y0 - w2 * r, y0 - w2 * r, y0 + w2 * r, y0 + w2 * r)
      grid.polygon(x, y, gp = gpar(fill = col.line[1], col = col.line[1]))

      grid.lines(line.T.x.range[i, ], line.T.y.range[i, ], gp = gpar(col = col.line[2]))
      x0 = line.T.centre[i]; y0 = line.T.y.range[i, 1]
      ss.T = length(which(data.subgrp[[i]]$trt == 1))
      r =  ss.T / data.size
      x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
      y = c(y0 - w2 * r, y0 - w2 * r, y0 + w2 * r, y0 + w2 * r)
      grid.polygon(x, y, gp = gpar(col = col.line[2], fill = col.line[2]))
    }
    grid.lines(line.C.x.range[n.subgrp.tol + 1, ],
               1 - (vertical_width/2 +  vertical_width/2)-0.01,
               gp = gpar(col = col.line[1]))
    x0 = line.C.centre[n.subgrp.tol + 1]; y0 = 1 - (vertical_width/2 +  vertical_width/2)-0.01
    ss.C = length(which(dat$trt == 0))
    r =  ss.C / data.size
    x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
    y = c(y0 - w2 * r, y0 - w2 * r, y0 + w2 * r, y0 + w2 * r)
    grid.polygon(x, y, gp = gpar(col = col.line[1], fill = col.line[1]))

    grid.lines(line.T.x.range[n.subgrp.tol + 1, ],
               1 - (vertical_width/2 +  vertical_width/2)+0.01,
               gp = gpar(col = col.line[2]))
    x0 = line.T.centre[n.subgrp.tol + 1]; y0 = 1 - (vertical_width/2 +  vertical_width/2)+0.01
    ss.T = length(which(dat$trt == 1))
    r =  ss.T / data.size
    x = c(x0 - w * r, x0 + w * r, x0 + w * r, x0 - w * r)
    y = c(y0 - w2 * r, y0 - w2 * r, y0 + w2 * r, y0 + w2 * r)
    grid.polygon(x, y, gp = gpar(col = col.line[2], fill = col.line[2]))
    if(x.lim.min < 0 & 0 < x.lim.max){
      grid.lines(x = 0, y=c(0,1),gp = gpar(col="gray", lty=2))
    }
  upViewport()
  upViewport()
  }
  ### Kaplan-Meier curves
  if (KM==TRUE){
    vp <- viewport(x = widhts[1]+widhts[2], y = 0.10, width=widhts[3], height=0.83, just = c("left", "bottom"))
    pushViewport(vp)

    vp <- viewport(x = 0.3, y = 1-vertical_width/2, width=0.4, height=vertical_width/2, just = c("left", "bottom"))
    pushViewport(vp)
    grid.legend(labels = c("Treatment", "Control"), nrow = 1, do.lines = TRUE,
                hgap = unit(0.5, "lines"),
                gp=gpar(col = rev(col.line), cex = font.size[2]))
    upViewport()

    vp <- viewport(x = 0.05, y = 0, width=0.9, height=1, just = c("left", "bottom"))
    pushViewport(vp)
    if (is.null(max.time)) max.time = round(max(dat$time), 0)
    for (i in 1:n.subgrp.tol){
      vp <- viewport(x = 0, y = 1 - vertical_width/2 - vertical_width*(i+1), width=1, height = vertical_width, just = c("left", "bottom"))
      pushViewport(vp)
      vp <- viewport(x = 0, y = 0.1, width=1, height = 0.8, just = c("left", "bottom"))
      pushViewport(vp)
      par(plt = gridBase::gridPLT(), mgp = c(0,0,0), new = TRUE)
      plot(plot.data[[i]], col = col.line, cex = 0, yaxs="i",
           xaxt = "n", yaxt = "n", bty="n", xlim = c(0, max.time))
      if (show.km.axis==1){
      axis(side=1, at = c(0,max.time), labels = NA, tcl=-0.1, lwd = 0.2, las = 1, cex.axis = font.size[2]/2, mgp=c(0,-0.25,0))
      axis(side=2, at = c(0,1),                     tcl=-0.1, lwd = 0.2, las = 1, cex.axis = font.size[2]/2, mgp=c(0,0.125,0))

      }

      if (show.km.axis == 2){
        axis(side=1, at = c(0,max.time), labels = NA, col="gray",tcl=-0.1, lwd = 0.1, las = 1,  cex.axis = font.size[2], mgp=c(0,-0.25,0))
        axis(side=2, at = c(0,1),        labels = NA, col="gray",tcl=-0.1, lwd = 0.1, las = 1, cex.axis = font.size[2]/2, mgp=c(0,0.125,0))
      }
      upViewport(2)
    }
    vp <- viewport(x = 0, y = 1 - vertical_width/2 - vertical_width, width=1, height = vertical_width, just = c("left", "bottom"))
    pushViewport(vp)
    vp <- viewport(x = 0, y = 0.1, width=1, height = 0.8, just = c("left", "bottom"))
    pushViewport(vp)
    par(plt = gridBase::gridPLT(), mgp = c(0,0,0), new = TRUE)
    plot(plot.data[[n.subgrp.tol+1]], col = col.line, yaxs="i",
         cex=0, xaxt = "n", yaxt = "n", bty="n", xlim = c(0, max.time))
    if (show.km.axis == 1){
      axis(side=1, at = c(0,max.time), labels = NA, tcl=-0.1, lwd = 0.2, las = 1, cex.axis = font.size[2]/2, mgp=c(0,-0.25,0))
      axis(side=2, at = c(0,1),                     tcl=-0.1, lwd = 0.2, las = 1, cex.axis = font.size[2]/2, mgp=c(0,0.125,0))
    }
    if (show.km.axis == 2){
      axis(side=1, at = c(0,max.time), labels = NA, col="gray",tcl=-0.1, lwd = 0.1, las = 1,  cex.axis = font.size[2], mgp=c(0,-0.25,0))
      axis(side=2, at = c(0,1),        labels = NA, col="gray",tcl=-0.1, lwd = 0.1, las = 1, cex.axis = font.size[2]/2, mgp=c(0,0.125,0))
    }
    upViewport(2)

    grid.xaxis(at = seq(0, 1, len = n.brk),
               label = seq(0, max.time, len = n.brk),
               gp = gpar(cex = font.size[3]),
               edits = gEdit(gPath="labels", rot=0))
    upViewport(3)
  }
  par(old.par)
}
