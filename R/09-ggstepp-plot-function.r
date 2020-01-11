#' STEPP for subgroup effect size
#'
#' This function produces a plot of using the approach "Subpopulation Treatment Effect Pattern Plot". It shows the treatment effect
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
#' Contrary to \code{\link{plot_stepp}}, \code{ggplot_stepp} implements the proper x-axis.
#' This function uses the ggplot2 package to draw the actual plot. To control font sizes, use \code{\link[ggplot2]{theme}} with the resulting object.
#'
#' @param dat            a data set
#' @param covari.sel     a vector of indices of the two covariates
#' @param trt.sel        a variate index specifying the treatment code
#' @param resp.sel       a variate index specifying the response variable
#' @param outcome.type   a string specifying the type of the response variable, it can be "continuous", or "binary" or  "survival".
#' @param setup.ss       a vector specifying the approximate overlap size (N2) and subgroup sample size (N1).
#' @param alpha          the type I error rate
#' @param title          a string specifying the main title.
#' @param lab.y          a string specifying the labels of the y-axis.
#' @param subtitle       strings specifying the subtitle
#'
#' @examples
#' # Load the data to be used
#' data(prca)
#' dat = prca
#'
#' ## 9. stepp Plot -----------------------------------------------------------
#' lab.y.title = paste("Treatment effect size (log-hazard ratio)");
#' setup.ss = c(30,40)
#' sub.title = paste0("(Subgroup sample sizes are set to ", setup.ss[2],
#'                    "; overlap sizes are set to ", setup.ss[1], ")" )
#' ggplot_stepp(dat,
#'            covari.sel = 8,
#'            trt.sel = 3,
#'            resp.sel = c(1, 2),
#'            outcome.type = "survival",
#'            setup.ss = c(30,40),
#'            alpha = 0.05,
#'            title = NULL,
#'            lab.y = lab.y.title,
#'            subtitle = sub.title)
#'
#'
#' @seealso \code{\link{plot_stepp}}
#' @export
#' @import ggplot2
ggplot_stepp <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type, setup.ss, alpha,
                         title = NULL, lab.y = NULL, subtitle = NULL)
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
    cutpoint.covar1[[2]][i] = covari1.table[min(upp.bd.covar1.idx, ss.full)]
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

  covari.subgrp.mid = matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  treatment.mean = matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  treatment.upper.idl = matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  treatment.lower.idl = matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  treatment.upper = matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  treatment.lower = matrix(rep(0, n.subgrp.covar1), nrow = n.subgrp.covar1, ncol = 1)
  for (i in 1 : n.subgrp.covar1){
    if ((sum((data.subgrp.covar1[[i]])$trt == "1")) == 0 | (sum((data.subgrp.covar1[[i]])$trt == "0")) == 0 ){
      covari.subgrp.mid[i] = NA
      treatment.mean[i] = NA
      treatment.upper[i] = NA
      treatment.lower[i] = NA
      treatment.upper.idl[i] = NA
      treatment.lower.idl[i] = NA
    }else{

      covari.subgrp.mid[i] = mean(data.subgrp.covar1[[i]][, covari.sel[1]])
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
        model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = data.subgrp.covar1[[i]])
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
      model.int = survival::coxph(survival::Surv(time, status) ~ trt, data = dat)
      model.sum = summary(model.int)
      treatment.mean.overall = model.sum$coef[1, 1]
    }
  }

  cat("The subgroup sample sizes are actually", ss.subgrp.covar1, "\n")

  ################################################ 2. produce a graph  #################################################################

  y = c(treatment.mean,
        rep(treatment.mean.overall, length(treatment.mean)),
        treatment.upper.idl,
        treatment.lower.idl,
        treatment.upper,
        treatment.lower)
  type = c(rep("1", length(treatment.mean)),
           rep("2", length(treatment.mean)),
           rep("3", length(treatment.upper.idl)),
           rep("3", length(treatment.lower.idl)),
           rep("5", length(treatment.upper)),
           rep("5", length(treatment.lower)))
  type2 = c(rep("1", length(treatment.mean)),
           rep("2", length(treatment.mean)),
           rep("3", length(treatment.upper.idl)),
           rep("4", length(treatment.lower.idl)),
           rep("5", length(treatment.upper)),
           rep("6", length(treatment.lower)))
  plotdat = data.frame(x = rep(covari.subgrp.mid, 6),
                       y = y,
                       type = type,
                       type2 = type2)

  labels_legend = c("Subgroup treatment effect",
                    "Overall treatment effect",
                    paste("Boundaries for", (1-alpha) * 100, "% C.I."),
                    paste("Boundaries for", (1-alpha) * 100, "% S.C.I."))

  ggplot(plotdat) +
    geom_hline(yintercept = 0, linetype = 2, color = "gray") +
    geom_point(aes_string(x = "x", y = "y", color = "type", shape = "type")) +
    geom_line(aes_string(x = "x", y = "y", color = "type", linetype = "type", group = "type2")) +
    coord_cartesian(ylim = c(min(treatment.lower, na.rm = TRUE), max(treatment.upper, na.rm = TRUE))) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = title, subtitle = subtitle, x = lab.var, y = lab.y,
         color = "", linetype = "", shape = "") +
    scale_color_manual(values = c("red", "green", "blue", "orange"),
                       labels = labels_legend) +
    scale_linetype_manual(values = c(1, 1, 2, 2),
                          labels = labels_legend) +
    scale_shape_manual(values = c(16, 32, 32, 32),
                       labels = labels_legend)
}
