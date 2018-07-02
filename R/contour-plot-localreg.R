#' Contour plot for effect size via local regression.
#'
#' this function produces a contour plot showing the treatment effect size of subgroups. The subgroups are first defined by certain
#' ranges of the first continuous covariate; and then further divided into smaller subgroup by certain ranges of the second covariate
#' . The subgroups over the first covariate have a sample size close to one pre-specified value (N2) and any neighboring subgroups
#' have an overlap size near the second pre-specified value (N1). Similarly, each subgroup over the first covariate has a sample size
#' near the third pre-specified value (N4), and any neighboring subgroups which are further divided over the second covariate have a
#' sample size near the fourth pre-specified value (N3). The x-coordinate and y-coordinate of a point indicates the middle point of
#' the range over the first covariate and that over the second covariate, respectively. The contours show approximate effect sizes
#' which are obtained by fitting grid points over the polynormial surface interpolating the points corresponding to subgroups.Note
#' that there are three parameters for controlling the setting of contours. In addition, the function uses log odd ratio and log
#' hazard ratio for displaying subgroup effect sizes in binary and survival data, respectively. Also, the actual subgroup sample
#' sizes over the covariates are shown on the console window.
#'
#' @param dat            a data set
#' @param covari.sel     a vector of indices of the two covariates
#' @param trt.sel        a variable index specifying the treatment code
#' @param resp.sel       a variable index specifying the response variable
#' @param outcome.type   a string specifying the type of the response variable,
#'  it can be "continuous", or "binary" or  "survival".
#' @param setup.ss       a vector specifying approximate subgroup sample size
#' and neibourghing subgroup overlap sample size. The first and the second elements
#' are for overlap sizes and subgroup sample sizes over the first covariate;
#' the third and thefourth are for further divided overlap sizes
#' and subgroup sample sizes over the second covariate.
#' @param n.grid         a vector specifying the numbers of the grid points on
#'  the x-axis and the y-axis respectively.
#' @param brk.es         a vector specifying the break points on effect size,
#' where each range partition is given with a different colour on points.
#' @param n.brk.axis   a number specifying the number of breakpoints dividing the axis of the argument "range.strip".
#' @param para.plot      a vector specifying the parameters of the contour plot;
#'  the first value is for controlling the degree of smoothing; the second
#'                  is for controlling the degree of the polynomials fitting to
#'                  be used (normally 1 or 2); the third is for controlling the
#'                  number of contour lines.
#' @param font.size      a vector specifying the size of labels and text; the
#'  first element is for the main title, the second is for for x-axis and y-axis
#'  labels; the third is for the subtitle; the fourth is for the text in the
#'  legend; the fifth is for the labels on contour lines.
#' @param title          a string specifying the main title.
#' @param subtitle       strings specifying the subtitle
#' @param unit.x step for the x variable to create the grid that will center the kernel to apply the local regression
#' @param unit.y step for the x variable to create the grid that will center the kernel to apply the local regression
#' @param effect           either "HR" or "RMST". only when outcome.type = "survival"
#' @param show.overall     logical. whether to show or not the overall treatment effect in the strip
#' @param strip        a string specifying the title of the colour strip.
# created by Yi-Da Chiu, 01/08/17
# revised by Yi-Da Chiu, 18/08/17
#' @export
plot_contour_localreg <- function(dat, covari.sel, trt.sel, resp.sel, outcome.type,
                                  setup.ss, n.grid = c(41, 41),
                                  brk.es = c(0, 1, 2, 3),
                                  n.brk.axis = 3,
                                  para.plot = c(0.35, 2, 20),
                                  font.size = c(1.5,1.2,1,0.85,0.8),
                                  title = NULL, subtitle = NULL,
                                  unit.x = 1, unit.y = 1,
                                  effect = "HR", show.overall = TRUE,
                                  strip = "Effect Size") {
  ################################################ 0. argument validity check  #################################################################

  # if (missing(dat)) stop("Data have not been inputed!")
  # if (!(is.data.frame(dat))) stop("The data set is not with a data frame!")
  #
  # if (missing(covari.sel)) stop("The variables for defining subgroups have not been specified!")
  # if (!(is.numeric(covari.sel))) stop("The variables for defining subgroups are not numeric!")
  # if (length(covari.sel) > 2) stop("This function only considers 2 covariates at most for defining subgroups!")
  #
  # if (missing(trt.sel)) stop("The variable specifying the treatment code (for treatment / control groups) has not been specified!")
  # if (!(length(trt.sel) == 1)) stop("The variable specifying the treatment code can not have more than one component!")
  # if (!(is.factor(dat[, trt.sel]))) stop("The variable specifying the treatment code is not categorical!")
  # if (length(names(table(dat[, trt.sel]))) > 2) stop("The variable specifying the treatment code is not binary!")
  # if (sum(is.element(names(table(dat[, trt.sel])), c("0","1"))) != 2) stop("The treatment code is not 0 or 1!")
  #
  # type.all = c("continuous", "binary",  "survival")
  # if (is.null(outcome.type)) stop("The type of the response variable has not been specified!")
  # if (!(is.element(outcome.type, type.all)) == TRUE) stop("A unrecognized type has been inputed!")
  # if (outcome.type == "continuous"){
  #   stop("Only survival implemented for now")
  #   if (missing(resp.sel)) stop("The response variable has not been specified!")
  #   if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
  #   if (!(is.numeric(dat[, resp.sel]))) stop("The response variable is not numeric!")
  # }else if (outcome.type == "binary"){
  #   stop("Only survival implemented for now")
  #   if (missing(resp.sel)) stop("The response variable has not been specified!")
  #   if (!(length(resp.sel) == 1)) stop("The response variable has more than one component!")
  #   if (!(is.factor(dat[, resp.sel]) || is.numeric(dat[, resp.sel])  )) stop("The response variable is not categorical or numerical!")
  #   if (length(names(table(dat[, resp.sel]))) > 2) stop("The response variable is not binary!")
  #   if (sum(is.element(names(table(dat[, resp.sel])), c("0","1"))) != 2) stop(" The response variable is not coded as 0 and 1!")
  # }else if (outcome.type == "survival"){
  #   if (missing(resp.sel)) stop("The response variablehas not been specified!")
  #   if (!(length(resp.sel) == 2)) stop("The response variable for analysing survival data should have two components!")
  #   if (!(is.numeric(dat[, resp.sel[1]]))) stop("The response variable specifying survival time is not numeric!")
  #   if (!(is.numeric(dat[, resp.sel[2]]) || is.logical(dat[, resp.sel[2]]) ) ) stop("The response variable specifying indicators of right censoring should be numerical or logical!")
  #   if (length(names(table(dat[, resp.sel[2]]))) > 2) stop("The response variable specifying indicators of right censoring is not binary!")
  #   if (sum(is.element(names(table(dat[, resp.sel[2]])), c("0","1"))) != 2) stop("The response variable specifying indicators of right censoring is not coded as 0 and 1!")
  # }
  #
  # if (missing(setup.ss)) stop("The setting for subgroup sample size and overlap have not been specified!")
  # if (!(is.numeric(setup.ss))) stop("The setting for subgroup sample size and overlap are not numeric!")
  # if (length(setup.ss) !=  4) stop("The setting for subgroup smaple size and overlap does not have four elements!")
  # if ((setup.ss[1] > setup.ss[2]) || (setup.ss[3] > setup.ss[4]) || (setup.ss[4] > setup.ss[2])){
  #   stop("subgroup overlap sample sizes is larger than subgroup sample size! Or subgroup sample sizes over the first covariate are not
  #        larger than their further divided subgroup sample sizes over the second covariate!")
  # }
  #
  # if (missing(n.grid)) stop("The vector specifying the numbers of the grid points has not been specified!")
  # if (!(length(n.grid) == 2)) stop("The vector specifying the numbers of the grid points does not have two components only!")
  # if (!(is.numeric(n.grid)) || (sum(n.grid < 2) != 0 )) stop("The vector specifying the numbers of the grid points is not numeric or has
  #                                                            a value less than 2!")
  #
  # if (missing(brk.es)) stop("The vector specifying the numbers of break points for effect sizes has not been specified!")
  # if (length(brk.es) > 5) stop("The vector specifying the numbers of break points for effect sizes should have five components only!")
  # if (!(is.numeric(brk.es))) stop("The vector specifying the numbers of break points for effect sizes is not numeric!")
  #
  # if (missing(para.plot)) stop("The vector specifying the parameters of the contour plot has not been specified!")
  # if (!(length(para.plot) == 3)) stop("The vector specifying the parameters of the contour plot should have 3 components only!")
  # if (!(is.numeric(para.plot)) || (sum(para.plot < 0) != 0 )) stop("The vector specifying the parameters of the contour plot is not numeric or has
  #                                                                  a negative element!")
  # if (!(para.plot[2] %in% c(0, 1, 2)) ) stop("The second plot parameter is given with a unallowable value!")
  # if (!(para.plot[3]%%1==0) || (para.plot[3] < 0)  ) stop("The third plot parameter should be a positive integer!")
  #
  # if (!(is.numeric(font.size))) stop("The argument about the font sizes of the label and text is not numeric!")
  # if (!(length(font.size) == 5)) stop("The length of the font size settings is not 5!!")

  ################################################ 1. create subgroup data  #################################################################
  names(dat)[trt.sel] = "trt"                            # rename the variable for treatment code
  if (outcome.type == "continuous"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "binary"){
    names(dat)[resp.sel] = "resp"                        # rename the response variable
  }else if (outcome.type == "survival"){
    names(dat)[resp.sel[1]] = "time"                     # rename the response variable for survival time
    names(dat)[resp.sel[2]] = "status"                     # rename the response variable for survival right censoring status
  }

  x.lim = range(dat[covari.sel[1]])
  y.lim = range(dat[covari.sel[2]])
  grid.pts.x = seq(x.lim[1], x.lim[2], unit.x)
  grid.pts.y = seq(y.lim[1], y.lim[2], unit.y)
  grid.xy = expand.grid(grid.pts.x, grid.pts.y)
  alpha = 0.8
  # var = 1/(1-alpha)^2
  # sqrt(var)
  n.cutoff = 20

  # Now do a bivariate local regression ####
  results.xy = do.call(rbind, lapply(1:nrow(grid.xy), FUN = function(i){
    center.x = grid.xy[i, 1]
    center.y = grid.xy[i, 2]
    # cat(center.x, center.y)
    var = 1/(1-alpha)^2
    x.conf.up = center.x + 2 * sqrt(var)
    weight.up = exp(-mahalanobis(x = data.frame(x.conf.up, center.y),
                                 center = c(center.x, center.y),
                                 cov    = matrix(c(var,0,0,var), nrow = 2))/2)
    weight. = exp(-mahalanobis(x = data.frame(dat[covari.sel[1]], dat[covari.sel[2]]),
                               center = c(center.x, center.y),
                               cov    = matrix(c(var,0,0,var), nrow = 2))/2)

    if(sum(weight.>weight.up)>n.cutoff){
      sum((weight.>0.01))
      cox.result = survival::coxph(survival::Surv(time, status) ~ trt,
                         data = dat,
                         weights = weight.)
      summary(cox.result)
      data.frame(x = center.x, y = center.y, estimate = cox.result$coefficients)
    }else{
      data.frame(x = center.x, y = center.y, estimate = NA)
    }
  }))

  z.matrix = matrix(results.xy$estimate,
                    nrow = length(grid.pts.x),
                    ncol = length(grid.pts.y), byrow = F)
  rownames(z.matrix) = grid.pts.x
  colnames(z.matrix) = grid.pts.y

  lab.vars = names(dat)[covari.sel]

  col.power = 0.75
  col.vec = rev(colorspace::diverge_hcl(n = length(brk.es)-1,
                                        c = 100, l = c(50,90),
                                        power = col.power))

  ### Create a nice plot -------------------------------------------------------
  if (!(outcome.type == "survival" & effect == "HR")) col.vec = rev(col.vec)
  cols = col.vec
  layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(4,1))
  if (is.null(title)){
    par(mar=c(4,4,2,1))
  } else{
    par(mar=c(4,4,4,1))
  }
  plot(grid.xy[,1], grid.xy[,2], type = "n",
       # yaxs="i", xaxs="i",
       xlim = range(grid.pts.x),
       ylim = range(grid.pts.y),
       xlab = lab.vars[1], ylab = lab.vars[2],
       main = title,
       col  = "gray80",
       cex.main = font.size[1],
       cex.lab  = font.size[2],
       cex.axis = font.size[2],
       cex.sub  = font.size[3])
  breaks = seq(min(brk.es),max(brk.es),      length.out = length(cols)+1)
  breaks.axis = seq(min(brk.es),max(brk.es), length.out = n.brk.axis)
  .filled.contour(grid.pts.x, grid.pts.y, z.matrix,
                  levels = breaks,
                  col = rev(cols))
  points(dat[, covari.sel[1]], dat[, covari.sel[2]], cex = 0.5)
  if (is.null(title)){
    par(mar=c(4,2,2,2.5))
  } else{
    par(mar=c(4,2,4,2.5))
  }
  image.scale(brk.es,
              col= rev(cols),
              breaks = breaks,
              axis.pos = 4, add.axis = FALSE)
  axis(2, at = breaks.axis, labels = round(breaks.axis, 3), las = 0, cex.axis = font.size[5])
  mtext(strip, side=4, line=1, cex.lab = font.size[5])

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
  if(show.overall){
    cat("Overall Treatment effect is:",
        overall.treatment.mean, ", with confidence interval: (",
        overall.treatment.lower,";",overall.treatment.upper,")\n")
    points(x = 0.5,
           (overall.treatment.mean), pch = 20)
    points(x = 0.5, overall.treatment.lower, pch = "-")
    points(x = 0.5, overall.treatment.upper, pch = "-")
    segments(x0 = 0.5, x1 = 0.5,
             y0 = overall.treatment.lower,
             y1 = overall.treatment.upper)
  }

}

